-module(query_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

-define(SEARCH_URL, "https://www.bing.com/search?q=").
-define(EXCLUDED_CONTENT, ["bing.com", "microsoft.com", "ignalez", "bingj.com"]).

init(Req0, State) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    io:format("Received body: ~p~n", [Body]),
    EmbryoList = generate_embryo_list(Body),
    Response = #{embryo_list => EmbryoList},
    EncodedResponse = jsone:encode(Response),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        EncodedResponse,
        Req
    ),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

generate_embryo_list(JsonBinary) ->
    case jsone:decode(JsonBinary, [{keys, atom}]) of
        Search when is_map(Search) ->
            Value = binary_to_list(maps:get(value, Search, "")),
            Timeout = list_to_integer(binary_to_list(maps:get(timeout, Search, "10"))),
            EncodedSearch = uri_string:quote(Value, unicode),
            SearchUrl = lists:concat([?SEARCH_URL, EncodedSearch]),
            io:format("Search URL: ~s~n", [SearchUrl]),
            case httpc:request(get, {SearchUrl, []}, [{timeout, Timeout * 1000}], [{body_format, binary}]) of
                {ok, {{_, 200, _}, _, Body}} ->
                    io:format("Received response from Bing. Body length: ~p~n", [byte_size(Body)]),
                    extract_links_from_results(Body, Timeout);
                {error, Reason} ->
                    io:format("Error fetching search results: ~p~n", [Reason]),
                    []
            end;
        {error, Reason} ->
            io:format("Error decoding JSON: ~p~n", [Reason]),
            []
    end.

extract_links_from_results(Html, TimeoutSecs) ->
    case html_parser:parse_string(Html) of
        {ok, ParsedHtml} ->
            StartTime = erlang:system_time(millisecond),
            Timeout = TimeoutSecs * 1000,
            case html_parser:extract_elements(ParsedHtml, "li.b_algo") of
                {match, ResultItems} ->
                    io:format("Found ~p result items~n", [length(ResultItems)]),
                    extract_links(ResultItems, StartTime, Timeout, []);
                _ ->
                    io:format("No result items found~n"),
                    []
            end;
        {error, _} ->
            io:format("Failed to parse HTML~n"),
            []
    end.

extract_links([], _StartTime, _Timeout, Acc) ->
    lists:reverse(Acc);
extract_links([Element | Rest], StartTime, Timeout, Acc) ->
    CurrentTime = erlang:system_time(millisecond),
    case CurrentTime - StartTime >= Timeout of
        true -> 
            lists:reverse(Acc);
        false ->
            case process_result_item(Element) of
                {ok, Embryo} -> 
                    extract_links(Rest, StartTime, Timeout, [Embryo | Acc]);
                skip -> 
                    extract_links(Rest, StartTime, Timeout, Acc)
            end
    end.

process_result_item(Element) ->
    case html_parser:extract_elements(Element, "div a") of
        {match, [[FullLink | _] | _]} ->
            case html_parser:extract_attribute(FullLink, "href") of
                {ok, Link} ->
                    io:format("Extracted link: ~p~n", [Link]),
                    case should_skip_link(Link) of
                        true ->
                            io:format("Link should be skipped~n"),
                            skip;
                        false ->
                            case html_parser:extract_elements(Element, "div p") of
                                {match, [[DescElement | _] | _]} ->
                                    DescText = html_parser:get_text(DescElement),

                                    IconText = case html_parser:extract_elements(Element, ".algoSlug_icon") of
                                        {match, [[IconElement | _] | _]} ->
                                            html_parser:get_text(IconElement);
                                        _ ->
                                            <<>>
                                    end,

                                    DtText = case html_parser:extract_elements(Element, ".news_dt") of
                                        {match, [[DtElement | _] | _]} ->
                                            html_parser:get_text(DtElement);
                                        _ ->
                                            <<>>
                                    end,

                                    Resume = clean_text(DescText, IconText, DtText),

                                    Embryo = #{
                                        properties => #{
                                            <<"url">> => Link,
                                            <<"resume">> => Resume
                                        }
                                    },
                                    {ok, Embryo};
                                _ ->
                                    io:format("No description found, skipping~n"),
                                    skip
                            end
                    end;
                _ ->
                    io:format("No href attribute found, skipping~n"),
                    skip
            end;
        _ ->
            io:format("No link element found, skipping~n"),
            skip
    end.

should_skip_link(Link) ->
    IsExcluded = lists:any(fun(Excluded) ->
        binary:match(Link, list_to_binary(Excluded)) =/= nomatch
    end, ?EXCLUDED_CONTENT),
    IsExcluded orelse binary:match(Link, <<"http">>) =/= {0,4}.

clean_text(DescText0, IconText0, DtText0) ->
    DescText = ensure_binary(DescText0),
    IconText = ensure_binary(IconText0),
    DtText = ensure_binary(DtText0),
    Text1 = safe_binary_replace(DescText, IconText, <<>>),
    Text2 = safe_binary_replace(Text1, DtText, <<>>),
    Text3 = safe_binary_replace(Text2, <<" . ">>, <<>>),
    decode_html_entities(Text3).

ensure_binary(Text) when is_binary(Text) -> Text;
ensure_binary(_) -> <<>>.

safe_binary_replace(Subject, Pattern, Replacement) ->
    try
        case byte_size(Pattern) of
            0 -> Subject;
            _ -> binary:replace(Subject, Pattern, Replacement, [global])
        end
    catch
        _:_ -> Subject
    end.

decode_html_entities(Text) ->
    Text1 = decode_numeric_entities(Text),
    Text2 = decode_hex_entities(Text1),
    decode_named_entities(Text2).

decode_numeric_entities(Text) ->
    {ok, Pattern} = re:compile(<<"&#([0-9]+);">>),
    case re:run(Text, Pattern, [{capture, all, binary}, global]) of
        {match, Matches} ->
            lists:foldl(fun([Full, NumBin], Acc) ->
                try
                    Num = binary_to_integer(NumBin),
                    Char = unicode:characters_to_binary([Num], unicode, utf8),
                    safe_binary_replace(Acc, Full, Char)
                catch
                    _:_ -> Acc
                end
            end, Text, Matches);
        nomatch ->
            Text
    end.

decode_hex_entities(Text) ->
    {ok, Pattern} = re:compile(<<"&#x([0-9A-Fa-f]+);">>),
    case re:run(Text, Pattern, [{capture, all, binary}, global]) of
        {match, Matches} ->
            lists:foldl(fun([Full, HexBin], Acc) ->
                try
                    Num = binary_to_integer(HexBin, 16),
                    Char = unicode:characters_to_binary([Num], unicode, utf8),
                    safe_binary_replace(Acc, Full, Char)
                catch
                    _:_ -> Acc
                end
            end, Text, Matches);
        nomatch ->
            Text
    end.

decode_named_entities(Text) ->
    try
        mochiweb_html:decode_entities(Text)
    catch
        _:_ ->
            {ok, Pattern} = re:compile(<<"&([a-zA-Z]+);">>),
            case re:run(Text, Pattern, [{capture, all, binary}, global]) of
                {match, Matches} ->
                    lists:foldl(fun([Full, Name], Acc) ->
                        Entity = resolve_named_entity(Name),
                        case Entity of
                            undefined -> Acc;
                            _ -> safe_binary_replace(Acc, Full, Entity)
                        end
                    end, Text, Matches);
                nomatch ->
                    Text
            end
    end.

resolve_named_entity(<<"nbsp">>) -> <<" ">>;
resolve_named_entity(<<"amp">>) -> <<"&">>;
resolve_named_entity(<<"lt">>) -> <<"<">>;
resolve_named_entity(<<"gt">>) -> <<">">>;
resolve_named_entity(<<"quot">>) -> <<"\"">>;
resolve_named_entity(<<"apos">>) -> <<"'">>;
resolve_named_entity(<<"eacute">>) -> <<"é">>;
resolve_named_entity(<<"egrave">>) -> <<"è">>;
resolve_named_entity(<<"agrave">>) -> <<"à">>;
resolve_named_entity(<<"ccedil">>) -> <<"ç">>;
resolve_named_entity(_) -> undefined.

