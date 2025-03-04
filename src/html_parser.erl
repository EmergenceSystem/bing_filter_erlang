-module(html_parser).
-export([parse_string/1, extract_elements/2, get_text/1, extract_attribute/2]).

parse_string(Html) when is_binary(Html) ->
    try
        CleanHtml = clean_html(Html),
        {ok, CleanHtml}
    catch
        _:Reason ->
            io:format("HTML cleaning failed: ~p~n", [Reason]),
            {error, cleaning_failed}
    end;
parse_string(Html) when is_list(Html) ->
    parse_string(list_to_binary(Html)).

clean_html(Html) ->
    % Simple cleaning: remove script tags and their content
    re:replace(Html, "<script[^>]*>.*?</script>", "", [global, dotall, {return, binary}]).

extract_elements(Html, Selector) ->
    case Selector of
        "li.b_algo" ->
            re:run(Html, "<li[^>]*class=['\"]b_algo['\"][^>]*>(.*?)</li>", [global, dotall, {capture, all_but_first, binary}]);
        "div a" ->
            re:run(Html, "<a[^>]*>(.*?)</a>", [global, dotall, {capture, all, binary}]);
        "div p" ->
            re:run(Html, "<p[^>]*>(.*?)</p>", [global, dotall, {capture, all, binary}]);
        ".algoSlug_icon" ->
            re:run(Html, "class=['\"]algoSlug_icon['\"][^>]*>(.*?)<", [global, dotall, {capture, all, binary}]);
        ".news_dt" ->
            re:run(Html, "class=['\"]news_dt['\"][^>]*>(.*?)<", [global, dotall, {capture, all, binary}]);
        _ ->
            {match, []}
    end.

get_text(Element) ->
    % Simple text extraction: remove all HTML tags
    re:replace(Element, "<[^>]*>", "", [global, {return, binary}]).

extract_attribute(Element, Attribute) ->
    case re:run(Element, Attribute ++ "=['\"]([^'\"]*)['\"]", [{capture, all_but_first, binary}]) of
        {match, [Value]} -> {ok, Value};
        _ -> error
    end.

