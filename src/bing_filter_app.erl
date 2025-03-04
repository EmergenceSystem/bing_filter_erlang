-module(bing_filter_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_started(inets),
    application:ensure_started(ssl),
    
    {ok, Port} = em_filter:find_port(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/query", query_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("Filter registrer: http://localhost:~p/query~n", [Port]),
    em_filter:register_filter(io_lib:format("http://localhost:~p/query", [Port])),
    bing_filter_sup:start_link().

stop(_State) ->
    ok.
