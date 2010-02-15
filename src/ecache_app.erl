-module(ecache_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:start(?MODULE).

start(_Type, _StartArgs) ->
    ecache_sup:start_link().

stop(_State) ->
    ok.
