-module(ecache_util).
-export([now_seconds/0]).

now_seconds() ->
    {MegaSeconds, Seconds, _} = now(),
    1000000 * MegaSeconds + Seconds.
