-module(ecache_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Ecache = {ecache, {ecache, start_link, []},
              permanent, 5000, worker, [ecache]},
    Purge = {ecache_purge_server, {ecache_purge_server, start_link, []},
             permanent, 5000, worker, [ecache_purge_server]},
    {ok, {{one_for_all, 10, 30}, [Purge, Ecache]}}.
