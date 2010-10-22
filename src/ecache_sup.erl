%% @author David Weldon
%% @copyright 2010 David Weldon
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

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
