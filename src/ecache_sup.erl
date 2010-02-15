%% Copyright (c) 2010 David Weldon
%% This file is part of ecache.
%% ecache is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
