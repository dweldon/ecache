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

-module(ecache_util).
-export([now_seconds/0]).

%% @spec now_seconds() -> integer()
%% @doc Returns the total number of seconds elapsed in Unix time.
-spec now_seconds() -> pos_integer().
now_seconds() ->
    {MegaSeconds, Seconds, _} = now(),
    1000000 * MegaSeconds + Seconds.
