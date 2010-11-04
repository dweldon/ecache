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
%%
%% @doc The purge server's job is to keep track of potentially expired keys, and
%% to periodically ask ecache to delete them. The server state is a priority
%% queue of `{ExpirationTime, Key}' pairs. Every few seconds, the server will
%% look for expired keys in the queue. For each expired key, an asynchronous
%% message is sent to the ecache server to examine that key. If ecache finds
%% that the key is indeed expired, it will delete it from its internal store.
%% Note that a key may be updated on the ecache server after being placed in the
%% queue - in this case there will be multiple entries in the queue for the same
%% key.

-module(ecache_purge_server).
-behaviour(gen_server).
-export([start_link/0,
         stop/0]).
-export([store/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-define(PURGE_PAUSE_TIME, 5000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% @spec store(term(), integer()) -> ok
%% @doc Inserts `Key' into the priority queue. `ExpTime' should be a time in the
%% future when the server will attempt to purge `Key' from the cache. e.g. if we
%% wanted to purge the key `"car"' from the cache in 20 seconds, we would call:
%% `ecache_purge_server:store("car", ecache_util:now_seconds() + 20)'
%% @see ecache_util:now_seconds().
-spec store(term(), pos_integer()) -> ok.
store(Key, ExpTime) ->
    gen_server:cast(?MODULE, {store, Key, ExpTime}).

%% @hidden
init([]) ->
    register(purge_reminder, spawn_link(fun purge_reminder/0)),
    {ok, gb_trees:empty()}.

%% @hidden
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(purge, Tree) ->
    NewTree = purge(Tree),
    {noreply, NewTree};
handle_cast({store, Key, ExpTime}, Tree) ->
    %% include now() in the key to ensure uniqueness
    NewTree = gb_trees:insert({ExpTime, now()}, Key, Tree),
    {noreply, NewTree};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    purge_reminder ! stop,
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @spec purge(gb_tree()) -> gb_tree()
%% @doc Finds exired keys in the priority queue and asks ecache purge them.
%% Returns a new tree with the expired Keys removed.
-spec purge(gb_tree()) -> gb_tree().
purge(Tree) ->
    case gb_trees:size(Tree) of
        0 -> Tree;
        _ ->
            {{ExpTime, _}, Key} = gb_trees:smallest(Tree),
            case ExpTime < ecache_util:now_seconds() of
                true ->
                    {_, Key, NewTree} = gb_trees:take_smallest(Tree),
                    gen_server:cast(ecache, {purge, Key}),
                    purge(NewTree);
                false ->
                    Tree
            end
    end.

%% @spec purge_reminder() -> none()
%% @doc A simple loop which wakes up every PURGE_PAUSE_TIME milliseconds, and
%% reminds the server to purge any expired keys.
-spec purge_reminder() -> no_return().
purge_reminder() ->
    receive
        stop -> ok
    after ?PURGE_PAUSE_TIME ->
        gen_server:cast(?MODULE, purge),
        purge_reminder()
    end.
