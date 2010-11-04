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
%% @doc ecache is a simple, fast, in-memory hash table for storing arbitrary
%% erlang terms on a local node. It has a memcached-like interface, complete
%% with expiring keys.
%%
%% ecache stores its data in a private ets table. All reads
%% and writes are serialized by the server. All data are stored as
%% `{Key, Value, ExpirationTime}'. During a read, the server will guarantee that
%% if a key has expired, it will delete the key and return `{error,not_found}'.
%% During a store, if a timeout is set, an asynchronous message is sent to the
%% purge server to track (and later purge) the given key.

-module(ecache).
-behaviour(gen_server).
-export([start_link/0,
         stop/0]).
-export([count/0,
         decrement/1,
         delete/1,
         flush/0,
         increment/1,
         load/1,
         load_all/0,
         store/2,
         store/3]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% @spec count() -> integer()
%% @doc Returns the number of keys in the cache.
-spec count() -> non_neg_integer().
count() ->
    gen_server:call(?MODULE, count).

%% @spec decrement(any()) -> {ok, NewValue::integer()} | {error, not_found} |
%%                           {error, not_an_integer}
%% @doc Decrement the value associated with `Key' by 1. Returns `{ok, NewValue}'
%% if the decrement succeeded. Returns `{error, not_found}' if the Key could not
%% be found in the cache. Returns `{error, not_an_integer}' if the original
%% value was not an integer.
-spec decrement(any()) -> {ok, integer()} | {error, not_found} |
                          {error, not_an_integer}.
decrement(Key) ->
    Fun = fun(X) -> X - 1 end,
    gen_server:call(?MODULE, {increment_or_decrement, Key, Fun}).

%% @spec delete(any()) -> ok
%% @doc Deletes `Key' from the cache.
-spec delete(any()) -> ok.
delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

%% @spec flush() -> ok
%% @doc Deletes all keys from the cache.
-spec flush() -> ok.
flush() ->
    gen_server:cast(?MODULE, flush).

%% @spec increment(any()) -> {ok, NewValue::integer()} | {error, not_found} |
%%                           {error, not_an_integer}
%% @doc Increment the value associated with `Key' by 1. Returns `{ok, NewValue}'
%% if the increment succeeded. Returns `{error, not_found}' if the Key could not
%% be found in the cache. Returns `{error, not_an_integer}' if the original
%% value was not an integer.
-spec increment(any()) -> {ok, integer()} | {error, not_found} |
                          {error, not_an_integer}.
increment(Key) ->
    Fun = fun(X) -> X + 1 end,
    gen_server:call(?MODULE, {increment_or_decrement, Key, Fun}).

%% @spec load(any()) -> {ok, Value::any()} | {error, not_found}
%% @doc Returns `{ok, Value}' if the value associated with `Key' could be found,
%% and `{error, not_found}' otherwise.
-spec load(any()) -> {ok, any()} | {error, not_found}.
load(Key) ->
    gen_server:call(?MODULE, {load, Key}).

%% @spec load_all() -> {ok, [{Key::any(), Value::any()}]}
%% @doc Returns a list of all `{Key, Value}' pairs in the cache.
-spec load_all() -> {ok, [{any(), any()}]}.
load_all() ->
    gen_server:call(?MODULE, load_all).

%% @spec store(any(), any()) -> ok
%% @doc Stores a `Key' - `Value' pair in the cache. If `Key' already exists in
%% the cache, the associated value is replaced by `Value'.
-spec store(any(), any()) -> ok.
store(Key, Value) ->
    gen_server:cast(?MODULE, {store, Key, Value, 0}).

%% @spec store(any(), any(), integer()) -> ok
%% @doc Stores a `Key' - `Value' pair in the cache for `Timeout' seconds. If
%% `Key' already exists in the cache, the associated value is replaced by
%% `Value'. After `Timeout' seconds have elapsed, the key will be removed from
%% the cache. Subsequent stores to the same key will always use the most recent
%% timeout value (if one was given). Storing with a timeout of 0 is equivalent
%% to storing without a timeout.
-spec store(any(), any(), non_neg_integer()) -> ok.
store(Key, Value, Timeout) when Timeout >= 0 ->
    gen_server:cast(?MODULE, {store, Key, Value, Timeout}).

%% @hidden
init([]) ->
    {ok, new_table()}.

%% @hidden
handle_call(load_all, _From, Table) ->
    Now = ecache_util:now_seconds(),
    LoadFun =
        fun({Key, Value, ExpTime}, Loaded) ->
            %% only return {Key, Value} pairs which have not already expired
            case ExpTime > 0 andalso ExpTime < Now of
                true ->
                    true = ets:delete(Table, Key),
                    Loaded;
                false -> [{Key, Value}|Loaded]
            end
        end,
    Reply = {ok, ets:foldr(LoadFun, [], Table)},
    {reply, Reply, Table};
handle_call({load, Key}, _From, Table) ->
    case load_and_delete_if_expired(Key, Table) of
        {error, not_found} ->
            {reply, {error, not_found}, Table};
        {ok, {Key, Value, _ExpTime}} ->
            {reply, {ok, Value}, Table}
    end;
handle_call(count, _From, Table) ->
    Count = proplists:get_value(size, ets:info(Table)),
    {reply, Count, Table};
handle_call({increment_or_decrement, Key, Fun}, _From, Table) ->
    case load_and_delete_if_expired(Key, Table) of
        {error, not_found} ->
            {reply, {error, not_found}, Table};
        {ok, {Key, Value, ExpTime}} ->
            case is_integer(Value) of
                true ->
                    NewValue = Fun(Value),
                    true = ets:insert(Table, {Key, NewValue, ExpTime}),
                    {reply, {ok, NewValue}, Table};
                false ->
                    {reply, {error, not_an_integer}, Table}
            end
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast({store, Key, Value, 0}, Table) ->
    %% 0 is the magic number indicating that the value will not expire
    true = ets:insert(Table, {Key, Value, 0}),
    {noreply, Table};
handle_cast({store, Key, Value, Timeout}, Table) ->
    ExpTime = ecache_util:now_seconds() + Timeout,
    true = ets:insert(Table, {Key, Value, ExpTime}),
    ecache_purge_server:store(Key, ExpTime),
    {noreply, Table};
handle_cast({purge, Key}, Table) ->
    load_and_delete_if_expired(Key, Table),
    {noreply, Table};
handle_cast({delete, Key}, Table) ->
    true = ets:delete(Table, Key),
    {noreply, Table};
handle_cast(flush, Table) ->
    true = ets:delete(Table),
    {noreply, new_table()};
handle_cast(stop, Table) ->
    true = ets:delete(Table),
    {stop, normal, Table};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

new_table() ->
    ets:new(ecache_table, [private]).

%% @spec load_and_delete_if_expired(any(), ets:tid()) -> {ok, any()} |
%%                                                       {error, not_found}
%% @doc If Key is in the table and not expired, {ok, Value} is returned. 
%% Otherwise, {error, not_found} is returned and Key is deleted from the table
%% if it was found and expired.
-spec load_and_delete_if_expired(any(), ets:tid()) -> {ok, any()} |
                                                      {error, not_found}.
load_and_delete_if_expired(Key, Table) ->
    case ets:lookup(Table, Key) of
        [] -> {error, not_found};
        [Data|[]] ->
            {Key, _Value, ExpTime} = Data,
            %% check if the value has already expired
            case ExpTime > 0 andalso ExpTime < ecache_util:now_seconds() of
                true ->
                    %% the value has expired - delete it and return an error
                    true = ets:delete(Table, Key),
                    {error, not_found};
                false ->
                    {ok, Data}
            end
    end.

-ifdef(TEST).

count_test() ->
    ecache:start_link(),
    ?assertEqual(0, ecache:count()),
    ecache:store(apple, 1),
    ecache:store(peach, 1),
    ?assertEqual(2, ecache:count()),
    ecache:delete(apple),
    ?assertEqual(1, ecache:count()),
    ecache:stop().

decrement_test() ->
    ecache:start_link(),
    ecache:store(apple, 10),
    ?assertEqual({ok, 9}, ecache:decrement(apple)),
    ?assertEqual({ok, 8}, ecache:decrement(apple)),
    ?assertEqual({ok, 8}, ecache:load(apple)),
    ecache:store(apple, "10"),
    ?assertEqual({error, not_an_integer}, ecache:decrement(apple)),
    ?assertEqual({error, not_found}, ecache:decrement(peach)),
    ecache:stop().

delete_test() ->
    ecache:start_link(),
    ecache:store(apple, 1),
    ecache:store(peach, 1),
    ecache:delete(apple),
    ?assertEqual({ok, 1}, ecache:load(peach)),
    ?assertEqual({error, not_found}, ecache:load(apple)),
    ecache:stop().

flush_test() ->
    ecache:start_link(),
    ecache:store(apple, 1),
    ecache:store(peach, 1),
    ecache:flush(),
    ?assertEqual(0, ecache:count()),
    ?assertEqual({error, not_found}, ecache:load(apple)),
    ?assertEqual({error, not_found}, ecache:load(peach)),
    ecache:stop().

increment_test() ->
    ecache:start_link(),
    ecache:store(apple, 10),
    ?assertEqual({ok, 11}, ecache:increment(apple)),
    ?assertEqual({ok, 12}, ecache:increment(apple)),
    ?assertEqual({ok, 12}, ecache:load(apple)),
    ecache:store(apple, "10"),
    ?assertEqual({error, not_an_integer}, ecache:increment(apple)),
    ?assertEqual({error, not_found}, ecache:increment(peach)),
    ecache:stop().

load_store_test() ->
    ecache:start_link(),
    ecache:store({autos, <<"ford">>}, 10),
    ecache:store({autos, <<"toyota">>}, 9),
    ecache:store("intel", "Santa Clara"),
    ecache:store("amd", "Sunnyvale"),
    ?assertEqual({ok, 10}, ecache:load({autos, <<"ford">>})),
    ?assertEqual({ok, 9}, ecache:load({autos, <<"toyota">>})),
    ?assertEqual({ok, "Santa Clara"}, ecache:load("intel")),
    ?assertEqual({ok, "Sunnyvale"}, ecache:load("amd")),
    ?assertEqual({error, not_found}, ecache:load(<<"amd">>)),
    ecache:store({autos, <<"toyota">>}, 1),
    ?assertEqual({ok, 1}, ecache:load({autos, <<"toyota">>})),
    ecache:stop().

expiration_test() ->
    ecache:start_link(),
    ecache:store(fruit, "apple", 1),
    ?assertEqual({ok, "apple"}, ecache:load(fruit)),
    timer:sleep(2000),
    ?assertEqual({error, not_found}, ecache:load(fruit)),
    ?assertEqual(0, ecache:count()),
    ecache:stop().

load_all_test() ->
    ecache:start_link(),
    ecache:store(a, "a", 1),
    ecache:store(b, "b", 5),
    ecache:store(c, "c"),
    {ok, L1} = ecache:load_all(),
    Expected1 = [{a, "a"}, {b, "b"}, {c, "c"}],
    timer:sleep(2000),
    {ok, L2} = ecache:load_all(),
    Expected2 = [{b, "b"}, {c, "c"}],
    ?assertEqual(Expected1, lists:sort(L1)),
    ?assertEqual(Expected2, lists:sort(L2)),
    ecache:stop().

-endif.
