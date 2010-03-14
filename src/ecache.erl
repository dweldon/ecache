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

-module(ecache).
-behaviour(gen_server).
-export([start_link/0,
         stop/0,
         load_all/0,
         load/1,
         store/2,
         store/3,
         delete/1,
         count/0,
         flush/0,
         increment/1,
         decrement/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-include_lib("eunit/include/eunit.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    {ok, new_table()}.

% @spec load_all() -> {ok, [Row]}
%       Row = {Key::any()), Value::any())}
load_all() ->
    gen_server:call(?MODULE, load_all).

% @spec load(Key::any()) -> {ok, any()} | {error, not_found}
load(Key) ->
    gen_server:call(?MODULE, {load, Key}).

% @spec store(Key::any(), Value::any()) -> ok
store(Key, Value) ->
    gen_server:cast(?MODULE, {store, Key, Value, 0}).

% @spec store(Key::any(), Value::any(), Seconds::integer()) -> ok
store(Key, Value, Seconds) when Seconds >= 0 ->
    gen_server:cast(?MODULE, {store, Key, Value, Seconds}).

% @spec delete(Key::any()) -> ok
delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

% @spec count() -> integer()
count() ->
    gen_server:call(?MODULE, count).

% @spec flush() -> ok
flush() ->
    gen_server:cast(?MODULE, flush).

% @spec increment(Key::any()) -> {ok, integer()} |
%                                {error, not_found} |
%                                {error, not_an_integer}
increment(Key) ->
    Fun = fun(X) -> X + 1 end,
    gen_server:call(?MODULE, {increment_or_decrement, Key, Fun}).

% @spec decrement(Key::any()) -> {ok, integer()} |
%                                {error, not_found} |
%                                {error, not_an_integer}
decrement(Key) ->
    Fun = fun(X) -> X - 1 end,
    gen_server:call(?MODULE, {increment_or_decrement, Key, Fun}).

%-------------------------------------------------------------------------------
handle_call(load_all, _From, Table) ->
    Now = ecache_util:now_seconds(),
    LoadFun =
        fun({Key, Value, ExpTime}, Loaded) ->
            % check if the value has already expired
            case ExpTime > 0 andalso ExpTime < Now of
                true ->
                    % the value has expired - delete it
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

handle_cast({store, Key, Value, 0}, Table) ->
    % 0 is the magic number indicating that the value will not expire
    true = ets:insert(Table, {Key, Value, 0}),
    {noreply, Table};
handle_cast({store, Key, Value, Seconds}, Table) ->
    ExpTime = ecache_util:now_seconds() + Seconds,
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

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%-------------------------------------------------------------------------------
new_table() ->
    ets:new(ecache_table, [private]).

% @spec load(Key::any()) -> {ok, any()} | {error, not_found}
load_and_delete_if_expired(Key, Table) ->
    case ets:lookup(Table, Key) of
        [] -> {error, not_found};
        [Data|[]] ->
            {Key, _Value, ExpTime} = Data,
            % check if the value has already expired
            case ExpTime > 0 andalso ExpTime < ecache_util:now_seconds() of
                true ->
                    % the value has expired - delete it and return an error
                    true = ets:delete(Table, Key),
                    {error, not_found};
                false ->
                    {ok, Data}
            end
    end.

%-------------------------------------------------------------------------------
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

expiration_test() ->
    ecache:start_link(),
    ecache:store(fruit, "apple", 1),
    ?assertEqual({ok, "apple"}, ecache:load(fruit)),
    timer:sleep(2000),
    ?assertEqual({error, not_found}, ecache:load(fruit)),
    ?assertEqual(0, ecache:count()),
    ecache:stop().

delete_test() ->
    ecache:start_link(),
    ecache:store(apple, 1),
    ecache:store(peach, 1),
    ecache:delete(apple),
    ?assertEqual({ok, 1}, ecache:load(peach)),
    ?assertEqual({error, not_found}, ecache:load(apple)),
    ecache:stop().

count_test() ->
    ecache:start_link(),
    ?assertEqual(0, ecache:count()),
    ecache:store(apple, 1),
    ecache:store(peach, 1),
    ?assertEqual(2, ecache:count()),
    ecache:delete(apple),
    ?assertEqual(1, ecache:count()),
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
