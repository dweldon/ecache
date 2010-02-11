-module(ecache).
-behaviour(gen_server).
-export([start_link/0,
         stop/0,
         load/1,
         store/2,
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

% @spec load(Key::any()) -> {ok, any()} | {error, not_found}
load(Key) ->
    gen_server:call(?MODULE, {load, Key}).

% @spec store(Key::any(), Value::any()) -> ok
store(Key, Value) ->
    gen_server:cast(?MODULE, {store, Key, Value}).

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
handle_call({load, Key}, _From, Table) ->
    case ets:lookup(Table, Key) of
        [] ->
            {reply, {error, not_found}, Table};
        [Data|[]] ->
            {Key, Value} = Data,
            {reply, {ok, Value}, Table}
    end;
handle_call(count, _From, Table) ->
    Count = proplists:get_value(size, ets:info(Table)),
    {reply, Count, Table};
handle_call({increment_or_decrement, Key, Fun}, _From, Table) ->
    case ets:lookup(Table, Key) of
        [] ->
            {reply, {error, not_found}, Table};
        [Data|[]] ->
            {Key, Value} = Data,
            case is_integer(Value) of
                true ->
                    NewValue = Fun(Value),
                    true = ets:insert(Table, {Key, NewValue}),
                    {reply, {ok, NewValue}, Table};
                false ->
                    {reply, {error, not_an_integer}, Table}
            end
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({store, Key, Value}, Table) ->
    true = ets:insert(Table, {Key, Value}),
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
