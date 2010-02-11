-module(ecache).
-behaviour(gen_server).
-export([start_link/0,
         stop/0,
         load/2,
         store/3,
         delete/2,
         count/0,
         flush/0,
         increment/2,
         decrement/2]).
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

% @spec load(Namespace::any(), Key::any()) -> {ok, any()} | {error, not_found}
load(Namespace, Key) ->
    gen_server:call(?MODULE, {load, Namespace, Key}).

% @spec store(Namespace::any(), Key::any(), Value::any()) -> ok
store(Namespace, Key, Value) ->
    gen_server:cast(?MODULE, {store, Namespace, Key, Value}).

% @spec delete(Namespace::any(), Key::any()) -> ok
delete(Namespace, Key) ->
    gen_server:cast(?MODULE, {delete, Namespace, Key}).

% @spec count() -> integer()
count() ->
    gen_server:call(?MODULE, count).

% @spec flush() -> ok
flush() ->
    gen_server:cast(?MODULE, flush).

% @spec increment(Namespace::any(), Key::any()) -> {ok, integer()} |
%                                                  {error, not_found} |
%                                                  {error, not_an_integer}
increment(Namespace, Key) ->
    gen_server:call(?MODULE, {increment, Namespace, Key}).

% @spec decrement(Namespace::any(), Key::any()) -> {ok, integer()} |
%                                                  {error, not_found} |
%                                                  {error, not_an_integer}
decrement(Namespace, Key) ->
    gen_server:call(?MODULE, {decrement, Namespace, Key}).

%-------------------------------------------------------------------------------
handle_call({load, Namespace, Key}, _From, Table) ->
    case ets:lookup(Table, {Namespace, Key}) of
        [] ->
            {reply, {error, not_found}, Table};
        [Data|_] ->
            {{Namespace, Key}, Value} = Data,
            {reply, {ok, Value}, Table}
    end;
handle_call(count, _From, Table) ->
    Count = proplists:get_value(size, ets:info(Table)),
    {reply, Count, Table};
handle_call({increment, Namespace, Key}, _From, Table) ->
    case ets:lookup(Table, {Namespace, Key}) of
        [] ->
            {reply, {error, not_found}, Table};
        [Data|_] ->
            {{Namespace, Key}, Value} = Data,
            case is_integer(Value) of
                true ->
                    true = ets:insert(Table, {{Namespace, Key}, Value+1}),
                    {reply, {ok, Value+1}, Table};
                false ->
                    {reply, {error, not_an_integer}, Table}
            end
    end;
handle_call({decrement, Namespace, Key}, _From, Table) ->
    case ets:lookup(Table, {Namespace, Key}) of
        [] ->
            {reply, {error, not_found}, Table};
        [Data|_] ->
            {{Namespace, Key}, Value} = Data,
            case is_integer(Value) of
                true ->
                    true = ets:insert(Table, {{Namespace, Key}, Value-1}),
                    {reply, {ok, Value-1}, Table};
                false ->
                    {reply, {error, not_an_integer}, Table}
            end
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({store, Namespace, Key, Value}, Table) ->
    true = ets:insert(Table, {{Namespace, Key}, Value}),
    {noreply, Table};
handle_cast({delete, Namespace, Key}, Table) ->
    true = ets:delete(Table, {Namespace, Key}),
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
    ecache:store(autos, <<"ford">>, 10),
    ecache:store(autos, <<"toyota">>, 9),
    ecache:store(<<"cpu">>, "intel", "Santa Clara"),
    ecache:store(<<"cpu">>, "amd", "Sunnyvale"),
    ?assertEqual({ok, 10}, ecache:load(autos, <<"ford">>)),
    ?assertEqual({ok, 9}, ecache:load(autos, <<"toyota">>)),
    ?assertEqual({ok, "Santa Clara"}, ecache:load(<<"cpu">>, "intel")),
    ?assertEqual({ok, "Sunnyvale"}, ecache:load(<<"cpu">>, "amd")),
    ?assertEqual({error, not_found}, ecache:load("cpu", "amd")),
    ecache:store(autos, <<"toyota">>, 1),
    ?assertEqual({ok, 1}, ecache:load(autos, <<"toyota">>)),
    ecache:stop().

delete_test() ->
    ecache:start_link(),
    ecache:store(fruit, apple, 1),
    ecache:store(fruit, peach, 1),
    ecache:delete(fruit, apple),
    ?assertEqual({ok, 1}, ecache:load(fruit, peach)),
    ?assertEqual({error, not_found}, ecache:load(fruit, apple)),
    ecache:stop().

count_test() ->
    ecache:start_link(),
    ?assertEqual(0, ecache:count()),
    ecache:store(fruit, apple, 1),
    ecache:store(fruit, peach, 1),
    ?assertEqual(2, ecache:count()),
    ecache:delete(fruit, apple),
    ?assertEqual(1, ecache:count()),
    ecache:stop().

flush_test() ->
    ecache:start_link(),
    ecache:store(fruit, apple, 1),
    ecache:store(fruit, peach, 1),
    ecache:flush(),
    ?assertEqual(0, ecache:count()),
    ?assertEqual({error, not_found}, ecache:load(fruit, apple)),
    ?assertEqual({error, not_found}, ecache:load(fruit, peach)),
    ecache:stop().

increment_test() ->
    ecache:start_link(),
    ecache:store(fruit, apple, 10),
    ?assertEqual({ok, 11}, ecache:increment(fruit, apple)),
    ?assertEqual({ok, 12}, ecache:increment(fruit, apple)),
    ?assertEqual({ok, 12}, ecache:load(fruit, apple)),
    ecache:store(fruit, apple, "10"),
    ?assertEqual({error, not_an_integer}, ecache:increment(fruit, apple)),
    ?assertEqual({error, not_found}, ecache:increment(fruit, peach)),
    ecache:stop().

decrement_test() ->
    ecache:start_link(),
    ecache:store(fruit, apple, 10),
    ?assertEqual({ok, 9}, ecache:decrement(fruit, apple)),
    ?assertEqual({ok, 8}, ecache:decrement(fruit, apple)),
    ?assertEqual({ok, 8}, ecache:load(fruit, apple)),
    ecache:store(fruit, apple, "10"),
    ?assertEqual({error, not_an_integer}, ecache:decrement(fruit, apple)),
    ?assertEqual({error, not_found}, ecache:decrement(fruit, peach)),
    ecache:stop().
