-module(ecache).
-behaviour(gen_server).

% TODO:
% increment/1
% decrement/1
% delete/1
% count/0
% flush/0

-export([start_link/0,
         stop/0,
         load/2,
         store/3]).
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
    Table = ets:new(ecache_table, [private]),
    {ok, Table}.

% @spec load(Namespace::any(), Key::any()) -> {ok, any()} | {error, none}
load(Namespace, Key) ->
    gen_server:call(?MODULE, {load, Namespace, Key}).

% @spec store(Namespace::any(), Key::any(), Value::any()) -> ok
store(Namespace, Key, Value) ->
    gen_server:cast(?MODULE, {store, Namespace, Key, Value}).

handle_call({load, Namespace, Key}, _From, Table) ->
    case ets:lookup(Table, {Namespace, Key}) of
        [] ->
            {reply, {error, none}, Table};
        [Data|_] ->
            {{Namespace, Key}, Value} = Data,
            {reply, {ok, Value}, Table}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({store, Namespace, Key, Value}, Table) ->
    true = ets:insert(Table, {{Namespace, Key}, Value}),
    {noreply, Table};
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
    ?assertEqual({error, none}, ecache:load("cpu", "amd")),
    ecache:store(autos, <<"toyota">>, 1),
    ?assertEqual({ok, 1}, ecache:load(autos, <<"toyota">>)),
    ecache:stop().
