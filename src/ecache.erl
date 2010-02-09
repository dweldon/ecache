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

load(Namespace, Key) ->
    gen_server:call(?MODULE, {load, Namespace, Key}).

store(Namespace, Key, Value) ->
    gen_server:cast(?MODULE, {store, Namespace, Key, Value}).

handle_call({load, Namespace, Key}, _From, Table) ->
    case ets:lookup(Table, {Namespace, Key}) of
        [] ->
            {reply, {error, none}, Table};
        Data when length(Data) =:= 1 ->
            {reply, {ok, hd(Data)}, Table}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({store, Namespace, Key, Value}, Table) ->
    true = ets:insert(Table, {{Namespace, Key}, Value}),
    {noreply, Table};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
