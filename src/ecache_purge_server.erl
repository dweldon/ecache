-module(ecache_purge_server).
-behaviour(gen_server).
-export([start_link/0,
         stop/0,
         store/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-include_lib("eunit/include/eunit.hrl").
-define(PURGE_PAUSE_TIME, 5000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    register(purge_reminder, spawn_link(fun purge_reminder/0)),
    {ok, gb_trees:empty()}.

% @spec store(Key::any(), ExpTime::integer()) -> ok
store(Key, ExpTime) ->
    gen_server:cast(?MODULE, {store, Key, ExpTime}).

%-------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(purge, Tree) ->
    NewTree = purge(Tree),
    {noreply, NewTree};
handle_cast({store, Key, ExpTime}, Tree) ->
    NewTree = gb_trees:insert({ExpTime, now()}, Key, Tree),
    {noreply, NewTree};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    purge_reminder ! stop,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%-------------------------------------------------------------------------------
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

purge_reminder() ->
    receive
        stop -> ok
    after ?PURGE_PAUSE_TIME ->
        gen_server:cast(?MODULE, purge),
        purge_reminder()
    end.
