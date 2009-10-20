%%%-------------------------------------------------------------------
%%% @author Petter Sandholdt <petter@sandholdt.se>
%%% @doc
%%% == Chunk replicator ==
%%%
%%% Manages a replication system
%%%
%%% This module checks the ratio of chunks and queues actions to make
%%% sure the ratio is kept correct. It does that with close
%%% communication with the chunk server
%%%
%%% ===ActionQueue ===
%%% The action queue contains actions to take for replicationlevel to
%%% be good. It contains tupes of these types:
%%%
%%%  {delete, chunk_id(), storage_url()}
%%%  {copy, chunk_id(), To :: storage_url(), Prio :: integer()}
%%%
%%% Where low prio is first. Priority infinite is for chunks on only one
%%% storage and have a ratio above 1.
%%% priority is based on (ratio - length(storages)) which means that
%%% if a storage is far from its ratio it will get prioritised
%%%
%%% @end
%%% Created : 13 Oct 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_chunk_replicator).

-behaviour(gen_server).
-include("friendfs.hrl").

%% API
-export([start/1]).

%% Replication API
-export([info/0, refresh/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 10000).
-define(HARD_RATIO, 1).

-record(state, {queue = []  % replication queue
            }).

%%====================================================================
%% Typedefs
%%====================================================================

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the chunk replication server
%%
%% @spec
%%   start(config()) ->
%%     {ok, pid()} | {error, {already_started, pid()}}
%% @end
%%--------------------------------------------------------------------
start(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).


%%--------------------------------------------------------------------
%% @doc
%%   Get info about service
%%
%% @spec
%%   info() -> list()
%% @end
%%--------------------------------------------------------------------
info() ->
    gen_server:call(?SERVER, info).

refresh() ->
    gen_server:cast(?SERVER, refresh).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%% {ok, State, Timeout} |
%% ignore |
%% {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Config) ->
    {ok, #state{}, ?INTERVAL}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%% {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call( info, _From, State) ->
    {reply, State, State, ?INTERVAL};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, ?INTERVAL}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add, Action}, State = #state{queue=Q}) ->
    {noreply, State#state{queue=Q++[Action]}, ?INTERVAL};

handle_cast(refresh, State = #state{queue=Q}) ->
    % Find chunks with low ratio
    RatioA = create_copy_actions(),
    RatioA2 = lists:filter(fun(A) -> not lists:member(A, Q) end, RatioA),
    
    % Find delete actions
    DeleteA  = create_delete_actions(),
    % remove actions already in Queue
    DeleteA2 = lists:filter(fun(A) -> not lists:member(A, Q) end, DeleteA),
    
    Q2 = lists:sort(fun prioritize/2, Q ++ RatioA2 ++ DeleteA2),
    {noreply, State#state{queue=Q2}, ?INTERVAL};

handle_cast(_Msg, State) ->
    {noreply, State, ?INTERVAL}.

create_delete_actions() ->
    Match = ets:match(chunks, #chunk{id='$1', storages='$2', ref_cnt=0, _='_'}),
    create_delete_action_tuples(Match).
create_delete_action_tuples([]) ->
    [];
create_delete_action_tuples([[_, []] | R]) ->
    create_delete_action_tuples(R);
create_delete_action_tuples([[ChunkId, [StorageUrl | R]] | R2]) ->
    [{delete, ChunkId, StorageUrl} |
     create_delete_action_tuples([[ChunkId, R] | R2])].

create_copy_actions() ->
    Match = ets:match(chunks, #chunk{id='$1', storages='$2', ratio='$3', _='_'}),
    create_copy_action_tuples(Match).

create_copy_action_tuples([]) ->
    [];
create_copy_action_tuples([[_ChunkId, _Storages, 0]|R]) ->
    create_copy_action_tuples(R);
create_copy_action_tuples([[_ChunkId, _Storages, undefined]|R]) ->
    create_copy_action_tuples(R);
create_copy_action_tuples([[_ChunkId, Storages, Ratio]|R])
  when length(Storages) >= Ratio ->
    create_copy_action_tuples(R);
create_copy_action_tuples([[ChunkId, Storages, _Ratio] | R])
  when length(Storages) =< ?HARD_RATIO ->
    % Needs to be replicated now
    [{copy, ChunkId, infinite} |
     create_copy_action_tuples(R)];
create_copy_action_tuples([[ChunkId, Storages, infinite]|R]) ->
    AllStorages = ets:match(storages, #storage{url='$1', _='_'}),
    case lists:filter(
           fun([S]) -> not lists:member(S, Storages) end, AllStorages) of
        [] -> create_copy_action_tuples(R);
        Remaining ->
            [{copy, ChunkId, length(Remaining)} |
             create_copy_action_tuples(R)]
    end;
create_copy_action_tuples([[ChunkId, Storages, Ratio]|R]) ->
    Prio = Ratio - length(Storages),
    [{copy, ChunkId, Prio} | create_copy_action_tuples(R)].

prioritize({copy, _, _}, {delete, _, _}) ->   true;
prioritize({delete, _, _}, {delete, _, _}) -> true;
prioritize({copy, _, Ratio1}, {copy, _, Ratio2})
  when Ratio1 > Ratio2 ->
    true;
prioritize({copy, _, infinite}, _) ->
    true;
prioritize(_, _) -> false.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State = #state{queue=[]}) ->
    {noreply, State, ?INTERVAL};
handle_info(timeout, State = #state{queue=[Action | Queue]}) ->
    Res = action(Action),
    io:format("~p: ~p -> ~p~n", [?MODULE, Action, Res]),
    {noreply, State#state{queue=Queue}, ?INTERVAL};
handle_info(_Info, State) ->
    io:format("Info ~p~n", [_Info]),
    {noreply, State, ?INTERVAL}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%==================================================================

%% add_action(copy, _From, [], _ChunkId, State) ->
%%     State;
%% add_action(copy, From, [To | Remaining], ChunkId, State) ->
%%     Action = {copy, From, To, ChunkId},
%%     Queue = State#state.replicate_queue ++ [Action],
%%     State2 = State#state{replicate_queue = Queue},
%%     add_action(copy, From, Remaining, ChunkId, State2).

action(_A = {delete, ChunkId, StorageUrl}) ->
    ffs_chunk_server:delete(ChunkId, StorageUrl);

action(_A = {copy, ChunkId, _Ratio}) ->
    case ffs_chunk_server:read(ChunkId) of
        {ok, Data} ->
            ffs_chunk_server:write_replicate(ChunkId, Data);
        _ ->
            {error, {read_error, ChunkId}}
    end;

action(Action) ->
    {error, {unknown_action, Action}}.

