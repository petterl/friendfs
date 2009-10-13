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
%%% @end
%%% Created : 13 Oct 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_chunk_replicator).

-behaviour(gen_server).
-include("friendfs.hrl").

%% API
-export([start/1]).

%% Replication API
-export([info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 10000).

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
    % fetch all chunks
    [C | _] = ets:match_object(chunks, #chunk{id='$1', ratio=0, _='_'}),
    [S | _] = C#chunk.storages,
    % find the actions needed for each and put in Queue
    io:format("Chunk with no ratio; ~p ~p~n", [C, S]),
    Action = {delete, C#chunk.id, S},
    {noreply, State#state{queue=Q++[Action]}, ?INTERVAL};

handle_cast(_Msg, State) ->
    {noreply, State, ?INTERVAL}.


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
    action(Action),
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

action({delete, ChunkId, StorageUrl}) ->
    [Storage] = ets:lookup(storages, StorageUrl),
    gen_server:cast(Storage#storage.pid, {delete, ChunkId});

action(Action) ->
    io:format("action: ~p~n", [Action]).
%%     io:format("DO ACTION: ~p~n", [Action]),
%%     ok.

