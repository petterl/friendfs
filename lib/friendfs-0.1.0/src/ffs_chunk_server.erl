%%%-------------------------------------------------------------------
%%% @author Petter Sandholdt <petter@sandholdt.se>
%%% @doc
%%% Storage Manager
%%%
%%% Manages all storages and prioritize storages at read and write
%%% Verify a storage before making it availible
%%%
%%% @end
%%% Created : 10 Aug 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_chunk_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% Storage API
-export([read/1, write/2]).
-export([register_storage/3, update_storage/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {storages = [],  %% Registered storages
                chunks = []     %% List of {Chunk, RequestedRatio}
                }).

-record(storage, {
          url,            % storage URL
          pid,            % Pid of storage process
          priority = 100  % Priority of storage
         }).

-record(chunk, {
          id,           % Chunk ID
          ratio = 1,    % Maximum Requested chunk ratio by fileservers
          storages = [] % List of pids where chunk is stored
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Config) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

%%--------------------------------------------------------------------
%% @doc
%% Read a chunk from a storage
%%
%% @spec
%% read(ChunkId) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
read(Cid) ->
    gen_server:call(?SERVER, {read, Cid}).

%%--------------------------------------------------------------------
%% @doc
%% Write a chunk to storages
%%
%% @spec
%% write(ChunkId, Data) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
write(Cid, Data) ->
    gen_server:call(?SERVER, {write, Cid, all, Data}).

%%--------------------------------------------------------------------
%% @doc
%% List all configured Storages
%%
%% @spec
%% register_storage(FromPid, Url, Chunks) -> ok
%% @end
%%--------------------------------------------------------------------
register_storage(FromPid, Url, Chunks) ->
    gen_server:cast(?SERVER, {register_storage, FromPid, Url, Chunks}).

update_storage(FromPid, Added, Removed) ->
    gen_server:cast(?SERVER, {update_storage, FromPid, Added, Removed}).

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
    {ok, #state{}}.

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
handle_call({read, ChunkId}, _From, State) ->
    Storages = lists:keysearch(ChunkId, #chunk.id, State#state.chunks),
    StoragePid = choose_storage(ChunkId, Storages, State#state.storages),
    Res = gen_server:call(StoragePid, {read, ChunkId}),
    {reply, Res, State};
handle_call({write, Cid, _Ratio=all, Data}, _From, State) ->
    Res = lists:map(
            fun(#storage{pid = Pid}) ->
                    gen_server:call(Pid, {write, Cid, Data}) end,
            State#state.storages),
    {reply, Res, State};
handle_call({write, Cid, Ratio, Data}, _From, State) ->
    Storages = lists:sublist(State#state.storages, Ratio),
    Res = lists:map(
            fun(#storage{pid = Pid}) ->
                    gen_server:call(Pid, {write, Cid, Data}) end,
            Storages),
    {reply, Res, State};
handle_call(info, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({register_storage, From, Url, Chunks}, State) ->
    Storages = [#storage{url = Url, pid = From} | State#state.storages],
    Chunks2 = add_pid_to_chunks(State#state.chunks, Chunks, From),
    {noreply, State#state{storages = Storages, chunks = Chunks2}};
handle_cast({update_storage, From, Added, Removed}, State) ->
    %io:format("Changed Files: ~nAdded~n~p~nRemoved~n~p~n", [Added, Removed]),
    Chunks  = remove_pid_from_chunks(State#state.chunks, Removed, From),
    Chunks2 = add_pid_to_chunks(Chunks, Added, From),
    {noreply, State#state{chunks = Chunks2}};

handle_cast(_Msg, State) ->
    {noreply, State}.

add_pid_to_chunks([], Chunks, Pid) ->
    lists:map(fun(Cid) -> #chunk{id=Cid, storages = [Pid]} end, Chunks);
add_pid_to_chunks([CRec | R], NewChunkList, Pid) ->
    {UpdatedChunk, NewChunkList2} =
        case lists:member(CRec#chunk.id, NewChunkList) of
            true -> {CRec#chunk{storages = [Pid | CRec#chunk.storages]},
                     lists:delete(CRec#chunk.id, NewChunkList)};
            false -> {CRec, NewChunkList}
        end,
    [UpdatedChunk | add_pid_to_chunks(R, NewChunkList2, Pid)].

remove_pid_from_chunks([], _Chunks, _Pid) ->
    [];
remove_pid_from_chunks([CRec | R], DelChunkList, Pid) ->
    {UpdatedChunk, DelChunkList2} =
        case lists:member(CRec#chunk.id, DelChunkList) of
            true -> {CRec#chunk{storages = lists:subtract([Pid], CRec#chunk.storages)},
                     lists:delete(CRec#chunk.id, DelChunkList)};
            false -> {CRec, DelChunkList}
        end,
    [UpdatedChunk | remove_pid_from_chunks(R, DelChunkList2, Pid)].


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
handle_info(_Info, State) ->
    {noreply, State}.

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
find_module(Url) ->
    {Scheme, _Rest} = friendfs_lib:urlsplit_scheme(Url),
    find_module_by_scheme(Scheme).

find_module_by_scheme(local) ->
    ffs_storage_file;
find_module_by_scheme(ram) ->
    ffs_storage_mem;
find_module_by_scheme(Type) ->
    throw({no_storage_module_availible, Type}).

find_storage(Url, []) ->
    throw({storage_module_missing, Url});
find_storage(Url, [Storage = #storage{url = Url} | _]) ->
    Storage;
find_storage(Url, [_ | R]) ->
    find_storage(Url, R).

choose_storage(_ChunkId, [Storage | _Rest], _StorageData) ->
    Storage.
