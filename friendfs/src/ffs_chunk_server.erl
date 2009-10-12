%%%-------------------------------------------------------------------
%%% @author Petter Sandholdt <petter@sandholdt.se>
%%% @doc
%%% == Chunk server ==
%%%
%%% Manages all chunks and storages
%%%
%%% This is where you do all communication with the storages, like
%%% read, write and delete. It also handles the replication of data
%%% to other stores if the ratio is to low.
%%%
%%% @end
%%% Created : 10 Aug 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_chunk_server).

-behaviour(gen_server).
-include("friendfs.hrl").

%% API
-export([start/1]).

%% Storage API
-export([read/1, write/2, write/3, delete/1]).
-export([register_storage/3, update_storage/4]).
-export([info/0]).


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
          ref,            % Monitor Reference
          priority = 100  % Priority of storage
         }). 
 
-record(chunk, {
          id,           % Chunk ID
          ratio = 1,    % Maximum Requested chunk ratio by fileservers
          storages = [] % List of storage URLs where chunk is stored
        }).

%%====================================================================
%% Typedefs 
%%====================================================================
%% @type config() = list().
%%     Contains configuration settings for ther system. 
%% @end
%% @type chunk_id() = string(). 
%%     The ID of a chunk. A generated unique string.
%% @end
%% @type storage_url() = string().  
%%     The url that this storage is connected to.
%% @end


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the chunk server
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
%% Read data from a chunk from any storage
%%
%% @spec
%%   read(ChunkId :: chunk_id()) -> ok | {error, Reason}
%%     Reason = enoent | eacces | eisdir | enotdir | enomem | atom()
%% @end
%%--------------------------------------------------------------------
read(ChunkId) ->
    gen_server:call(?SERVER, {read, ChunkId}).

%% --------------------------------------------------------------------
%% @equiv write(ChunkId, Data, infinite)
%% @end
%% --------------------------------------------------------------------
write(ChunkId, Data) ->
    write(ChunkId, Data, infinite).

%%--------------------------------------------------------------------
%% @doc
%% Create or Update data for a chunk in storages
%% Ratio is a a value of how many storages it should atleast be stored on.
%% If Ratio = infinite means store on all storages.
%%
%% @spec
%%   write(chunk_id(), binary(), Ratio) -> ok | {error, Reason}
%%     Ratio = infinite | int() 
%%     Reason = already_exists | enoent | eacces | eisdir | enotdir | atom()
%% @end
%%--------------------------------------------------------------------
write(ChunkId, Data, Ratio) ->
    gen_server:call(?SERVER, {write, ChunkId, Ratio, Data}).

%%--------------------------------------------------------------------
%% @doc
%% Delete a chunk from all storages
%%
%% @spec
%%   delete(chunk_id()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
delete(ChunkId) ->
    gen_server:call(?SERVER, {delete, ChunkId}).

%%--------------------------------------------------------------------
%% @doc
%%   Used by storages to register themselves in the chunk server
%%
%% @spec
%%   register_storage(storage_url(), pid(), [chunk_id()]) -> ok
%% @end
%%--------------------------------------------------------------------
register_storage(Url, Pid, ChunkIds) ->
    gen_server:cast(?SERVER, {update_storage, Pid, Url, ChunkIds, []}).

%%--------------------------------------------------------------------
%% @doc
%%   Used by storages to update their status
%%
%% @spec
%%   update_storage(storage_url(), pid(), [chunk_id()], [chunk_id()]) -> ok
%% @end
%%--------------------------------------------------------------------
update_storage(Url, Pid, Added, Removed) ->
    gen_server:cast(?SERVER, {update_storage, Pid, Url, Added, Removed}).

%%--------------------------------------------------------------------
%% @doc
%%   Get info about connected stores and chunks
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
    process_flag(trap_exit, true),
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
handle_call({ read, ChunkId}, From, State) ->
    % Find Chunk information from ChunkId
    Chunk = lists:keyfind(ChunkId, #chunk.id, State#state.chunks),
    % Find all storages having this chunk
    StoragesWithChunk = Chunk#chunk.storages,
    % Get the Pid of the prefered storage to fetch from
    StoragePid = choose_storage(ChunkId, StoragesWithChunk,
                                State#state.storages),
    % Send a read cast to that storage and return
    % (the storage will reply with data or error)
    gen_server:cast(StoragePid, {read, ChunkId, From}),
    {noreply, State};

handle_call({ write, ChunkId, Ratio, Data}, _From, State) ->
    case lists:keysearch(ChunkId, #chunk.id, State#state.chunks) of
        {value, _} ->
            {reply, {error, already_exists}, State};
        _ ->
            [Storage | _OtherStores] =
                choose_write_storages(Ratio, State#state.storages),
            % store on one server
            Res = store_and_schedule(Storage, ChunkId, Data),
            {reply, Res, State}
    end;

handle_call({ delete, ChunkId}, _From, State) ->
    % Find Chunk information from ChunkId
    case lists:keysearch(ChunkId, #chunk.id, State#state.chunks) of
        false ->
            Chunks = State#state.chunks,
            Res = {error, missing};
        {value, Chunk} ->
            % Remove from chunklist by setting ratio to 0
            Chunks = lists:keyreplace(ChunkId, #chunk.id, State#state.chunks, Chunk#chunk{ratio=0}),
            % TODO:Schedule removal from all storages
            % Right now, remove from all
            lists:map(fun(#storage{pid = StoragePid}) ->
                              gen_server:cast(StoragePid, {delete, ChunkId})
                      end,
                      get_storage_from_url(Chunk#chunk.storages,
                                           State#state.storages)),
            Res = ok                
    end,
    {reply, Res, State#state{chunks = Chunks}};

handle_call( info, _From, State) ->
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
handle_cast({update_storage, From, Url, AddedChunkIds, RemovedChunkIds}, State) ->
    case lists:keymember(From, #storage.pid, State#state.storages) of
        true ->
            Chunks0 = remove_storage_from_chunks(State#state.chunks, RemovedChunkIds, Url),
            Chunks = add_storage_to_chunks(Chunks0, AddedChunkIds, Url),
            Storages = State#state.storages;
        false ->
            % Monitor that storage if it goes down
            Ref = erlang:monitor(process, From),
            % Add storage information to list of connect storages
            Storages = [#storage{url = Url, pid = From, ref = Ref} | State#state.storages],
            % Update handled chunks
            Chunks = add_storage_to_chunks(State#state.chunks, AddedChunkIds, Url)
    end,
    {noreply, State#state{storages = Storages, chunks = Chunks}};

handle_cast(_Msg, State) ->
    {noreply, State}.


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
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    Storages = lists:keydelete(Pid, #storage.pid, State#state.storages),
    Chunks = remove_storage_from_chunks(State#state.chunks, all, Pid),
    {noreply, State#state{storages = Storages, chunks = Chunks}};
handle_info(_Info, State) ->
    io:format("Info ~p~n", [_Info]),
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
%find_module(Url) ->
%    {Scheme, _Rest} = friendfs_lib:urlsplit_scheme(Url),
%    find_module_by_scheme(Scheme).

%find_module_by_scheme(local) ->
%    ffs_storage_file;
%find_module_by_scheme(ram) ->
%    ffs_storage_mem;
%find_module_by_scheme(Type) ->
%    throw({no_storage_module_availible, Type}).

%find_storage(Url, []) ->
%    throw({storage_module_missing, Url});
%find_storage(Url, [Storage = #storage{url = Url} | _]) ->
%    Storage;
%find_storage(Url, [_ | R]) ->
%    find_storage(Url, R).



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds the Pid in the list of storages for each chunk in Chunklist
%%
%% @spec add_storage_to_chunks(list(), list(), tuple()) -> list()
%% @end
%%--------------------------------------------------------------------
add_storage_to_chunks([], [], _Storage) ->
    [];
add_storage_to_chunks([], [StorageChunkId | R], Storage) ->
    [#chunk{id=StorageChunkId, storages=[Storage]} |
    add_storage_to_chunks([], R, Storage)];
add_storage_to_chunks([Chunk | R], StorageChunkIds, Storage) ->
    case lists:member(Chunk#chunk.id, StorageChunkIds) of
        true ->
            [Chunk#chunk{storages = [Storage | Chunk#chunk.storages]} |
             add_storage_to_chunks(R, StorageChunkIds -- [Chunk#chunk.id], Storage)];
        false ->
            [Chunk | add_storage_to_chunks(R, StorageChunkIds, Storage)]
    end.

remove_storage_from_chunks([], _StorageChunkIds, _Storage) ->
    [];
remove_storage_from_chunks([Chunk | R], all, Storage) ->
    [Chunk#chunk{storages = Chunk#chunk.storages -- [Storage]} |
     remove_storage_from_chunks(R, all, Storage)];
remove_storage_from_chunks([Chunk | R], StorageChunkIds, Storage) ->
    case lists:member(Chunk#chunk.id, StorageChunkIds) of
        true ->
            [Chunk#chunk{storages = Chunk#chunk.storages -- [Storage]} |
             remove_storage_from_chunks(R, StorageChunkIds, Storage)];
        false ->
            [Chunk | remove_storage_from_chunks(R, StorageChunkIds, Storage)]
    end.
    
    
choose_storage(_ChunkId, [], _StorageList) ->
    not_found;
choose_storage(_ChunkId, [StorageURL | _Rest], StorageList) ->
    {value, Storage} = lists:keysearch(StorageURL, #storage.url, StorageList),
    Storage#storage.pid.

choose_write_storages(infinite, StorageList) ->
    StorageList;
choose_write_storages(Ratio, StorageList) ->
    lists:sublist(StorageList, Ratio).

store_and_schedule(#storage{pid = Pid}, ChunkId, Data) ->
    case gen_server:call(Pid, {write, ChunkId, Data}) of
        ok ->
            % TODO: Schedule sync of data
            ok;
        _ ->
            % Failed to store data
            error
    end.

get_storage_from_url([], _Storages) ->
    [];
get_storage_from_url([Url | R], Storages) ->
    case lists:keysearch(Url, #storage.url, Storages) of
        {value, Storage} ->
            [Storage | get_storage_from_url(R, Storages)];
        false ->
            get_storage_from_url(R, Storages)
    end.
