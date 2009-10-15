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
-export([read/1, read_async/2, write/2, write/3, delete/1, delete/2]).
-export([write_replicate/2, update_storage/3]).
-export([info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {storages = []  %% Registered storages
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
%% @type action() = tuple().
%%     An action for the replication system
%%     {copy, StorageUrl, Chunk, TargetStorageUrl}
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
%%   read(ChunkId :: chunk_id()) -> {ok, Data} | {error, Reason}
%%     Reason = enoent | eacces | eisdir | enotdir | enomem | atom()
%% @end
%%--------------------------------------------------------------------
read(ChunkId) ->
    gen_server:call(?SERVER, {read, ChunkId}).

%%--------------------------------------------------------------------
%% @doc
%% Read data from a chunk from any storage
%% Respond by calling the callback function with the response
%%
%% @spec
%%   read_async(ChunkId :: chunk_id(), Fun :: fun()) -> ok
%%     Response = {ok, Data} | {error, Reason}
%%     Reason   = enoent | eacces | eisdir | enotdir | enomem | atom()
%% @end
%%--------------------------------------------------------------------
read_async(ChunkId, Fun) ->
    gen_server:cast(?SERVER, {read_async, ChunkId, Fun}).


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
%% Replicate data for chunk to new storages
%%
%% Used by replication server when replication of a chunk is needed
%%
%% @spec
%%   write_replicate(chunk_id(), binary()) -> ok | {error, Reason}
%%     Ratio = infinite | int()
%%     Reason = already_exists | enoent | eacces | eisdir | enotdir | atom()
%% @end
%%--------------------------------------------------------------------
write_replicate(ChunkId, Data) ->
    gen_server:call(?SERVER, {write_replicate, ChunkId, Data}).

%%--------------------------------------------------------------------
%% @doc
%% Delete a chunk from all storages
%%
%% It actuallt sets the ratio for the chunk to 0 and letting the
%% replicator delete the chunks when possible
%%
%% @spec
%%   delete(chunk_id()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
delete(ChunkId) ->
    gen_server:call(?SERVER, {delete, ChunkId}).

%%--------------------------------------------------------------------
%% @doc
%% Delete a chunk from a specific storage
%%
%% Called by the replicator to delete from specific storages.
%% Will always return ok, even if nothins was deleted.
%%
%% @spec
%%   delete(chunk_id(), storage_url()) -> ok
%% @end
%%--------------------------------------------------------------------
delete(ChunkId, StorageUrl) ->
    gen_server:cast(?SERVER, {delete, ChunkId, StorageUrl}).

%%--------------------------------------------------------------------
%% @doc
%%   Used by storages to update their status
%%
%% @spec
%%   update_storage(storage_url(), pid(), [chunk_id()], [chunk_id()]) -> ok
%% @end
%%--------------------------------------------------------------------
update_storage(Url, Pid, ChunkIds) ->
    gen_server:cast(?SERVER, {update_storage, Pid, Url, ChunkIds}).

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
    ets:new(storages, [set,protected,named_table,{keypos, 2}]),
    ets:new(chunks, [set,protected,named_table,{keypos, 2}]),
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
    case ets:lookup(chunks, ChunkId) of
        [#chunk{storages = StorageUrls}] ->
            % Get the prefered storage to fetch from
            case choose_storage(StorageUrls) of
                #storage{pid = Pid} ->
                    % Send a read cast to that storage and return
                    % (the storage will reply with data or error)
                    gen_server:cast(Pid, {read, ChunkId, From}),
                    {noreply, State};
                not_found ->
                    {reply, {error, enoent}, State}
            end;
        [] ->
            {reply, {error, enoent}, State}
    end;

handle_call({ write, ChunkId, Ratio, Data}, _From, State) ->
    case ets:lookup(chunks, ChunkId) of
        [#chunk{}] ->
            {reply, {error, already_exists}, State};
        [] ->
            % store on one server
            case write_to_one(ets:tab2list(storages), ChunkId, Data) of
                {ok, Storage} ->
                    ets:insert(chunks, #chunk{id=ChunkId, ratio=Ratio,
                               storages=[Storage#storage.url]}),
                    ffs_chunk_replicator:refresh(),
                    {reply, ok, State};
                {error, _} = Error ->
                    {reply, Error, State}
            end
    end;

handle_call({ write_replicate, ChunkId, Data}, _From, State) ->
    case ets:lookup(chunks, ChunkId) of
        [#chunk{storages = StorageUrls, ratio = Ratio}] when
           length(StorageUrls) >= Ratio;
           Ratio == infinite ->
            AllStorages = ets:tab2list(storages),

            case lists:filter(
                   fun(#storage{url=Url}) ->
                           not lists:member(Url, StorageUrls)
                   end, AllStorages) of
                [] ->
                    {reply, {error, not_needed}, State};
                Remaining ->
                    S = choose_write_storage(Remaining),
                    io:format("Replicate ~p to ~p~n", [ChunkId, S#storage.url]),
                    Res = gen_server:call(S#storage.pid, {write, ChunkId, Data}),
                    ets:update_element(chunks, ChunkId,
                                       {#chunk.storages, StorageUrls ++ [S#storage.url]}),
                    {reply, Res, State}
            end;
        [#chunk{}] ->
            {reply, {error, not_needed}, State};
        [] ->
            {reply, {error, chunk_not_found}, State}
    end;

handle_call({ delete, ChunkId}, _From, State) ->
    Res =
        case ets:update_element(chunks, ChunkId, {#chunk.ratio, 0}) of
            true ->
                ffs_chunk_replicator:refresh(),
                ok;
            false ->
                {error, not_found}
        end,
    {reply, Res, State};

handle_call( info, _From, State) ->
    S = ets:tab2list(storages),
    C = ets:tab2list(chunks),
    io:format("Storages:~n~p~nChunks:~n~p~n", [S, C]),
    {reply, ok, State};

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
handle_cast({ read_async, ChunkId, Fun}, State) ->
    %% Find Chunk information from ChunkId
    case ets:lookup(chunks, ChunkId) of
        [#chunk{storages = StorageUrls}] ->
            read_async_int(StorageUrls, ChunkId, Fun, no_error);
        [] ->
            Fun({error, enoent})
    end,
    {noreply, State};

handle_cast({ delete, ChunkId, StorageUrl}, State) ->
    case ets:lookup(chunks, ChunkId) of
        [#chunk{storages = StorageUrls}] ->
            case ets:lookup(storages, StorageUrl) of
                [S = #storage{}] ->
                    gen_server:cast(S#storage.pid, {delete, ChunkId}),
                    ets:update_element(chunks, ChunkId,
                                       {#chunk.storages,
                                        StorageUrls -- [S#storage.url]});
                [] ->
                    % Error: Storage missing, ignore
                    ok
            end;
        [] ->
            % Error: Chunk missing, ignore
            ok
    end,
    {noreply, State};

handle_cast({update_storage, From, Url, ChunkIds}, State) ->
    case ets:lookup(storages, Url) of
        [#storage{}] ->
            Updated =
                ets:foldl(
                  fun(#chunk{id = ChunkId, storages=StorageUrls}, Acc) ->
                          case lists:member(ChunkId, ChunkIds) of
                              true ->
                                  % Chunk in this storage
                                  case lists:member(Url, StorageUrls) of
                                      true ->
                                          Acc;
                                      false ->
                                          % Added
                                          ets:update_element(chunks, ChunkId,
                                                             {#chunk.storages, StorageUrls ++ [Url]}),
                                          true
                                  end;
                              false ->
                                  % chunk not in storage
                                  case lists:member(Url, StorageUrls) of
                                      true ->
                                          % Removed
                                          ets:update_element(chunks, ChunkId,
                                                             {#chunk.storages, StorageUrls -- [Url]}),
                                          true;
                                      false ->
                                          Acc
                                  end
                          end
                  end,
                  false, chunks),
            case Updated of
                true ->
                    ffs_chunk_replicator:refresh();
                false ->
                    ok
            end;
%                    add_storage_to_chunks(AddedChunkIds, Url),
%            remove_storage_from_chunks(RemovedChunkIds, Url),

        [] ->
            % Monitor that storage if it goes down
            Ref = erlang:monitor(process, From),
            % Add storage information to list of connect storages
            ets:insert(storages, #storage{url = Url, pid = From, ref = Ref}),
            % Update handled chunks
            add_storage_to_chunks(ChunkIds, Url)
    end,
    {noreply, State};

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
    ets:match_delete(storages, #storage{pid=Pid, _='_'}),
    remove_storage_from_chunks(all, Pid),
    {noreply, State};
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

%%--------------------------------------------------------------------
%% @doc
%% Adds the Storage (URL) in the list of storages for each chunk in Chunklist
%%
%% @spec add_storage_to_chunks(AllChunks, ChunksInThisStore, Storage) -> list()
%% @end
%%--------------------------------------------------------------------

add_storage_to_chunks([], _StorageUrl) ->
    [];
add_storage_to_chunks([ChunkId | R], StorageUrl) ->
    case ets:lookup(chunks, ChunkId) of
        [#chunk{storages = S}] ->
            case lists:member(StorageUrl, S) of
                true -> %already member
                    ok;
                false ->
                    ets:update_element(chunks, ChunkId,
                                       {#chunk.storages, S ++ [StorageUrl]})
            end;
        [] ->
            ets:insert(chunks, #chunk{id=ChunkId, storages = [StorageUrl]})
    end,
    add_storage_to_chunks(R, StorageUrl).

remove_storage_from_chunks([], _StorageUrl) ->
    [];
remove_storage_from_chunks(all, StorageUrl) ->
    Chunks = ets:tab2list(chunks),
    lists:map(
      fun(#chunk{id=ChunkId, storages=S}) ->
              case lists:member(StorageUrl, S) of
                  true ->
                      S2= lists:delete(StorageUrl, S),
                      ets:update_element(chunks, ChunkId,
                                         {#chunk.storages, S2});
                  false ->
                       ok
              end
      end, Chunks);
remove_storage_from_chunks([ChunkId | R], StorageUrl) ->
    case ets:lookup(chunks, ChunkId) of
        [#chunk{storages = S}] ->
            Storages = lists:delete(StorageUrl, S),
            ets:update_element(chunks, ChunkId,
                               {#chunk.storages, Storages});
        [] ->
            ok
    end,
    remove_storage_from_chunks(R, StorageUrl).

delete_unused_chunks() ->
    ets:match_delete(chunks, #chunk{id='$1', ratio=0, storages=[], _='_'}).

choose_storage([]) ->
    not_found;
choose_storage([StorageUrl | Rest]) ->
    case ets:lookup(storages, StorageUrl) of
        [S = #storage{}] -> S;
        [] ->
            % Storage not connected
            choose_storage(Rest)
    end.

choose_write_storage([]) -> no_storage;
choose_write_storage(Storages) ->    lists:nth(random:uniform(length(Storages)), Storages).

write_to_one([], _ChunkId, _Data) ->
    {error, failed_to_write};
write_to_one(Storages, ChunkId, Data) ->
    case choose_write_storage(Storages) of
        no_storage -> {error, failed_to_write};
        S ->
            case gen_server:call(S#storage.pid, {write, ChunkId, Data}) of
                ok ->
                    {ok, S};
                _ ->
                    % Failed to store data, try store on next
                    S2 = lists:delete(S, Storages),
                    write_to_one(S2, ChunkId, Data)
            end
    end.


read_async_int([], _ChunkId, Fun, _Error) ->
    Fun({error, enoent});
read_async_int([StorageUrl | R], ChunkId, Fun, _) -> 
    case ets:lookup(storages, StorageUrl) of
	[#storage{pid=Pid}] ->
	    %% Send a read cast to that storage and return
	    %% (the storage will reply with data or error)
	    ErrorFun = fun(Error) -> read_async_int(R, ChunkId, Fun, Error) end,
	    gen_server:cast(Pid, {read_async, ChunkId, Fun, ErrorFun});
	_Else ->
	    read_async_int(R, ChunkId, Fun, {error, {storage_not_found, StorageUrl}})
    end.
