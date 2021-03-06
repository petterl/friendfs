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
-include("chunk_server.hrl").
-include("debug.hrl").

%% API
-export([start/1]).

%% Storage API
-export([read/1, write/2, delete/1, delete/2]).
-export([connected_storages/0, update_storage/4]).
-export([register_chunk/3, info/0, replication_level/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(READ_TIMEOUT, 60000). % 60 sec read timeout 

-record(state, {storages = [],  %% Registered storages
                sessions = [],
                replicator
               }).

-record(session, {ref,
                  chunk_id,
                  caller_pid,
                  storage,
                  remaining_storages = [],
                  data,
                  timer_ref,
                  start_time}).

-ifdef(TEST).
-include("ffs_chunk_server.hrl").
-endif.


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
%% Selects a storage and sends the request to that storage, the
%% storage will reply directly to the calling process, or send an
%% error code back to chunk server that will find a new storage or
%% respond with an error to calling process.
%%
%% @spec
%%   read(ChunkId :: chunk_id()) -> {ok, Data} | {error, Reason}
%%     Reason = enoent | atom()
%% @end
%%--------------------------------------------------------------------
read(ChunkId) ->
    gen_server:call(?SERVER, {read, ChunkId}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Create a chunk in storages using data sent to function.
%% Ratio is a a value of how many storages it should atleast be stored on.
%%
%% Data is MD5 checksummed and then stored. If we already have a
%% chunk with the same checksum, it is never stored.
%%
%% @spec
%%   write(binary(), Ratio :: integer()) -> {ok, chunk_id()} | {error, Reason}
%%     Reason = no_storage_avail | atom()
%% @end
%%--------------------------------------------------------------------
write(Data, Ratio) when is_binary(Data)->
    gen_server:call(?SERVER, {write, Ratio, Data}, infinity).


%%--------------------------------------------------------------------
%% @doc
%% Delete a chunk from all storages
%%
%% It actuallt decrements the ref count on the chunk.
%% If ref count is 0 it will be
%% deleted by the chunk replicator when possible.
%%
%% @spec
%%   delete(chunk_id()) -> ok | {error, not_found}
%% @end
%%--------------------------------------------------------------------
delete(ChunkId) ->
    gen_server:call(?SERVER, {delete, ChunkId}).

%%--------------------------------------------------------------------
%% @doc
%% Delete a chunk from a specific storage
%%
%% Called by the replicator to delete from specific storages.
%% Will always return ok, even if nothing was deleted.
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
%%   update_storage(storage_url(), pid(), [chunk_id()], integer()) -> ok
%% @end
%%--------------------------------------------------------------------
update_storage(Url, Pid, ChunkIds, Priority) ->
    gen_server:cast(?SERVER, {update_storage, Pid, Url, ChunkIds, Priority}).

%%--------------------------------------------------------------------
%% @doc
%% Gives a list of all storages that are currently connected
%%
%% @spec
%%   connected_storages() -> [storage_url()]
%% @end
%%--------------------------------------------------------------------
connected_storages() ->
    [ S#storage.url || S <- ets:tab2list(storages)].

%%--------------------------------------------------------------------
%% @doc
%% Register ratio for a chunk in filesystem
%%
%% @spec
%%   register_chunk(FsName::string(), chunk_id(), Ratio) -> ok | {error, Reason}
%%     Ratio = integer()
%%     Reason = atom()
%% @end
%%--------------------------------------------------------------------
register_chunk(FsName, ChunkId, Ratio) ->
    gen_server:call(?SERVER, {register_chunk, ChunkId, Ratio, FsName}).

replication_level(ChunkId) ->
    case ets:lookup(chunks, ChunkId) of
        [#chunk{storages = S}] ->
            length(S);
        [] -> {error, not_found}
    end.

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
init(Config) ->
    {ok, Pid} = ffs_chunk_replicator:start_link(Config),
    ets:new(storages, [set,protected,named_table,{keypos, 2}]),
    ets:new(chunks, [set,protected,named_table,{keypos, 2}]),
    {ok, #state{replicator = Pid}}.

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
    %% Find Chunk information from ChunkId
    case ets:lookup(chunks, ChunkId) of
        [#chunk{storages = StoragePids}] ->
	    %% Get the prefered storage to fetch from
            case select_read_storage(StoragePids) of
                #storage{pid = Pid}->
                    Ref = make_ref(),
                    TRef = read_with_timer(Pid, ChunkId, From, Ref),
		    %% Update session with new timer and fewer urls
                    StoragePids2 = StoragePids -- [Pid],
                    State2 = save_session(Ref, ChunkId, [], From, Pid,
                                          StoragePids2, TRef, State),
                    {noreply, State2};
                not_found ->
                    {reply, {error, enoent}, State}
            end;
        [] ->
            {reply, {error, enoent}, State}
    end;

handle_call({ write, Ratio, Data}, From, State) ->
    %% Generate chunkid from Data
    ChunkId = ffs_lib:get_chunkid(Data),
    %% Find Chunk from ChunkId
    case ets:lookup(chunks, ChunkId) of
        [#chunk{storages=StoragePids, ratio=Ratio0}]
        when Ratio == -1,
        Ratio0 =< length(StoragePids) ->
            %% Replication call, but enough storages
            {reply, {error, not_needed}, State};
        [#chunk{storages=StoragePids}]
        when Ratio == -1 ->
            % We already have chunk, this is a replication call
            AllStorages = connected_storages_int(),
            case select_write_storage(lists:filter(
                   fun(#storage{pid=Pid}) ->
                           not lists:member(Pid, StoragePids)
                   end, AllStorages)) of
                no_storage ->
                    % Replication requested but not possible
                    {reply, {error, not_enough_storages}, State};
                #storage{url = Url, pid = Pid} ->
                    ?LOG(chunk_replication, 
                         "Replicate ~p to ~p~n", [ChunkId, Url]),
                    % store on one server
                    Ref = make_ref(),
                    TRef = write_with_timer(Pid, ChunkId, Data, From, Ref),
		            %% Update session with new timer and fewer urls
                    StoragePids2 = StoragePids -- [Pid],
                    State2 = save_session(Ref, ChunkId, Data, From, Pid,
                                          StoragePids2, TRef, State),
                    {noreply, State2}
            end;
        [#chunk{ref_cnt=RefCnt, ratio=Ratio0}] ->
            %% Cool we already have that chunk!
            %% Add 1 to refcounter
            ets:update_element(chunks, ChunkId, 
                               [{#chunk.ref_cnt, RefCnt+1},
                                {#chunk.ratio, erlang:max(Ratio0, Ratio)}]),
            {reply, {ok, ChunkId}, State};
        [] -> 
            StoragePids = connected_storages_int(),
            % Select best server to store on
            case select_write_storage(StoragePids) of
                #storage{pid = Pid} ->
 		            %% store on one server
                    Ref = make_ref(),
                    TRef = write_with_timer(Pid, ChunkId, Data, From, Ref),
		            %% Update session with new timer and fewer urls
                    StoragePids2 = StoragePids -- [Pid],
                    State2 = save_session(Ref, ChunkId, Data, From, Pid,
                                          StoragePids2, TRef, State),
		    ets:insert(chunks, #chunk{id=ChunkId, 
					      ratio=Ratio,
					      ref_cnt=1,
					      storages=[]}),
                    {noreply, State2};
		no_storage ->
		    {reply, {error, no_storage_avail}, State}
	    end
    end;

handle_call({ delete, ChunkId}, _From, State) ->
    Res = case ets:lookup(chunks, ChunkId) of
              [#chunk{ref_cnt=RefCnt}] when RefCnt =< 0 ->
                  % Refcount 0 no user of file
                  {error, not_found};
              [#chunk{ref_cnt=RefCnt}] ->
                  ets:update_element(chunks, ChunkId, 
                                     {#chunk.ref_cnt, RefCnt-1}),
                  ffs_chunk_replicator:refresh(),
                  ok;
              [] ->
                  % Chunk not found, no user of file.
                  {error, not_found}
          end,
    {reply, Res, State};

handle_call({ register_chunk, ChunkId, Ratio, _FsName}, _From, State) ->
    Res =
	case ets:lookup(chunks, ChunkId) of
	    [#chunk{ref_cnt = RefCnt}] ->
            ets:update_element(chunks, ChunkId, [{#chunk.ratio, Ratio},
                                                 {#chunk.ref_cnt, RefCnt+1}]);
	    [] ->
            ets:insert(chunks, #chunk{id=ChunkId, ratio=Ratio,
                                      ref_cnt = 1})
	end,
    ffs_chunk_replicator:refresh(),
    {reply, Res, State};

handle_call( info, _From, State) ->
    Res = {{storages, ets:tab2list(storages)},
           {chunks, ets:tab2list(chunks)}},
    {reply, Res, State};

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
handle_cast({ read_callback, Ref, {ok, Speed}}, State) ->
    % Stop timer
    stop_timer(Ref, State),
    ?DBG("Read with ~pb/s\n", [Speed]),
    Pid = get_storage(Ref, State),
    update_speed(read, Speed, Pid),
    {noreply, remove_session(Ref, State)};

handle_cast({ read_callback, Ref, {error, _Err}}, State) ->
    % Stop timer
    stop_timer(Ref, State),

    % Select new storage
    StoragePids = get_storages(Ref, State),
    FromPid = get_caller(Ref, State),
    case select_read_storage(StoragePids) of
        #storage{pid = Pid} ->
            ChunkId = get_chunkid(Ref, State),
            TRef = read_with_timer(Pid, ChunkId, FromPid, Ref),

            % Update session with new timer and fewer urls
            StoragePids2 = StoragePids -- [Pid],
            State2 = save_session(Ref, ChunkId, [], FromPid, Pid,
                                  StoragePids2, TRef, State),

            {noreply, State2};
        not_found ->
            %% No storages has the chunk
            gen_server:reply(FromPid, {error, enoent}),
            State1 = remove_session(Ref, State),
            {noreply, State1}
    end;
 
handle_cast({ write_callback, Ref, {ok, Speed}}, State) ->
    % Stop timer
    stop_timer(Ref, State),
    ?DBG("Write: ~pb/s\n", [Speed]),
    ChunkId = get_chunkid(Ref, State),
    Pid = get_storage(Ref, State),
    ets:update_element(chunks, ChunkId,
		       {#chunk.storages, [Pid]}),
    update_speed(write, Speed, Pid),
    ffs_chunk_replicator:refresh(),
    {noreply, remove_session(Ref, State)};

handle_cast({ write_callback, Ref, {error, _Err}}, State) ->
    % Stop timer
    stop_timer(Ref, State),

    % Select new storage
    StorageUrls = get_storages(Ref, State),
    FromPid = get_caller(Ref, State),
    case select_write_storage(StorageUrls) of
        #storage{url = Url, pid = Pid} ->
            ChunkId = get_chunkid(Ref, State),
            Data = get_data(Ref, State),
            TRef = write_with_timer(Pid, ChunkId, Data, FromPid, Ref),
            % Update session with new timer and fewer urls
            StorageUrls2 = StorageUrls -- [Url],
            State2 = save_session(Ref, ChunkId, Data, FromPid, Pid,
                                  StorageUrls2, TRef, State),
            {noreply, State2};
        not_found ->
            %% No storages has the chunk
            gen_server:reply(FromPid, {error, no_storage_avail}),
            State1 = remove_session(Ref, State),
            {noreply, State1}
    end;		

handle_cast({ delete, ChunkId, StorageUrl}, State) ->
    case ets:lookup(chunks, ChunkId) of
        [#chunk{storages = StorageUrls}] ->
            case get_storage(StorageUrl, State) of
                [S = #storage{}] ->
                    gen_server:cast(S#storage.pid, {delete, ChunkId}),
                    ets:update_element(chunks, ChunkId,
                                       {#chunk.storages,
                                        StorageUrls -- [S#storage.url]});
                [] ->
		    %% Error: Storage missing, ignore
                    ok
            end;
        [] ->
            % Error: Chunk missing, ignore
            ok
    end,
    {noreply, State};

handle_cast({update_storage, From, Url, ChunkIds, Priority}, State) ->
    case ets:lookup(storages, Url) of
        [#storage{}] ->
            Updated =
                ets:foldl(
                  fun(#chunk{id = ChunkId, storages=StoragePids}, Acc) ->
                          case lists:member(ChunkId, ChunkIds) of
                              true ->
                                  % Chunk in this storage
                                  case lists:member(From, StoragePids) of
                                      true ->
                                          Acc;
                                      false ->
                                          % Added
                                          ets:update_element(
                                            chunks, ChunkId,
                                            {#chunk.storages,
                                             StoragePids ++ [From]}),
                                          true
                                  end;
                              false ->
                                  % chunk not in storage
                                  case lists:member(From, StoragePids) of
                                      true ->
                                          % Removed
                                          ets:update_element(
                                            chunks, ChunkId,
                                            {#chunk.storages,
                                             StoragePids -- [From]}),
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
            ets:insert(storages, #storage{url = Url, pid = From,
                                          ref = Ref, priority=Priority}),
            % Update handled chunks
            add_storage_to_chunks(ChunkIds, From)
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

save_session(Ref, ChunkId, Data, From, Pid, StorageUrls, Tref,
             #state{sessions = Sessions0} = State) ->
    case lists:keysearch(Ref, #session.ref, Sessions0) of
        {value, #session{} = S} ->
            Sessions = lists:keyreplace(Ref, #session.ref, Sessions0,
                                        S#session{storage = Pid,
                                                  remaining_storages = StorageUrls,
                                                  timer_ref = Tref});
        _ ->
            Session = #session{ref = Ref,
                               chunk_id = ChunkId,
                               caller_pid = From,
                               storage = Pid,
                               data = Data,
                               remaining_storages = StorageUrls,
                               timer_ref = Tref},
            Sessions = [Session | Sessions0]
    end,
    State#state{sessions = Sessions}.

remove_session(Ref, #state{sessions = Sessions}=State) ->
    Sessions2 = lists:keydelete(Ref, #session.ref, Sessions),
    State#state{sessions = Sessions2}.

get_chunkid(Ref, #state{sessions = Sessions}) ->
    case lists:keysearch(Ref, #session.ref, Sessions) of
        {value, #session{chunk_id = Cid}} -> Cid;
        false -> []
    end.

get_data(Ref, #state{sessions = Sessions}) ->
    case lists:keysearch(Ref, #session.ref, Sessions) of
        {value, #session{data = Data}} -> Data;
        false -> <<>>
    end.
 
get_caller(Ref, #state{sessions = Sessions}) ->
    io:format("ref: ~p~ns: ~p~n", [Ref, Sessions]),
    case lists:keysearch(Ref, #session.ref, Sessions) of
        {value, #session{caller_pid = Pid}} -> Pid;
        false -> no_caller
    end.
 
get_storages(Ref, #state{sessions = Sessions}) ->
    case lists:keysearch(Ref, #session.ref, Sessions) of
        {value, #session{remaining_storages = StoreageUrls}} -> StoreageUrls;
        false -> []
    end.

get_storage(Ref, #state{sessions = Sessions}) ->
    case lists:keysearch(Ref, #session.ref, Sessions) of
        {value, #session{storage = Pid}} -> Pid;
        false -> []
    end.


stop_timer(Ref, #state{sessions = Sessions}) ->
    case lists:keysearch(Ref, #session.ref, Sessions) of
        {value, #session{timer_ref = TRef}} ->
            timer:cancel(TRef);
        _ ->
            no_timer
    end.

read_with_timer(Pid, ChunkId, FromPid, Ref) ->
    % Send a read cast to that storage
    % (the storage will reply with data or error)
    gen_server:cast(Pid, {read, ChunkId, FromPid, Ref}),
    % Add timeout callback if storage does not responds in time
    {ok, TRef} =
        timer:apply_after(?READ_TIMEOUT, gen_server, cast,
                          [?MODULE, {read_callback, Ref,
                                     {error, timeout}}]),
    TRef.

write_with_timer(Pid, ChunkId, Data, FromPid, Ref) ->
    % Send a write cast to that storage
    % (the storage will reply with ok or error)
    gen_server:cast(Pid, {write, ChunkId, Data, FromPid, Ref}),
    % Add timeout callback if storage does not responds in time
    {ok, TRef} =
        timer:apply_after(?READ_TIMEOUT, gen_server, cast,
                          [?MODULE, {write_callback, Ref,
                                     {error, timeout}}]),
    TRef.


%% Adds the Storage (URL) to the list of storages for each chunk 
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

%% Remove the Storage (URL) to the list of storages for each chunk 
%% Right now storages refrech with a fill list so first cases will not be used
%% remove_storage_from_chunks([], _StorageUrl) ->
%%    [];
%% remove_storage_from_chunks([ChunkId | R], StorageUrl) ->
%%     case ets:lookup(chunks, ChunkId) of
%%         [#chunk{storages = S}] ->
%%             Storages = lists:delete(StorageUrl, S),
%%             ets:update_element(chunks, ChunkId,
%%                                {#chunk.storages, Storages});
%%         [] ->
%%             ok
%%     end,
%%     remove_storage_from_chunks(R, StorageUrl);
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
      end, Chunks).

%% Selects a read storage to use
select_read_storage([]) ->
    not_found;
select_read_storage([Pid | Rest]) ->
    case ets:match_object(storages, #storage{pid=Pid, _='_'}) of
        [S = #storage{}] -> S;
        [] ->
            % Storage not connected
            select_read_storage(Rest)
    end.

%% Selects a write stroage to use
select_write_storage([]) -> no_storage;
select_write_storage(Storages) ->
    lists:nth(random:uniform(length(Storages)), Storages).

connected_storages_int() ->
    ets:tab2list(storages).

update_speed(write, Speed, Pid) ->
    case ets:match_object(storages, #storage{pid=Pid, _='_'}) of
        [#storage{url=Url}] -> 
            ets:update_element(storages, Url,
                               {#storage.write_speed, Speed});
        [] ->
            % Storage not connected
            ok
    end;
update_speed(read, Speed, Pid) ->
    case ets:match_object(storages, #storage{pid=Pid, _='_'}) of
        [#storage{url=Url}] -> 
            ets:update_element(storages, Url,
                               {#storage.read_speed, Speed});
        [] ->
            % Storage not connected
            ok
    end.
    

    
