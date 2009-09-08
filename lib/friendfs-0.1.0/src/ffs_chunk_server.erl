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
-module(ffs_storage_mgr).
 
-behaviour(gen_server).
 
%% API
-export([start_link/0]).
 
%% Storage API
-export([list/0, connect/1, disconnect/1]).
-export([read/1, write/2]).
 
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
-define(SERVER, ?MODULE).
 
-record(state, {storages = []}).
 
-record(storage, {
          url,            % storage URL
          pid,            % Pid of storage process
          priority = 100, % Priority of storage
          cids = []       % Chunk IDs in storage
         }).
 
%%%===================================================================
%%% API
%%%===================================================================
 
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
 
%%--------------------------------------------------------------------
%% @doc
%% List all configured Storages
%%
%% @spec
%% list() -> [Storage]
%% @end
%%--------------------------------------------------------------------
list() ->
    gen_server:call(?SERVER, list).
 
%%--------------------------------------------------------------------
%% @doc
%% Connect a new storage
%%
%% @spec
%% connect(StorageUrl) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
connect(StorageUrl) ->
    gen_server:call(?SERVER, {connect, StorageUrl}).
 
%%--------------------------------------------------------------------
%% @doc
%% List all chunks on a specificDisconnect a storage
%%
%% @spec
%% disconnect(Storage) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
disconnect(Storage) ->
    gen_server:call(?SERVER, {disconnect, Storage}).
 
 
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
init([]) ->
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
handle_call(list, _From, State) ->
    Reply = State#state.storages,
    {reply, Reply, State};
handle_call({connect, StorageUrl}, _From, State) ->
    Module = find_module(StorageUrl),
    case gen_server:start_link(Module, StorageUrl) of
        {ok, Pid} ->
            Reply = ok,
            Storage = #storage{url = StorageUrl,
                               pid = Pid},
            State1 = State#state{storages = [Storage | State#state.storages]};
        Error ->
            Reply = Error,
            State1 = State
    end,
    {reply, Reply, State1};
handle_call({disconnect, StorageUrl}, _From, State) ->
    Storage = find_storage(StorageUrl, State#state.storages),
    Pid = Storage#storage.pid,
    Reply = gen_server:cast(Pid, stop),
    {reply, Reply, State};
handle_call({read, ChunkId}, _From, State) ->
    Storage = choose_storage(ChunkId, State#state.storages),
    Pid = Storage#storage.pid,
    Res = gen_server:call(Pid, {read, ChunkId}),
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
 
choose_storage(_ChunkId, [Storage | _Rest]) ->
    Storage.
