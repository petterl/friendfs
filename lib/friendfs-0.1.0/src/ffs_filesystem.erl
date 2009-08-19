%%%-------------------------------------------------------------------
%%% @author Petter Larsson <petterl@lysator.liu.se>
%%% @doc
%%%   Storage Manager
%%%
%%% Manages all storages and prioritize storages at read and write
%%% Verify atorage before making it availible
%%%
%%% @end
%%% Created : 10 Aug 2009 by Petter Larsson <petterl@lysator.liu.se>
%%%-------------------------------------------------------------------
-module(ffs_filesystem).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% Storage API
-export([list/1,read/3, write/3, delete/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name, storages = []}).

-record(storage, {
		  filelist = [],       % A list of files which exist in this store.
          pid,                 % Pid of storage process
          priority = 100       % Priority of storage
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
start_link(Name,Args) ->
    gen_server:start_link({local, Name}, ?MODULE, [#state{name = Name},Args], []).


%%--------------------------------------------------------------------
%% @doc
%% List all configured Storages
%%
%% @spec
%% list() -> [Storage]
%% @end
%%--------------------------------------------------------------------
list(Name) ->
    gen_server:call(Name, list).


%%--------------------------------------------------------------------
%% @doc
%% Read a chunk from a storage
%%
%% @spec
%%   read(ChunkId) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
read(Name,Path, Offset) ->
    gen_server:call(Name, {read, Path, Offset}).

%%--------------------------------------------------------------------
%% @doc
%% Write a chunk to storages
%%
%% @spec
%%   write(Name, Path, Data) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
write(Name, Path, Data) ->
    gen_server:call(Name, {write, Path, Data}).


%%--------------------------------------------------------------------
%% @doc
%% Delete a data path from all storages.
%%
%% @spec
%%   delete(Name,Path) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
delete(Name, Path) ->
    gen_server:call(Name, {delete, Path}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([State,_Args]) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(list, _From, State) ->
	AllFiles = lists:flatmap(fun(#storage{filelist = FileList}) ->
								FileList
							end,State#state.storages),
    {reply, lists:usort(AllFiles), State};
handle_call({read, Path, Offset}, From, State) ->
    Storage = choose_storage(Path, State#state.storages),
	if 
		Storage == enoent ->
			{reply,enoent,State};
		true ->
    		Pid = Storage#storage.pid,
    		gen_server:cast(Pid, {read, From, Path, Offset}),
    		{noreply, State}
	end;
handle_call({write, Path, Data}, From, State) ->
	Storage = hd(State#state.storages),
	Pid = Storage#storage.pid,
	gen_server:cast(Pid,{write, From, Path, Data}),
	NewStorage = Storage#storage{ filelist = lists:usort([Path|Storage#storage.filelist])},
    {noreply, State#state{ storages = lists:keyreplace(Pid,#storage.pid,State#state.storages,NewStorage)}};
handle_call({delete, Path}, From, State) ->
	New = lists:map(fun(#storage{pid = Pid, filelist = FL} = S) ->
				case lists:member(Path,FL) of
					true ->
						gen_server:call(Pid,{delete,Path}),
						S#storage{ filelist = S#storage.filelist -- [Path]};
					false ->
						S
				end
			end,State#state.storages),
	{reply, ok, State#state{storages = New}};
handle_call(_Request, _From, State) ->
	
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({connect, Pid, FileList}, State) ->
	error_logger:info_report(io_lib:format("A new storage has been connected with ~p",[State#state.name])),
	NewStore = #storage{ pid = Pid, filelist = FileList},
    {noreply, State#state{ storages = [NewStore|State#state.storages]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
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

choose_storage(Path, [#storage{ filelist = FileList} = Storage| Rest]) ->
	case lists:member(Path, FileList) of
		true ->
			Storage;
		false ->
			choose_storage(Path,Rest)
	end;
choose_storage(_Path, []) ->
	enoent.
