%%%-------------------------------------------------------------------
%%% @author Lukas Larsson <garazdawi@gmail.com>
%%% @doc
%%%   Filesystem
%%%
%%% The brains of friendsfs. It manages a FAT table of the filesystem.
%%%
%%%
%%% @end
%%% Created : 10 Aug 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_filesystem).

-behaviour(gen_server).

-include_lib("friendfs/include/friendfs.hrl").

%% API
-export([start_link/2]).

%% Filesystem API
-export([list/2,read/3, write/4, flush/2, delete/3, make_dir/2, lookup/2, find/3,
	 get_config/1,get_stats/1,create/6]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name, fat, stats, config}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Name,Args) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name,Args) ->
    gen_server:start_link({local, Name}, ?MODULE, [#state{name = Name},Args], []).


%%--------------------------------------------------------------------
%% @doc
%% List all files
%%
%% @spec
%% list(Name, InodeI) -> [Storage]
%% @end
%%--------------------------------------------------------------------
list(Name,INodeI) ->
    gen_server:call(Name, {list, INodeI}).


%%--------------------------------------------------------------------
%% @doc
%% Read a chunk from a storage
%%
%% @spec
%%   read(Name,Path,Offset) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
read(Name,Path, Offset) ->
    gen_server:call(Name, {read, Path, Offset}).

%%--------------------------------------------------------------------
%% @doc
%% Write a chunk to storages
%%
%% @spec
%%   write(Name, Inode, Path, Data) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
write(SrvName, InodeI, Data, Offset) ->
    gen_server:call(SrvName, {write, InodeI, Data, Offset}).

%%--------------------------------------------------------------------
%% @doc
%% Write a chunk to storages
%%
%% @spec
%%   flush(Name, InodeI) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
flush(SrvName, InodeI ) ->
    gen_server:call(SrvName, {flush, InodeI}).

%%--------------------------------------------------------------------
%% @doc
%% create a new chunk
%%
%% @spec
%%  create(Name, Parent, Name, Uid, Gid, Mode) -> ok | {error, Error}	
%% @end
%%--------------------------------------------------------------------
create(SrvName, ParentI,Name,Uid,Gid,Mode) ->
    gen_server:call(SrvName, {create,ParentI,Name,Uid,Gid,Mode}).



%%--------------------------------------------------------------------
%% @doc
%% Delete a data path from all storages.
%%
%% @spec
%%   delete(Name,ParentI, Name) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
delete(SrvName, ParentI,Name) ->
    gen_server:call(SrvName, {delete, ParentI, Name}).


%%--------------------------------------------------------------------
%% @doc
%% Create a new directory at the given path
%%
%% @spec
%%   make_dir(Name,Path) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
make_dir(Name, Path) ->
    gen_server:call(Name, {make_dir, Path}).

%%--------------------------------------------------------------------
%% @doc
%% Get information about the node.
%%
%% @spec
%%   lookup(Name,Inode) -> #ffs_node{} | {error, Error}
%% @end
%%--------------------------------------------------------------------
lookup(Name,Inode) ->
    gen_server:call(Name, {lookup, Inode}).


%%--------------------------------------------------------------------
%% @doc
%% Get information about the node in the given path.
%%
%% @spec
%%   find(Name,Inode,Path) -> #ffs_node{} | {error, Error}
%% @end
%%--------------------------------------------------------------------
find(Name,Inode,Path) ->
    gen_server:call(Name, {find, Inode, Path}).

%%--------------------------------------------------------------------
%% @doc
%% Get config values of the filesystem
%%
%% @spec
%%   get_config(Name) -> prop_list() | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_config(Name) ->
    gen_server:call(Name, get_config).

%%--------------------------------------------------------------------
%% @doc
%% Get statistics about the filesystem
%%
%% @spec
%%   get_stats(Name) -> prop_list() | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_stats(Name) ->
    gen_server:call(Name, get_stats).

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
    process_flag(trap_exit,true),
	Stats = [{total_mem,0},
			 {free_mem,0},
    		 {free_inodes,1 bsl 32 - 1}],

	Config = [{block_size,512},
    		  {inode_limit,1 bsl 32},
    		  {filesystem_id,1},
    		  {chunk_size,1 bsl 15}, %% 32 kB
    		  {mnt_opts,0},
    		  {max_filename_size,36#sup},
              {uid,0},
              {gid,0},
              {mode,755}],

    Tid = ffs_fat:init(State#state.name,
					   ffs_lib:get_value(chunk_size,Config),
					   ffs_lib:get_value(uid,Config),
					   ffs_lib:get_value(gid,Config),
					   ffs_lib:get_value(mode,Config)),

    NewState = State#state{ fat = Tid, stats = Stats, config = Config },
    {ok, NewState}.

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
handle_call({list, InodeI}, _From, State) ->
	Links = ffs_fat:list(State#state.fat, InodeI),
	List = lists:map(fun(#ffs_link{ name = Name, to = To }) ->
					{Name,ffs_fat:lookup(State#state.fat,To)}
		end,Links),
    {reply, List , State};
handle_call({lookup, Inode}, _From, State) ->
    {reply, ffs_fat:lookup(State#state.fat, Inode), State};
handle_call({find, ParentInodeI, Path}, _From, State) ->
    Ret = case ffs_fat:find(State#state.fat, ParentInodeI, Path) of
              enoent -> enoent;
              Inode -> ffs_fat:lookup(State#state.fat, Inode)
          end,
    {reply, Ret, State};
handle_call(get_config, _From, State) ->
    {reply,State#state.config, State};
handle_call(get_stats, _From, State) ->
    {reply,State#state.stats, State};
handle_call({create,ParentI,Name,Uid,Gid,Mode}, _From, State) ->
	NewInode = ffs_fat:create(State#state.fat,ParentI,Name,Uid,Gid,Mode,0,0),
	{reply,NewInode,State};
handle_call({delete,ParentI,Name}, _From, State) ->
	ffs_fat:unlink(State#state.fat,ParentI,Name),
	{reply,ok,State};
handle_call({write,InodeI,Data,Offset}, _From, State) ->
	case ffs_fat:write_cache(State#state.fat,InodeI,Data,Offset) of
		[] ->
			ok;
 		Chunks ->
			[store_chunk(ChunkId,ChunkData,State#state.config) || {chunk,ChunkId,ChunkData} <- Chunks]
	end,
	{reply,ok,State};
handle_call({flush,InodeI}, _From, State) ->
	case ffs_fat:flush_cache(State#state.fat,InodeI) of
		{chunk,ChunkId,ChunkData} ->
			store_chunk(ChunkId,ChunkData,State#state.config);
		_GetMore ->
			ok
	end,
	{reply,ok,State};
	
%% Old code
%% handle_call({read_node_info,Path}, _From, #state{ fat = TabName} = State) ->
%%     case read_node_info_ets(TabName,Path) of
%% 	enoent ->
%% 	    {reply,{error,enoent},State};
%% 	Inode ->
%% 	    {reply,hd(ets:lookup(TabName,Inode)),State}
%%     end;
%% handle_call({read, Path, Offset}, From, State) ->
%%     Storage = choose_storage(Path, State#state.storages),
%% 	if
%% 		Storage == enoent ->
%% 			{reply,enoent,State};
%% 		true ->
%%     		Pid = Storage#storage.pid,
%% 		EncPath = encrypt_path(Path,file),
%%     		gen_server:cast(Pid, {read, From, EncPath, Offset}),
%%     		{noreply, State}
%% 	end;
%% handle_call({write, Path, Data}, From, State) ->
%%     Storage = hd(State#state.storages),
%%     Pid = Storage#storage.pid,
%%     EncPath = encrypt_path(Path,file),
%%     gen_server:cast(Pid,{write, From, EncPath, Data}),
%%     NewStorage = Storage#storage{
%% 		   filelist = lists:usort([Path|Storage#storage.filelist])},
%%     {noreply, State#state{ storages = lists:keyreplace(Pid,#storage.pid,State#state.storages,NewStorage)}};
%% handle_call({make_dir, Path}, _From, State) ->
%%     Storage = hd(State#state.storages),
%%     Pid = Storage#storage.pid,
%%     EncPath = encrypt_path(Path,dir),
%%     case gen_server:call(Pid,{write, EncPath, <<"">>}) of
%% 	ok ->
%% 	    make_dir_ets(State#state.fat,Path,Storage),
%% 	    {reply,ok,State};
%% 	{error,Reason} ->
%% 	    {reply,{error,Reason},State}
%%     end;
%% handle_call({delete, Path}, _From, State) ->
%%     New = lists:map(
%% 	    fun(#storage{pid = Pid, filelist = FL} = S) ->
%% 		    case lists:member(Path,FL) of
%% 			true ->
%% 			    EncPath = encrypt_path(Path,file),
%% 			    gen_server:call(Pid,{delete,EncPath}),
%% 			    S#storage{ filelist = S#storage.filelist -- [Path]};
%% 			false ->
%% 			    S
%% 		    end
%% 	    end,State#state.storages),
%%     {reply, ok, State#state{storages = New}};
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
handle_cast(_Msg, State) ->
    io:format("~p: Unknown handle_cast(~p,~p) call\n",[?MODULE,_Msg,State]),
    {noreply, State}.

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
    io:format("~p: Unknown handle_info(~p,~p) call\n",[?MODULE,_Info,State]),
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

store_chunk(ChunkId,Data,_Config) ->
	io:format("Storing ~p\n",[ChunkId]),
	ffs_chunk_server:write(ChunkId,Data),
	ok.

%% choose_storage(Path, [#storage{ filelist = FileList} = Storage| Rest]) ->
%% 	case lists:member(Path, FileList) of
%% 		true ->
%% 			Storage;
%% 		false ->
%% 			choose_storage(Path,Rest)
%% 	end;
%% choose_storage(_Path, []) ->
%% 	enoent.


%% read_node_info_ets(TabName,Path) ->
%%     Dir = filename:dirname(Path),
%%     Name = filename:basename(Path),
%%     find_inode(TabName,Dir,Name).

%% make_dir_ets(TabName,NewDirPath,Storage) ->
%%     NewName = filename:basename(NewDirPath),
%%     ParentPath = filename:dirname(NewDirPath),
%%     ParentName = filename:basename(ParentPath),
%%     Path = filename:dirname(ParentPath),
%%     case find_inode(TabName,Path,ParentName) of
%% 	enoent ->
%% 	    enoent;
%% 	ParentInode ->
%% 	    ffs_fat:make_dir(TabName,ParentInode,NewName,default,default,default)
%%     end.

%% create_file_ets(TabName,NewFilePath,Hash,Storage) ->
%%     ok.

%% find_inode(TabName,Path,Name) ->
%%   ffs_fat:find(TabName,Path).


%% encrypt_path(String,file) ->
%%     re:replace(String,"/","-",[global,{return,list}])++"?file.ffs";
%% encrypt_path(String,dir) ->
%%     re:replace(String,"/","-",[global,{return,list}])++"?dir.ffs".

%% decrypt_path(String) ->
%%     Stripped = lists:reverse(filename:rootname(String)),
%%     case re:split(Stripped,"[?]",[{return,list}]) of
%% 	["rid",Path] ->
%% 	    {dir,re:replace(lists:reverse(Path),"-","/",[global,{return,list}])};
%% 	["elif",Path] ->
%% 	    {file,re:replace(lists:reverse(Path),"-","/",[global,{return,list}])}
%%     end.


%% insert_dir(TabName,root,Path,Name,Store) ->
%%     insert_dir(TabName,1,none,Path,Name,[],Store),
%%     1;
%% insert_dir(TabName,Parent,Path,Name,Store) ->
%%     Inode = ets:last(TabName)+1,
%%     [Node] = ets:lookup(TabName,Parent),

%%     Data = Node#ffs_node.data,
%%     NewChildren = [Inode|Data#ffs_dir.children],

%%     NewNode = Node#ffs_node{ data = Data#ffs_dir{ children = NewChildren }},

%%     insert_dir(TabName,Inode,Parent,Path,Name,[],Store),
%%     update_node(TabName,NewNode),
%%     Inode.


%% insert_dir(TabName,Inode,Parent,Path,Name,Children,Store) ->
%%     ets:insert(TabName,#ffs_node{ inode = Inode,
%% 				  path = Path,
%% 				  parent = Parent,
%% 				  name = Name,
%% 				  data = #ffs_dir{ children = Children },
%% 				  stores = [Store]}).

%% update_node(TabName,Node) ->
%%     ets:insert(TabName,Node).


%% update_ets(Data,Store,TabName) ->
%%     insert_dirs(Data,Store,TabName),
%%     insert_files(Data,Store,TabName).

%% insert_dirs([H|T],Parent,Path,#ffs_dir{ children = Children },Store,TabName) ->
%%     case find_node(TabName,H,Children) of
%% 	node_not_found ->
%% 	    NextParent = insert_dir(TabName,Parent,Path,H,Store),
%% 	    insert_dirs(T,NextParent,Path++H,
%% 		       ets:lookup_element(TabName,NextParent,#ffs_node.data),Store,TabName);
%% 	#ffs_node{ inode = NextParent } = Node ->
%% 	    add_store(TabName,Node,Store),
%% 	    insert_dirs(T,NextParent,Path++H,Node#ffs_node.data,Store,TabName)
%%     end;
%% insert_dirs(Post,_Parent,Path,#ffs_file{},_Store,TabName) ->
%%     error_handler:info_report(
%%       io_lib:format("Trying to create ~p where a file exists.",[Path++Post]));
%% insert_dirs([],_Parent,_Path,_dir,_Store,_TabName) ->
%%     ok.

%% add_store(TabName,Node,Store) ->
%%     update_node(TabName,Node#ffs_node{
%% 			  stores = lists:usort([Store|Node#ffs_node.stores])}).

%% find_node(TabName,Name,[H|T]) ->
%%     case ets:lookup(TabName,H) of
%% 	[#ffs_node{ name = Name }  = Node] ->
%% 	    Node;
%% 	_ ->
%% 	    find_node(TabName,Name,T)
%%     end;
%% find_node(_TabName,_Name,[]) ->
%%     node_not_found.



%% insert_dirs([{dir,Path}|T],Store,TabName) ->
%%     insert_dirs(string:tokens(Path,"/"),1,"/",
%% 		ets:lookup_element(TabName,1,#ffs_node.data),Store,TabName),
%%     insert_dirs(T,Store,TabName);
%% insert_dirs([_|T],Store,TabName) ->
%%     insert_dirs(T,Store,TabName);
%% insert_dirs([],_Store,_TabName) ->
%%     ok.

%% insert_files(_,_Store,_TabName) ->
%%     ok.

