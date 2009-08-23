%%%-------------------------------------------------------------------
%%% @author Lukas Larsson <garazdawi@gmail.com>
%%% @doc
%%%   Filesystem
%%%
%%% The brains of friendsfs. It manages all the different stores and
%%% manages a FAT table of the filesystem. 
%%%
%%% @end
%%% Created : 10 Aug 2009 by Petter Larsson <petterl@lysator.liu.se>
%%%-------------------------------------------------------------------
-module(ffs_filesystem).

-behaviour(gen_server).

-include_lib("friendfs/include/friendfs.hrl").

%% API
-export([start_link/2]).

%% Storage API
-export([list/2,read/3, write/3, delete/2, make_dir/2, lookup/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name, fat, storages = []}).

-record(storage, {
	  filelist = [],       % A list of files which exist in this store.
	  storeinfo,
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
%% @spec start_link(Name,Args) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name,Args) ->
    gen_server:start_link({local, Name}, ?MODULE, [#state{name = Name},Args], []).


%%--------------------------------------------------------------------
%% @doc
%% List all configured Storages
%%
%% @spec
%% list(Name) -> [Storage]
%% @end
%%--------------------------------------------------------------------
list(Name,INodeI) ->
    gen_server:call(Name, {list,INodeI}).


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
%% Get information about the node in the given path.
%%
%% @spec
%%   read_node_info(Name,Path) -> #ffs_node{} | {error, Error}
%% @end
%%--------------------------------------------------------------------
lookup(Name,Inode) ->
    gen_server:call(Name, {lookup, Inode}).

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
    Tid = ffs_fat:init(State#state.name),
    NewState = State#state{ fat = Tid },
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
handle_call({list,InodeI}, _From, State) ->
	Links = ffs_fat:list(State#state.fat,InodeI),
	List = lists:map(fun(#ffs_link{ name = Name, to = To }) ->
					{Name,ffs_fat:lookup(State#state.fat,To)}
		end,Links),
    {reply, List , State};
handle_call({lookup,Inode},_From,State) ->
	{reply, ffs_fat:lookup(State#state.fat,Inode),State};
handle_call({read_node_info,Path}, _From, #state{ fat = TabName} = State) ->
    case read_node_info_ets(TabName,Path) of
	enoent ->
	    {reply,{error,enoent},State};
	Inode ->
	    {reply,hd(ets:lookup(TabName,Inode)),State}
    end;
handle_call({read, Path, Offset}, From, State) ->
    Storage = choose_storage(Path, State#state.storages),
	if 
		Storage == enoent ->
			{reply,enoent,State};
		true ->
    		Pid = Storage#storage.pid,
		EncPath = encrypt_path(Path,file),
    		gen_server:cast(Pid, {read, From, EncPath, Offset}),
    		{noreply, State}
	end;
handle_call({write, Path, Data}, From, State) ->
    Storage = hd(State#state.storages),
    Pid = Storage#storage.pid,
    EncPath = encrypt_path(Path,file),
    gen_server:cast(Pid,{write, From, EncPath, Data}),
    NewStorage = Storage#storage{
		   filelist = lists:usort([Path|Storage#storage.filelist])},
    {noreply, State#state{ storages = lists:keyreplace(Pid,#storage.pid,State#state.storages,NewStorage)}};
handle_call({make_dir, Path}, _From, State) ->
    Storage = hd(State#state.storages),
    Pid = Storage#storage.pid,
    EncPath = encrypt_path(Path,dir),
    case gen_server:call(Pid,{write, EncPath, <<"">>}) of
	ok ->
	    make_dir_ets(State#state.fat,Path,Storage),
	    {reply,ok,State};
	{error,Reason} ->
	    {reply,{error,Reason},State}
    end;
handle_call({delete, Path}, _From, State) ->
    New = lists:map(
	    fun(#storage{pid = Pid, filelist = FL} = S) ->
		    case lists:member(Path,FL) of
			true ->
			    EncPath = encrypt_path(Path,file),
			    gen_server:call(Pid,{delete,EncPath}),
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
handle_cast({connect, Pid, Storeinfo, FileList}, State) ->
    error_logger:info_report(
      io_lib:format("A new storage has been connected with ~p",[State#state.name])),
    DecrFileList = [decrypt_path(S) || S <- FileList],
%    update_ets(DecrFileList,Storeinfo, State#state.table),
    NewStore = #storage{ pid = Pid , storeinfo = Storeinfo },
    link(Pid),
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

choose_storage(Path, [#storage{ filelist = FileList} = Storage| Rest]) ->
	case lists:member(Path, FileList) of
		true ->
			Storage;
		false ->
			choose_storage(Path,Rest)
	end;
choose_storage(_Path, []) ->
	enoent.


read_node_info_ets(TabName,Path) ->
    Dir = filename:dirname(Path),
    Name = filename:basename(Path),
    find_inode(TabName,Dir,Name).

make_dir_ets(TabName,NewDirPath,Storage) ->
    NewName = filename:basename(NewDirPath),
    ParentPath = filename:dirname(NewDirPath),
    ParentName = filename:basename(ParentPath),
    Path = filename:dirname(ParentPath),
    case find_inode(TabName,Path,ParentName) of
	enoent ->
	    enoent;
	ParentInode ->
	    ffs_fat:make_dir(TabName,ParentInode,NewName,default,default,default)
    end.

create_file_ets(TabName,NewFilePath,Hash,Storage) ->
    ok.
	
find_inode(TabName,Path,Name) ->
  ffs_fat:find(TabName,Path).


encrypt_path(String,file) ->
    re:replace(String,"/","-",[global,{return,list}])++"?file.ffs";
encrypt_path(String,dir) ->
    re:replace(String,"/","-",[global,{return,list}])++"?dir.ffs".

decrypt_path(String) ->
    Stripped = lists:reverse(filename:rootname(String)),
    case re:split(Stripped,"[?]",[{return,list}]) of
	["rid",Path] ->
	    {dir,re:replace(lists:reverse(Path),"-","/",[global,{return,list}])};
	["elif",Path] ->
	    {file,re:replace(lists:reverse(Path),"-","/",[global,{return,list}])}
    end.


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
    
    
