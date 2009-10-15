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
-export([list/2,
	 read/5,
	 write/4,
	 flush/2,
	 delete/3,
	 make_dir/4,
	 lookup/2,
	 find/3,
	 get_config/1,
	 get_stats/1,
	 create/6]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Filesystem callbacks
-export([read_reply/2]).

-define(SERVER, ?MODULE).

-record(state, {name, fat, stats, config}).

%%%===================================================================
%%% API%%%===================================================================

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
%%   list(Name, InodeI) -> [Storage]
%% @end
%%--------------------------------------------------------------------
list(Name,INodeI) ->
    gen_server:call(Name, {list, INodeI}).


%%--------------------------------------------------------------------
%% @doc
%% Read a chunk from a storage
%%
%% @spec
%%   read(Name,Inode,Size,Offset,ReadCBFun) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
read(Name, Inode, Size, Offset, ReadCBFun) ->
    gen_server:cast(Name, {read, Inode, Size, Offset, ReadCBFun}).

%%--------------------------------------------------------------------
%% @doc
%% Write a chunk to storages
%%
%% @spec
%%   write(Name, Inode, Data, Offset) -> ok | {error, Error}
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
make_dir(SrvName, ParentInodeI, Name, Mode) ->
    gen_server:call(SrvName, {make_dir, ParentInodeI,Name,Mode}).

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
              {uid,-1},
              {gid,-1},
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
    
    case ffs_fat:unlink(State#state.fat,ParentI,Name) of
	{delete,Inode} ->
	    [ffs_chunk_server:delete(ChunkId) ||
		ChunkId <- Inode#ffs_inode.chunkids];
	_Else ->
	    ok
    end,
    
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
handle_call({make_dir, ParentI, Name, Mode}, _From, State) ->

    Parent = ffs_fat:lookup(State#state.fat,ParentI),

    #ffs_inode{ gid = Gid, uid = Uid} = Parent,
    
    NewInode = ffs_fat:make_dir(State#state.fat,ParentI, Name, Uid, Gid, Mode),
    
    {reply,NewInode,State};
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
handle_cast({read,InodeI,Size,Offset,Fun},State) ->
    {NewOffset,ChunkIds} = ffs_fat:read(State#state.fat,InodeI,Size,Offset),

    read_reply({ok,<<>>},{<<>>,ChunkIds,Size,NewOffset,Fun}),
    {noreply,State};
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

read_reply({ok,<<Data/binary>>},{<<Acc/binary>>,[],Size,Offset,Fun}) ->

    %% Remove offset data
    <<_Head:Offset/binary,NewData:Size/binary,_Rest/binary>> = 
	<<Acc/binary,Data/binary>>,
    
    Fun(<<NewData/binary>>);
read_reply({ok,<<Data/binary>>},
	   {<<Acc/binary>>,
	    [{chunk,ChunkId}|T],
	    Size,
	    Offset,
	    Fun}) ->
    NewAcc = <<Acc/binary,Data/binary>>,
    
    ffs_chunk_server:read_async(
      ChunkId,fun(Data) ->
		      read_reply(Data,{NewAcc,T,Size,Offset,Fun})
	      end);
read_reply({error,Error},{_Acc,_Chunks,_Size,_Offset,Fun}) ->
    Fun({error,Error}).



