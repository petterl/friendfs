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

-include_lib("friendfs/include/friendfs.hrl").

%% Filesystem API
-export([init/1,
	 list/2,
	 read/4,
	 write/4,
	 flush/2,
	 delete/3,
	 make_dir/4,
	 lookup/2,
	 find/3,
	 get_config/1,
	 get_stats/1,
	 create/6]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List all files
%%
%% @spec
%%   list(Name, InodeI) -> [Storage]
%% @end
%%--------------------------------------------------------------------
list(FsName,INodeI) ->
    Tid = ffs_config:read({fs_tid, FsName}),
    Links = ffs_fat:list(Tid, INodeI),
    lists:map(fun(#ffs_link{ name = Name, to = To }) ->
		      {Name,ffs_fat:lookup(Tid, To)}
	      end,Links).    

%%--------------------------------------------------------------------
%% @doc
%% Read a chunk from a storage
%%
%% @spec
%%   read(FsName,Inode,Size,Offset) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
read(FsName, InodeI, Size, Offset) ->
    Tid = ffs_config:read({fs_tid, FsName}),
    {NewOffset,Chunks} = ffs_fat:read(Tid,InodeI,Size,Offset),
    case read_chunks(Chunks) of
	{ok,<<_Head:NewOffset/binary,Data:Size/binary,_Rest/binary>>} ->
	    {ok,Data};
	{error,Reason} ->
	    {error,Reason}
    end.
	
%%--------------------------------------------------------------------
%% @doc
%% Write a chunk to storages
%%
%% @spec
%%   write(FsName, Inode, Data, Offset) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
write(FsName, InodeI, Data, Offset) ->
    Tid = ffs_config:read({fs_tid, FsName}),
    Config = ffs_config:read({fs_config, FsName}),
    case write_cache(Tid, InodeI, Data, Offset, Config) of
	[] ->
	    ok;
	Chunks when is_list(Chunks) ->
	    update_chunks(Chunks, Tid, Config)
    end.

update_chunks([{append_chunk, ChunkData} | R], Tid, Config) ->
	ChunkId = ffs_lib:get_chunkid(ChunkData),
    store_chunk(ChunkId,ChunkData,Config),
    update_chunks(R, Tid, Config);
update_chunks([{update_chunk, #ffs_chunk{ chunkid = OldChunkId,
 										  id = Id }, ChunkData} | R], 
		Tid, Config) ->
	NewChunkId = ffs_lib:get_chunkid(ChunkData),
	store_chunk(NewChunkId,ChunkData,Config),
    delete_chunk(OldChunkId, Config),
    update_chunks(R, Tid, Config);
update_chunks([],_Tid, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   write_cache(Tid,InodeI,NewData,Offset) -> [{chunk,ChunkId,Data}] | more
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      NewData = binary()
%%      Offset = integer()
%%      ChunkId = string()
%%      Data = binary()
%% @end
%%--------------------------------------------------------------------
write_cache(Tid,InodeI,Data,Offset,Config) ->
    write_cache(Tid,InodeI,Data,Offset,Config,[]).
write_cache(Tid,InodeI,Data,Offset,Config,Acc) ->
    ChunkSize = ffs_lib:get_value(chunk_size,Config),
    {NewOffset,Chunks} = ffs_fat:read(Tid,InodeI,size(Data),Offset),
	Inode = ffs_fat:lookup(Tid,InodeI),
	{ok,FirstChunkData} = get_chunkdata(Inode#ffs_inode.write_cache,Chunks),
	<<Head:NewOffset/binary,FirstChunkDataTail/binary>> = <<FirstChunkData/binary>>,
	{NewCache,Actions} = write_cache(Head,FirstChunkDataTail,NewOffset,Data,Chunks,Inode,Tid,ChunkSize,[]),
	ffs_fat:write_cache(Tid,InodeI,NewCache),
	Actions.

%% When we have reached the last chunk which needs to be modified
write_cache(<<Head/binary>>,<<Rest/binary>>,_Offset,<<Data/binary>>,
			Chunks,Inode,Tid,ChunkSize,Acc) 
		when size(Data) =< size(Rest); (size(Data) div ChunkSize) == 0 ->
	DataSize = size(Data),
	RestData = case Rest of
					<<_Pre:DataSize/binary,T/binary>> ->
						T;
					_Else ->
						<<"">>
			end,
			
	CachedChunkId = if 
						Chunks == [] -> -1; 
						true -> (hd(Chunks))#ffs_chunk.id
					end,
			
	{{CachedChunkId,<<Head/binary,Data/binary,RestData/binary>>},
	 lists:reverse(Acc)};
%% When this and more chunks have to be modified
write_cache(<<Head/binary>>,<<_Rest/binary>>,Offset,WholeData,
			Chunks,Inode,Tid,ChunkSize,Acc) ->
	DataSize = get_chunksize(Chunks,ChunkSize) - Offset,

	<<Data:DataSize/binary,RestData/binary>> = <<WholeData/binary>>,
	
	NewChunkData = <<Head/binary,Data/binary>>,
	
	{ok,NextChunkData} = get_chunkdata(undefined,safe_tl(Chunks)),
	
	ChunkUpdate = chunk_update(Chunks,NewChunkData),
	
	write_cache(<<>>,NextChunkData,0,RestData,safe_tl(Chunks),
				Inode,Tid,ChunkSize,
				ChunkUpdate++Acc).

get_chunksize([#ffs_chunk{ size = Size }|_T],_) ->
	Size;
get_chunksize(_,ChunkSize) ->
	ChunkSize.

chunk_update([],Data) ->
	[{append_chunk,Data}];
chunk_update([Chunk|_T],Data) ->
	[{update_chunk,Chunk,Data}].
	
safe_tl([]) ->
	[];
safe_tl([_|T]) ->
	T.
	
get_chunkdata({Id,Data},[#ffs_chunk{ id = Id }|_T]) ->
	{ok,Data};
get_chunkdata({_Id,Data},[]) ->
	{ok,Data};
get_chunkdata(_,[Chunk|_]) ->
	{ok,read_chunks([Chunk])};
get_chunkdata(_,_) ->
	{ok,<<"">>}.

%%--------------------------------------------------------------------
%% @doc
%% Write a chunk to storages
%%
%% @spec
%%   flush(Name, InodeI) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
flush(FsName, InodeI ) ->
    Tid = ffs_config:read({fs_tid, FsName}),
    Config = ffs_config:read({fs_config, FsName}),
    case flush_cache( Tid, InodeI) of
	[] ->
	    ok;
	Chunks ->
	    update_chunks(Chunks, Tid, Config)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Description	
%%
%% @spec
%%   flush_cache(Tid,InodeI) -> {chunk,ChunkId,Data} | empty
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%% @end
%%--------------------------------------------------------------------
flush_cache(Tid,InodeI) when is_integer(InodeI) ->
    flush_cache(Tid,ffs_fat:lookup(Tid,InodeI));
flush_cache(_Tid,#ffs_inode{ write_cache = undefined }) ->
    empty;
flush_cache(Tid,#ffs_inode{ inode = INodeI, write_cache = Data } = Inode) ->
    {M,F} = ffs_lib:get_value(chunkid_mfa,Tid#ffs_tid.config),
    ChunkId = M:F(Data),
    {add_chunk,ChunkId,Data};
flush_cache(_Tid,Else) ->
    Else.

%%--------------------------------------------------------------------
%% @doc
%% create a new chunk
%%
%% @spec
%%  create(Name, Parent, Name, Uid, Gid, Mode) -> ok | {error, Error}	
%% @end
%%--------------------------------------------------------------------
create(FsName, ParentI, Name, Uid, Gid, Mode) ->
    Tid = ffs_config:read({fs_tid, FsName}),
    ffs_fat:create(Tid,ParentI,Name,Uid,Gid,Mode,0,0).

%%--------------------------------------------------------------------
%% @doc
%% Delete a data path from all storages.
%%
%% @spec
%%   delete(Name,ParentI, Name) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
delete(FsName, ParentI,Name) ->
    Tid = ffs_config:read({fs_tid, FsName}),
    case ffs_fat:unlink(Tid,ParentI,Name) of
	{delete,Inode} ->
	    [ffs_chunk_server:delete(ChunkId) ||
		ChunkId <- Inode#ffs_inode.chunks];
	_Else ->
	    ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Create a new directory at the given path
%%
%% @spec
%%   make_dir(Name,Path) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
make_dir(FsName, ParentInodeI, Name, Mode) ->
    Tid = ffs_config:read({fs_tid, FsName}),
    Parent = ffs_fat:lookup(Tid,ParentInodeI),
    
    #ffs_inode{ gid = Gid, uid = Uid} = Parent,
    
    ffs_fat:make_dir(Tid, ParentInodeI, Name, Uid, Gid, Mode).


%%--------------------------------------------------------------------
%% @doc
%% Get information about the node.
%%
%% @spec
%%   lookup(Name,Inode) -> #ffs_node{} | {error, Error}
%% @end
%%--------------------------------------------------------------------
lookup(FsName,InodeI) ->
    Tid = ffs_config:read({fs_tid, FsName}),
    ffs_fat:lookup(Tid, InodeI).


%%--------------------------------------------------------------------
%% @doc
%% Get information about the node in the given path.
%%
%% @spec
%%   find(Name,Inode,Path) -> #ffs_node{} | {error, Error}
%% @end
%%--------------------------------------------------------------------
find(FsName,ParentInodeI,Path) ->
    Tid = ffs_config:read({fs_tid, FsName}),
    case ffs_fat:find(Tid, ParentInodeI, Path) of
	enoent -> enoent;
	InodeI -> ffs_fat:lookup(Tid, InodeI)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get config values of the filesystem
%%
%% @spec
%%   get_config(Name) -> prop_list() | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_config(Name) ->
    ffs_config:read({fs_config,Name}).

%%--------------------------------------------------------------------
%% @doc
%% Get statistics about the filesystem
%%
%% @spec
%%   get_stats(Name) -> prop_list() | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_stats(Name) ->
    ffs_config:read({fs_stats,Name}).

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
init(Name) ->
    Stats = [{total_mem,0},
	     {free_mem,0},
	     {free_inodes,1 bsl 32 - 1}],
    ffs_config:write({fs_stats, Name}, Stats), 
    
    Config = [{block_size,512},
	      {inode_limit,1 bsl 32},
	      {filesystem_id,1},
	      {chunk_size,1 bsl 3}, %% 8 B
	      {mnt_opts,0},
	      {max_filename_size,36#sup},
              {uid,-1},
              {gid,-1},
              {mode,755}],
    ffs_config:write({fs_config, Name}, Config), 
    
    Tid = ffs_fat:init(Name,
		       ffs_lib:get_value(uid,Config),
		       ffs_lib:get_value(gid,Config),
		       ffs_lib:get_value(mode,Config)),
    ffs_config:write({fs_tid, Name}, Tid), 
    ok.

%%%===================================================================
%%% Internal functions
%%%==================================================================

store_chunk(ChunkId,Data,_Config) ->
    io:format("Storing ~p\n",[ChunkId]),
    %ffs_chunk_server:write(ChunkId,Data),
    ok.

delete_chunk(ChunkId,_Config) ->
	io:format("Deleting ~p\n",[ChunkId]),
    ok.

read_chunks(Chunks) ->
    Data = ffs_lib:pmap(fun(#ffs_chunk{ chunkid = ChunkId }) ->
				ffs_chunk_server:read(ChunkId)
			end,Chunks),
    read_chunks(Data,<<>>).

read_chunks([{ok,<<Data/binary>>}|T],<<Acc/binary>>) ->
    read_chunks(T,<<Acc/binary,Data/binary>>);
read_chunks([{error,Reason}],_) ->
    {error,Reason};
read_chunks([],Acc) ->
    Acc.

