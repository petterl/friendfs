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
	 stop/1,
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
    Ctx = ffs_config:read({fs_ctx, FsName}),
    Links = ffs_fat:list(Ctx, INodeI),
    lists:map(fun(#ffs_fs_link{ name = Name, to = To }) ->
		      {Name,ffs_fat:lookup(Ctx, To)}
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
    Ctx = ffs_config:read({fs_ctx, FsName}),
    {NewOffset,Chunks} = ffs_fat:read(Ctx,InodeI,Size,Offset),
    case read_chunks(Chunks) of
	<<_Head:NewOffset/binary,Data:Size/binary,_Rest/binary>> ->
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
    Ctx = ffs_config:read({fs_ctx, FsName}),
    Config = ffs_config:read({fs_config, FsName}),
    case write_cache(Ctx, InodeI, Data, Offset, Config) of
	[] ->
	    ok;
	Chunks when is_list(Chunks) ->
	    update_chunks(Chunks, InodeI, Ctx, Config)
    end.

update_chunks([{append_chunk, ChunkData} | R], InodeI, Ctx, Config) ->
	ChunkPos = length((ffs_fat:lookup(Ctx,InodeI))#ffs_fs_inode.chunks),
    {ok, ChunkId} = store_chunk(ChunkData,Config),
	update_inode(Ctx,InodeI,ChunkPos,ChunkId,size(ChunkData)),
    update_chunks(R, InodeI, Ctx, Config);
update_chunks([{update_chunk, #ffs_fs_chunk{ chunkid = OldChunkId,
 										  id = ChunkPos }, ChunkData} | R], 
		InodeI, Ctx, Config) ->
	{ok, NewChunkId} = store_chunk(ChunkData,Config),
	update_inode(Ctx,InodeI,ChunkPos,NewChunkId,size(ChunkData)),
    delete_chunk(OldChunkId, Config),
    update_chunks(R, InodeI, Ctx, Config);
update_chunks([],_InodeI, _Ctx, _Config) ->
    ok.

update_inode(Ctx,InodeI,ChunkPos,ChunkId,Size) ->
	OldChunks = (ffs_fat:lookup(Ctx,InodeI))#ffs_fs_inode.chunks,
	NewChunk = #ffs_fs_chunk{ id = ChunkPos, size = Size, chunkid = ChunkId},
	ffs_fat:flush_cache(Ctx,InodeI,lists:keystore(ChunkPos,#ffs_fs_chunk.id,OldChunks,NewChunk)).

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   write_cache(Ctx,InodeI,NewData,Offset,Config) -> [{chunk,ChunkId,Data}] | more
%%      Ctx = ffs_ctx()
%%      InodeI = inodei()
%%      NewData = binary()
%%      Offset = integer()
%%      ChunkId = string()
%%      Data = binary()
%% @end
%%--------------------------------------------------------------------
write_cache(Ctx,InodeI,Data,Offset,Config) ->
    ChunkSize = ffs_lib:get_value(chunk_size,Config),
    {NewOffset,Chunks} = ffs_fat:read(Ctx,InodeI,size(Data),Offset),
	Inode = ffs_fat:lookup(Ctx,InodeI),
	{ok,FirstChunkData} = get_chunkdata(Inode#ffs_fs_inode.write_cache,Chunks),
	<<Head:NewOffset/binary,FirstChunkDataTail/binary>> = <<FirstChunkData/binary>>,
	{NewCache,Actions} = write_cache(Head,FirstChunkDataTail,NewOffset,Data,Chunks,Inode,Ctx,ChunkSize,[]),
	ffs_fat:write_cache(Ctx,InodeI,NewCache),
	Actions.

%% When we have reached the last chunk which needs to be modified
write_cache(<<Head/binary>>,<<Rest/binary>>,_Offset,<<Data/binary>>,
			Chunks,_Inode,_Ctx,ChunkSize,Acc) 
		when size(Data) =< size(Rest); (size(Data) div ChunkSize) == 0 ->
	DataSize = size(Data),
	RestData = case Rest of
					<<_Pre:DataSize/binary,T/binary>> ->
						T;
					_Else ->
						<<"">>
			end,
			
	CachedChunkId = if 
						Chunks == [] -> undefined; 
						true -> hd(Chunks)
					end,
			
	{{CachedChunkId,<<Head/binary,Data/binary,RestData/binary>>},
	 lists:reverse(Acc)};
%% When this and more chunks have to be modified
write_cache(<<Head/binary>>,<<_Rest/binary>>,Offset,WholeData,
			Chunks,Inode,Ctx,ChunkSize,Acc) ->
	DataSize = get_chunksize(Chunks,ChunkSize) - Offset,

	<<Data:DataSize/binary,RestData/binary>> = <<WholeData/binary>>,
	
	NewChunkData = <<Head/binary,Data/binary>>,
	
	{ok,NextChunkData} = get_chunkdata(undefined,safe_tl(Chunks)),
	
	ChunkUpdate = chunk_update(Chunks,NewChunkData),
	
	write_cache(<<>>,NextChunkData,0,RestData,safe_tl(Chunks),
				Inode,Ctx,ChunkSize,
				ChunkUpdate++Acc).

get_chunksize([#ffs_fs_chunk{ size = Size }|_T],_) ->
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
	
get_chunkdata({#ffs_fs_chunk{ id = Id },Data},[#ffs_fs_chunk{ id = Id }|_T]) ->
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
    Ctx = ffs_config:read({fs_ctx, FsName}),
    Config = ffs_config:read({fs_config, FsName}),
    case flush_cache( Ctx, InodeI ) of
	[] ->
	    ok;
	Chunks ->
	    update_chunks(Chunks, InodeI, Ctx, Config)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Description	
%%
%% @spec
%%   flush_cache(Ctx,InodeI) -> {chunk,ChunkId,Data} | empty
%%      Ctx = ffs_ctx()
%%      InodeI = inodei()
%% @end
%%--------------------------------------------------------------------
flush_cache(Ctx,InodeI) when is_integer(InodeI) ->
    flush_cache(Ctx,ffs_fat:lookup(Ctx,InodeI));
flush_cache(_Ctx,#ffs_fs_inode{ write_cache = undefined }) ->
    [];
flush_cache(_Ctx,#ffs_fs_inode{ write_cache = {undefined,Data} } ) ->
    [{append_chunk,Data}];
flush_cache(_Ctx,#ffs_fs_inode{ write_cache = {Chunk,Data} } ) ->
    [{update_chunk,Chunk,Data}].

%%--------------------------------------------------------------------
%% @doc
%% create a new chunk
%%
%% @spec
%%  create(Name, Parent, Name, Uid, Gid, Mode) -> ok | {error, Error}	
%% @end
%%--------------------------------------------------------------------
create(FsName, ParentI, Name, Uid, Gid, Mode) ->
    Ctx = ffs_config:read({fs_ctx, FsName}),
    ffs_fat:create(Ctx,ParentI,Name,Uid,Gid,Mode,0,0).

%%--------------------------------------------------------------------
%% @doc
%% Delete a data path from all storages.
%%
%% @spec
%%   delete(Name,ParentI, Name) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
delete(FsName, ParentI,Name) ->
    Ctx = ffs_config:read({fs_ctx, FsName}),
    case ffs_fat:unlink(Ctx,ParentI,Name) of
        {delete,Inode} ->
            [ffs_chunk_server:delete(ChunkId) ||
                ChunkId <- Inode#ffs_fs_inode.chunks];
        _Else ->
            ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Create a new directory at the given path
%%
%% @spec
%%   make_dir(FsName, ParentInodeI, Name, Mode) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
make_dir(FsName, ParentInodeI, Name, Mode) ->
    Ctx = ffs_config:read({fs_ctx, FsName}),
    Parent = ffs_fat:lookup(Ctx,ParentInodeI),
    
    #ffs_fs_inode{ gid = Gid, uid = Uid} = Parent,
    
    ffs_fat:make_dir(Ctx, ParentInodeI, Name, Uid, Gid, Mode).


%%--------------------------------------------------------------------
%% @doc
%% Get information about the node.
%%
%% @spec
%%   lookup(Name,Inode) -> #ffs_node{} | {error, Error}
%% @end
%%--------------------------------------------------------------------
lookup(FsName,InodeI) ->
    Ctx = ffs_config:read({fs_ctx, FsName}),
    ffs_fat:lookup(Ctx, InodeI).


%%--------------------------------------------------------------------
%% @doc
%% Get information about the node in the given path.
%%
%% @spec
%%   find(Name,Inode,Path) -> #ffs_node{} | {error, Error}
%% @end
%%--------------------------------------------------------------------
find(FsName,ParentInodeI,Path) ->
    Ctx = ffs_config:read({fs_ctx, FsName}),
    case ffs_fat:find(Ctx, ParentInodeI, Path) of
	enoent -> enoent;
	InodeI -> ffs_fat:lookup(Ctx, InodeI)
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
	      {chunk_size,1 bsl 16}, %% 8 B
	      {mnt_opts,0},
	      {max_filename_size,36#sup},
              {uid,-1},
              {gid,-1},
              {mode,755}],
    ffs_config:write({fs_config, Name}, Config), 
    
    Ctx = ffs_fat:init(Name,
		       ffs_lib:get_value(uid,Config),
		       ffs_lib:get_value(gid,Config),
		       ffs_lib:get_value(mode,Config)),
    ffs_config:write({fs_ctx, Name}, Ctx), 
    lists:foreach(fun(#ffs_fs_inode{chunks=Cs}) ->
                          Ratio = 2,
                          [ffs_chunk_server:register_chunk(Name, ChunkId, Ratio)
                           || #ffs_fs_chunk{chunkid=ChunkId } <- Cs]
                  end, ffs_fat:list(Ctx, all)),

    ok.

stop(Name) ->
    ffs_fat:stop(Name).

%%%===================================================================
%%% Internal functions
%%%==================================================================

store_chunk(Data,_Config) ->
    %% io:format("Storing data\n",[]), 
    Ratio = 2,
    ffs_chunk_server:write(Data,Ratio).

delete_chunk(ChunkId,_Config) ->
	io:format("Deleting ~p\n",[ChunkId]),
    ok.

read_chunks(Chunks) ->
    Data = ffs_lib:pmap(fun(#ffs_fs_chunk{ chunkid = ChunkId }) ->
				ffs_chunk_server:read(ChunkId)
			end,Chunks),
    read_chunks(Data,<<>>).

read_chunks([{ok,<<Data/binary>>}|T],<<Acc/binary>>) ->
    read_chunks(T,<<Acc/binary,Data/binary>>);
read_chunks([{error,Reason}],_) ->
    {error,Reason};
read_chunks([],Acc) ->
    Acc.

