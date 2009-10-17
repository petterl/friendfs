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
    Links = ffs_fat:list(ffs_config:read({fs_tid, FsName}), INodeI),
    lists:map(fun(#ffs_link{ name = Name, to = To }) ->
		      {Name,ffs_fat:lookup(ffs_config:read({fs_tid, FsName}),To)}
	      end,Links).    

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
write(FsName, InodeI, Data, Offset) ->
    Tid = ffs_config:read({fs_tid, FsName}),
    Config = ffs_config:read({fs_config, FsName}),
    case write_cache(Tid, InodeI, Data, Offset) of
	[] ->
	    ok;
	Chunks ->
	    update_chunks(Chunks, Config)
    end.

update_chunks([{add_chunk, ChunkId, ChunkData} | R], Config) ->
    store_chunk(ChunkId,ChunkData,Config),
    update_chunks(R, Config);
update_chunks([{delete_chunk, ChunkId} | R], Config) ->
    delete_chunk(ChunkId, Config),
    update_chunks(R, Config);
update_chunks([],_Config) ->
    [].

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
write_cache(Tid,InodeI,AppendData,Offset) ->
    write_cache(Tid,InodeI,AppendData,Offset,[]).
write_cache(Tid,InodeI,AppendData,Offset,Acc) ->
    ChunkSize = ffs_lib:get_value(chunk_size,Tid#ffs_tid.config),
    Inode = ffs_fat:lookup(Tid,InodeI),
    NewData = case Inode of
		  #ffs_inode{ write_cache = undefined } 
		    when (Offset rem ChunkSize) == 0 ->
		      AppendData;
		  #ffs_inode{ write_cache = OldData } 
		    when size(OldData) == (Offset rem ChunkSize) ->
		      <<OldData/binary,AppendData/binary>>
			  end,
    io:format("Size of data is ~p\n",[size(NewData)]),
    NewInode = Inode#ffs_inode{ size = size(AppendData) + Inode#ffs_inode.size },
    if 
	size(NewData) > ChunkSize ->
	    <<FirstChunk:ChunkSize/binary,Rest/binary>> = NewData,
	    Chunk = flush_cache(Tid,NewInode#ffs_inode{ write_cache = FirstChunk }),
	    write_cache(Tid,InodeI,Rest,Offset+ChunkSize,[Chunk|Acc]);
	size(NewData) == ChunkSize ->
	    Chunk = flush_cache(Tid,NewInode#ffs_inode{ write_cache = NewData }),
	    [Chunk|Acc];
	true ->
	    ffs_fat:write_cache( Tid, InodeI, NewData),
	    Acc
    end.


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
	    update_chunks(Chunks, Config)
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
		ChunkId <- Inode#ffs_inode.chunkids];
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
	      {chunk_size,1 bsl 15}, %% 32 kB
	      {mnt_opts,0},
	      {max_filename_size,36#sup},
              {uid,-1},
              {gid,-1},
              {mode,755}],
    ffs_config:write({fs_config, Name}, Config), 
    
    Tid = ffs_fat:init(list_to_atom(Name),
		       ffs_lib:get_value(chunk_size,Config),
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
	ffs_chunk_server:write(ChunkId,Data),
	ok.

delete_chunk(ChunkId,_Config) ->
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



