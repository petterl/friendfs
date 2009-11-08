%%%-------------------------------------------------------------------
%%% File    : ffs_fat.erl
%%% Author  : Lukas Larsson <garazdawi@gmail.com>
%%% Description : A representation of a filesystem in erlang using the posix 
%%%               inode way and three ets tables.
%%%
%%% Created : 21 Aug 2009 by Lukas Larsson <garazdawi@gmail.com>
%%%-------------------------------------------------------------------
-module(ffs_fat).

%% API
-export([init/1,
	 init/4,
	 make_dir/6,
	 create/8,
	 lookup/2,
	 find/3,
	 list/2,
	 ln/5,
	 unlink/3,
	 rename/4,
	 chmod/3,
	 chown/3,
	 chgrp/3,
	 access/2,
	 modify/4,
	 list_xattr/2,
	 get_xattr/3,
	 set_xattr/4,
	 delete_xattr/3,
	 write_cache/3,
	 flush_cache/3,
	 read/4
]).

-include_lib("friendfs/include/friendfs.hrl").

-ifdef(TEST).
-include("ffs_fat.hrl").
-endif.
%%====================================================================
%% Typedefs
%%====================================================================
%% @type ffs_fat_ctx() = term(). 
%%     Contains information about which tables to use.
%% @end
%%
%% @type ffs_inode() = #ffs_inode{}.
%%     Contains information about a specific inode.
%% @end
%%
%% @type ffs_link() = #ffs_link{}.
%%     Contains information about a specifik link.
%% @end
%%
%% @type inodei() = integer().
%%      An integer refering to an inode in the inode array.
%% @end

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Initiate a new ffs_fat system. The return value of this function 
%% is usedd as indata to all other functions in this module. 
%%
%% @spec init(Name) -> ffs_fat_ctx()
%%     Name = atom()
%% @end
%%--------------------------------------------------------------------
init(Name) ->
    init(Name,-1,-1,?U bor ?G_X bor ?G_R bor ?O_X bor ?O_R).
init(Name,Uid,Gid,Mode) ->
    Ctx = ffs_fat_store:init(Name),    
    create(Ctx,1,"..",Uid,Gid,?D bor Mode,0,0),
    Ctx.

stop(Name) ->
    ffs_fat_store:stop(Name).

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   make_dir(Ctx,Parent,Name,Uid,Gid,Mode) -> ffs_inode()
%%      Ctx = ffs_fat_ctx()
%%      Parent = inodei()
%%      Name = string()
%%      Uid = integer()
%%      Gid = integer()
%%      Mode = integer()
%% @end
%%--------------------------------------------------------------------
make_dir(Ctx, Parent, Name, Uid, Gid, Mode) -> 
    
    Inode = create(Ctx,Parent,Name,Uid,Gid,Mode bor ?D,0,4096),
    ln(Ctx,Inode#ffs_inode.inode,Parent,"..",hard),
    
    Inode.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   create(Ctx, Parent, Name, Uid, Gid, Mode, Hash, Size) -> ffs_inode()
%%      Ctx = ffs_fat_ctx()
%%      Parent = inodei()
%%      Name = string()
%%      Uid = integer()
%%      Gid = integer()
%%      Mode = integer()
%%      Hash = integer()
%%      Size = integer()
%% @end
%%--------------------------------------------------------------------
create(Ctx, Parent, Name, Uid, Gid, Mode, Hash, Size) ->
    NewInodeI = ffs_fat_store:get_new_inode(Ctx),

    Timestamp = now(),
    if
	?D band Mode =/= 0 ->
	    NewMode = Mode;
	true ->
	    NewMode = Mode bor ?F
    end,
    NewInode = #ffs_inode{
      inode = NewInodeI,
      hash = Hash,
      size = Size,
      uid = Uid,
      gid = Gid,
      mode = NewMode,
      ctime = Timestamp,
      atime = Timestamp,
      mtime = Timestamp,
      refcount = 0
     },
    ffs_fat_store:store_inode(Ctx, NewInode),
    ffs_fat_store:store_xattr(Ctx, #ffs_xattr{inode = NewInodeI}),
    ln(Ctx,Parent,NewInodeI,Name,hard),
    lookup(Ctx,NewInodeI).

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   lookup(Ctx,InodeI) -> #ffs_inode{} | enoent
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%% @end
%%--------------------------------------------------------------------
lookup(Ctx,InodeI) -> 
    case ffs_fat_store:lookup_inode(Ctx, InodeI) of
	[] ->
	    enoent;
	[Inode] ->
	    Inode
    end.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   find(Ctx,InodeI,Path) -> integer() | enoent
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      Path = string()
%% @end
%%--------------------------------------------------------------------
find(Ctx,_Inode,[$/|Path]) ->
    find(Ctx,1,Path);
find(Ctx,Inode,Path) when is_integer(hd(Path)) ->
    find(Ctx,Inode,string:tokens(Path,"/"));
find(Ctx,Inode,[Name|Path]) -> 
    Links = ffs_fat_store:lookup_links(Ctx, Inode),
    case lists:keyfind(Name,#ffs_link.name,Links) of
	false ->
	    {error,enoent};
	#ffs_link{ to = Next } ->
	    find(Ctx,Next,Path)
    end;
find(_Ctx, Inode, []) ->
    Inode.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   list(Ctx,InodeI) -> [#ffs_link{}] | []
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%% @end
%%--------------------------------------------------------------------
list(Ctx,InodeI) -> 
    Links = ffs_fat_store:lookup_links(Ctx,InodeI),
    [#ffs_link{ to = InodeI, from = InodeI, name = ".", type = hard} | Links].

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   ln(Ctx,From,To,Name,Type) -> #ffs_link{}
%%      Ctx = ffs_fat_ctx()
%%      From = inodei()
%%      To = inodei()
%%      Name = string()
%%      Type = soft | hard
%% @end
%%--------------------------------------------------------------------
ln(Ctx, From, To, Name, Type) -> 
    Link = #ffs_link{ from = From, 
		      to = To, 
		      name = Name,
		      type = Type},
    
    ffs_fat_store:store_link(Ctx, Link),
    [#ffs_inode{ refcount = Cnt } = Inode] = ffs_fat_store:lookup_inode(Ctx,To),
    ffs_fat_store:store_inode(Ctx, Inode#ffs_inode{ refcount = Cnt +1 }),
    Link.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   unlink(Ctx,From,Path) -> ok | {error, Error}
%%      Ctx = ffs_fat_ctx()
%%      From = inodei()
%%      Path = string()
%% @end
%%--------------------------------------------------------------------
unlink(Ctx,From,Path) -> 
    ToInodeI = find(Ctx,From,Path),
    %% Only unlink if it exixts
    if 
	ToInodeI =:= enoent ->
	    enoent;
	true ->
	    unlink(Ctx,From,Path,ToInodeI)
    end.
unlink(Ctx, From, Path, ToInodeI) ->
    ToInode = lookup(Ctx,ToInodeI),
    ffs_fat_store:delete_link(Ctx, #ffs_link{ from = From, 
					      to = ToInode#ffs_inode.inode, 
					      name = filename:basename(Path),
					      type = hard }),
    case ToInode of
	#ffs_inode{ refcount = 1 } ->
	    ffs_fat_store:delete_inode(Ctx, ToInodeI),
	    {delete,ToInode};
	#ffs_inode{ refcount = Cnt } ->
	    ffs_fat_store:store_inode(Ctx, ToInode#ffs_inode{ refcount = Cnt-1 }),
	    {keep,ToInode};
	enoent ->
	    enoent
    end.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   rename(Ctx,OldFrom,OldPath,NewPath) -> #ffs_link{} | enoent
%%      Ctx = ffs_fat_ctx()
%%      OldFrom = inodei()
%%      OldPath = string()
%%      NewPath = string()
%% @end
%%--------------------------------------------------------------------
rename(Ctx, OldFrom, OldPath, NewPath) -> 
    NewFromInodeI = find(Ctx,OldFrom,filename:dirname(NewPath)),
    OldToInodeI = find(Ctx,OldFrom,OldPath),
    
    if 
	(NewFromInodeI =:= enoent) or (OldToInodeI =:= enoent) ->
	    enoent;
	true ->
	    rename(Ctx,OldFrom,OldToInodeI,filename:basename(OldPath),
		   NewFromInodeI,filename:basename(NewPath))
    end.
rename(Ctx,OldFrom,OldTo,OldName,NewFrom,NewName) ->
    NewLink = #ffs_link{ to = OldTo,
			 from = NewFrom,
			 name = NewName,
			 type = hard },
    ffs_fat_store:store_link(Ctx, NewLink),
    ffs_fat_store:delete_link(Ctx, #ffs_link{ to = OldTo,
					      from = OldFrom,
					      name = OldName, _ = '_' }),
    NewLink.

%%--------------------------------------------------------------------
%% @doc
%% 
%%	
%% @spec
%%   chmod(Ctx, InodeI, NewMode) -> #ffs_inode{}
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      NewMode = integer()
%% @end
%%--------------------------------------------------------------------
chmod(Ctx, InodeI, NewMode) ->
    Inode = lookup(Ctx,InodeI),
    NewInode = Inode#ffs_inode{ mode = NewMode },
    ffs_fat_store:store_inode(Ctx, NewInode),
    NewInode.


%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   chgrp(Ctx, InodeI, NewGroup) -> #ffs_inode{}
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      NewGroup = integer()
%% @end
%%--------------------------------------------------------------------
chgrp(Ctx, InodeI, NewGroup) ->
    Inode = lookup(Ctx,InodeI),
    NewInode = Inode#ffs_inode{ gid = NewGroup },
    ffs_fat_store:store_inode(Ctx, NewInode),
    NewInode.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   chown(Ctx, InodeI, NewOwner) -> #ffs_inode{}
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      NewOwner = integer()
%% @end
%%--------------------------------------------------------------------
chown(Ctx, InodeI, NewOwner) ->
    Inode = lookup(Ctx,InodeI),
    NewInode = Inode#ffs_inode{ uid = NewOwner },
    ffs_fat_store:store_inode(Ctx, NewInode),
    NewInode.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   access(Ctx,InodeI) -> #ffs_inode{} | enoent
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%% @end
%%--------------------------------------------------------------------
access(Ctx, InodeI) -> 
    Inode = lookup(Ctx,InodeI),
    if 
	Inode =:= enoent ->
	    enoent;
	true ->
	    NewInode = Inode#ffs_inode{ atime = now() },
	    ffs_fat_store:store_inode(Ctx, NewInode),
	    NewInode
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   modify(Ctx,InodeI,NewHash,NewSize) -> #ffs_inode{} | enoent
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      NewHash = integer()
%%      NewSize = integer()
%% @end
%%--------------------------------------------------------------------
modify(Ctx, InodeI, NewHash, NewSize) -> 
    Inode = lookup(Ctx,InodeI),
    if 
	Inode =:= enoent ->
	    enoent;
	true ->
	    NewInode = Inode#ffs_inode{ mtime = now(),
					hash = NewHash,
					size = NewSize },
	    ffs_fat_store:store_inode(Ctx, NewInode),
	    NewInode
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   list_xattr(Ctx,InodeI) -> [{Key,Value}] | [] | enoent
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      Key = term()
%%      Value = term()
%% @end
%%--------------------------------------------------------------------
list_xattr(Ctx, InodeI) ->
    ffs_fat_store:lookup_xattr(Ctx, InodeI).

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   get_xattr(Ctx,InodeI,Key) -> Value | enoent
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      Key = term()
%%      Value = term()
%% @end
%%--------------------------------------------------------------------
get_xattr(Ctx, InodeI, Key) ->
    ffs_fat_store:lookup_xattr(Ctx, InodeI, Key).

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   set_xattr(Ctx,InodeI,Key,Value) -> ok | enoent
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      Key = term()
%%      Value = term()
%% @end
%%--------------------------------------------------------------------
set_xattr(Ctx, InodeI, Key, Value) ->
    ffs_fat_store:store_xattr(Ctx, InodeI, Key, Value).

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   delete_xattr(Ctx,InodeI,Key) -> ok | enoent
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      Key = term()
%% @end
%%--------------------------------------------------------------------
delete_xattr(Ctx, InodeI, Key) ->
    ffs_fat_store:delete_xattr(Ctx, InodeI, Key).

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   write_cache(Ctx,InodeI,Chunk) -> ok | enoent
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      Chunk = {Id, Data}
%%      Data = binary()
%%      Id = integer()
%% @end
%%--------------------------------------------------------------------
write_cache(Ctx,InodeI,Cache) ->
    Inode = lookup(Ctx,InodeI),
    ffs_fat_store:store_inode(Ctx, Inode#ffs_inode{ write_cache = Cache }).

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   flush_cache(Ctx,InodeI,Chunk) -> ok | enoent
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%% @end
%%--------------------------------------------------------------------
flush_cache(Ctx,INodeI,Chunk) ->
    INode = lookup(Ctx,INodeI),
    ffs_fat_store:store_inode(Ctx, INode#ffs_inode{ chunks = Chunk }).

%%--------------------------------------------------------------------
%% @doc
%% Description	
%%
%% @spec
%%   read(Ctx,InodeI,Size,Offset) -> {NewOffset,ChunkIds}
%%      Ctx = ffs_fat_ctx()
%%      InodeI = inodei()
%%      NewOffset = integer()
%%      ChunkIds = [{chunk,ChunkId}] | []
%% @end
%%--------------------------------------------------------------------
read(Ctx,InodeI,Length,Offset) ->
    Inode = lookup(Ctx,InodeI),
    read_chunks(Inode#ffs_inode.chunks,0,Offset,Length).

read_chunks([],CurrSize,Offset,_Length) ->
	{Offset - CurrSize,[]};
read_chunks([#ffs_chunk{ size = Size } = Chunk|T], CurrSize, Offset, Length) 
		when (Size + CurrSize) > Offset ->
	{Offset-CurrSize, read_data(T,Size - (Offset-CurrSize),[Chunk],Length)};
read_chunks([#ffs_chunk{ size = Size } = _Chunk|T], CurrSize, Offset, Length) ->
	read_chunks(T, CurrSize+Size, Offset, Length).

%% If we have read all chunks
read_data(Chunks, CurrLength, Acc, Length) 
		when Chunks == [];CurrLength == Length ->
	lists:reverse(Acc);
read_data([#ffs_chunk{ size = Size } = Chunk|_T], CurrLength, Acc, Length) 
		when (CurrLength + Size + 1) > Length ->
	lists:reverse([Chunk|Acc]);
read_data([#ffs_chunk{ size = Size } = Chunk|T], CurrLength, Acc, Length) ->
	read_data(T,CurrLength + Size,[Chunk|Acc],Length).


%%====================================================================
%% Internal functions
%%====================================================================
	
