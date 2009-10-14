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
	 init/2,
	 init/5,
	 init_counters/0,
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
	 write_cache/4,
	 flush_cache/2,
	 read/4
]).

-include_lib("friendfs/include/friendfs.hrl").

-define(COUNTER_TABLE, ffs_fat_counter).

-ifdef(TEST).
-include("ffs_fat.hrl").
-endif.
%%====================================================================
%% Typedefs
%%====================================================================
%% @type ffs_tid() = #ffs_tid{}. 
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
%% @spec init(Name) -> ffs_tid()
%%     Name = atom()
%% @end
%%--------------------------------------------------------------------
init(Name) ->
    init(Name,2 bsl 20).
init(Name,ChunkSize) ->
    init(Name,ChunkSize,-1,-1,?U bor ?G_X bor ?G_R bor ?O_X bor ?O_R).
init(Name,ChunkSize,Uid,Gid,Mode) ->
    ets:insert(?COUNTER_TABLE,{Name,0}),
    Tid = #ffs_tid{ 
      name = Name,
      inode = ets:new(Name,[{keypos,#ffs_inode.inode},set]),
      link = ets:new(Name,[{keypos,#ffs_link.from},bag]),
      xattr = ets:new(Name,[{keypos,#ffs_xattr.inode},set]),
      config = [{chunk_size,ChunkSize},{chunkid_mfa,{ffs_lib,get_chunkid}}]},
    create(Tid,1,"..",Uid,Gid,?D bor Mode,0,0),
    Tid.

%%--------------------------------------------------------------------
%% @doc
%% Initate the counter used to find new Inode numbers. This needs to be
%% called before calling the first init/1 call.
%%
%% @spec init_counters() -> pid()
%% @end
%%--------------------------------------------------------------------
init_counters() ->
    ets:new(?COUNTER_TABLE,[public,named_table,{keypos,1},set]).

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   make_dir(Tid,Parent,Name,Uid,Gid,Mode) -> ffs_inode()
%%      Tid = ffs_tid()
%%      Parent = inodei()
%%      Name = string()
%%      Uid = integer()
%%      Gid = integer()
%%      Mode = integer()
%% @end
%%--------------------------------------------------------------------
make_dir(Tid, Parent, Name, Uid, Gid, Mode) -> 
    
    Inode = create(Tid,Parent,Name,Uid,Gid,Mode bor ?D,0,4096),
    ln(Tid,Inode#ffs_inode.inode,Parent,"..",hard),
    
    Inode.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   create(Tid, Parent, Name, Uid, Gid, Mode, Hash, Size) -> ffs_inode()
%%      Tid = ffs_tid()
%%      Parent = inodei()
%%      Name = string()
%%      Uid = integer()
%%      Gid = integer()
%%      Mode = integer()
%%      Hash = integer()
%%      Size = integer()
%% @end
%%--------------------------------------------------------------------
create(#ffs_tid{ inode = InodeTid, xattr = XattrTid} = Tid,
       Parent, Name, Uid, Gid, Mode, Hash, Size) ->
    NewInodeI = ets:update_counter(?COUNTER_TABLE,Tid#ffs_tid.name,1),
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
    ets:insert(InodeTid,NewInode),
    ln(Tid,Parent,NewInodeI,Name,hard),
    ets:insert(XattrTid,#ffs_xattr{inode = NewInodeI}),
    lookup(Tid,NewInodeI).

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   lookup(Tid,InodeI) -> #ffs_inode{} | enoent
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%% @end
%%--------------------------------------------------------------------
lookup(#ffs_tid{ inode = InodeTid },InodeI) -> 
    case ets:lookup(InodeTid,InodeI) of
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
%%   find(Tid,InodeI,Path) -> integer() | enoent
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      Path = string()
%% @end
%%--------------------------------------------------------------------
find(Tid,_Inode,[$/|Path]) ->
    find(Tid,1,Path);
find(Tid,Inode,Path) when is_integer(hd(Path)) ->
    find(Tid,Inode,string:tokens(Path,"/"));
find(#ffs_tid{ link = LinkTid } = Tid,Inode,[Name|Path]) -> 
    Links = ets:lookup(LinkTid,Inode),
    case lists:keyfind(Name,#ffs_link.name,Links) of
	false ->
	    {error,enoent};
	#ffs_link{ to = Next } ->
	    find(Tid,Next,Path)
    end;
find(_Tid, Inode, []) ->
    Inode.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   list(Tid,InodeI) -> [#ffs_link{}] | []
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%% @end
%%--------------------------------------------------------------------
list(#ffs_tid{ link = LinkTid },InodeI) -> 
    Links = ets:lookup(LinkTid,InodeI),
    [#ffs_link{ to = InodeI, from = InodeI, name = ".", type = hard} | Links].

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   ln(Tid,From,To,Name,Type) -> #ffs_link{}
%%      Tid = ffs_tid()
%%      From = inodei()
%%      To = inodei()
%%      Name = string()
%%      Type = soft | hard
%% @end
%%--------------------------------------------------------------------
ln(#ffs_tid{ inode = InodeTid, link = LinkTid}, From, To, Name, Type) -> 
    Link = #ffs_link{ from = From, 
		      to = To, 
		      name = Name,
		      type = Type},
    
    ets:insert(LinkTid,Link),
    [#ffs_inode{ refcount = Cnt } = Inode] = ets:lookup(InodeTid,To),
    ets:insert(InodeTid,Inode#ffs_inode{ refcount = Cnt +1 }),
    Link.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   unlink(Tid,From,Path) -> ok | {error, Error}
%%      Tid = ffs_tid()
%%      From = inodei()
%%      Path = string()
%% @end
%%--------------------------------------------------------------------
unlink(Tid,From,Path) -> 
    ToInodeI = find(Tid,From,Path),
    %% Only unlink if it exixts
    if 
	ToInodeI =:= enoent ->
	    enoent;
	true ->
	    unlink(Tid,From,Path,ToInodeI)
    end.
unlink(#ffs_tid{ link = LinkTid, inode = InodeTid } = Tid,
       From, Path, ToInodeI) ->
    ToInode = lookup(Tid,ToInodeI),
    ets:delete_object(LinkTid,#ffs_link{ from = From, 
					 to = ToInode#ffs_inode.inode, 
					 name = filename:basename(Path),
					 type = hard }),
    
    case ToInode of
	#ffs_inode{ refcount = 1 } ->
	    ets:delete(InodeTid,ToInodeI),
	    {delete,ToInode};
	#ffs_inode{ refcount = Cnt } ->
	    ets:insert(InodeTid,ToInode#ffs_inode{ refcount = Cnt - 1 }),
	    {keep,ToInode};
	enoent ->
	    enoent
    end.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   rename(Tid,OldFrom,OldPath,NewPath) -> #ffs_link{} | enoent
%%      Tid = ffs_tid()
%%      OldFrom = inodei()
%%      OldPath = string()
%%      NewPath = string()
%% @end
%%--------------------------------------------------------------------
rename(Tid, OldFrom, OldPath, NewPath) -> 
    
    NewFromInodeI = find(Tid,OldFrom,filename:dirname(NewPath)),
    OldToInodeI = find(Tid,OldFrom,OldPath),
    
    if 
	(NewFromInodeI =:= enoent) or (OldToInodeI =:= enoent) ->
	    enoent;
	true ->
	    rename(Tid,OldFrom,OldToInodeI,filename:basename(OldPath),
		   NewFromInodeI,filename:basename(NewPath))
    end.
rename(#ffs_tid{ link = LinkTid },OldFrom,OldTo,OldName,NewFrom,NewName) ->
    NewLink = #ffs_link{ to = OldTo,
			 from = NewFrom,
			 name = NewName,
			 type = hard },
    ets:insert(LinkTid,NewLink),
    ets:delete_object(LinkTid,#ffs_link{ to = OldTo,
					 from = OldFrom,
					 name = OldName, _ = '_' }),
    NewLink.

%%--------------------------------------------------------------------
%% @doc
%% 
%%	
%% @spec
%%   chmod(Tid, InodeI, NewMode) -> #ffs_inode{}
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      NewMode = integer()
%% @end
%%--------------------------------------------------------------------
chmod(Tid, InodeI, NewMode) ->
    Inode = lookup(Tid,InodeI),
    NewInode = Inode#ffs_inode{ mode = NewMode },
    ets:insert(Tid#ffs_tid.inode,NewInode),
    NewInode.


%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   chgrp(Tid, InodeI, NewGroup) -> #ffs_inode{}
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      NewGroup = integer()
%% @end
%%--------------------------------------------------------------------
chgrp(Tid, InodeI, NewGroup) ->
    Inode = lookup(Tid,InodeI),
    NewInode = Inode#ffs_inode{ gid = NewGroup },
    ets:insert(Tid#ffs_tid.inode,NewInode),
    NewInode.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   chown(Tid, InodeI, NewOwner) -> #ffs_inode{}
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      NewOwner = integer()
%% @end
%%--------------------------------------------------------------------
chown(Tid, InodeI, NewOwner) ->
    Inode = lookup(Tid,InodeI),
    NewInode = Inode#ffs_inode{ uid = NewOwner },
    ets:insert(Tid#ffs_tid.inode,NewInode),
    NewInode.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   access(Tid,InodeI) -> #ffs_inode{} | enoent
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%% @end
%%--------------------------------------------------------------------
access(#ffs_tid{ inode = InodeTid } = Tid, InodeI) -> 
    Inode = lookup(Tid,InodeI),
    if 
	Inode =:= enoent ->
	    enoent;
	true ->
	    NewInode = Inode#ffs_inode{ atime = now() },
	    ets:insert(InodeTid, NewInode),
	    NewInode
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   modify(Tid,InodeI,NewHash,NewSize) -> #ffs_inode{} | enoent
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      NewHash = integer()
%%      NewSize = integer()
%% @end
%%--------------------------------------------------------------------
modify(#ffs_tid{ inode = InodeTid } = Tid,
       InodeI,
       NewHash,
       NewSize) -> 
    Inode = lookup(Tid,InodeI),
    if 
	Inode =:= enoent ->
	    enoent;
	true ->
	    NewInode = Inode#ffs_inode{ mtime = now(),
					hash = NewHash,
					size = NewSize },
	    ets:insert(InodeTid, NewInode),
	    NewInode
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   list_xattr(Tid,InodeI) -> [{Key,Value}] | [] | enoent
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      Key = term()
%%      Value = term()
%% @end
%%--------------------------------------------------------------------
list_xattr(#ffs_tid{}, _Inode) ->
    #ffs_xattr{}.

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec
%%   get_xattr(Tid,InodeI,Key) -> Value | enoent
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      Key = term()
%%      Value = term()
%% @end
%%--------------------------------------------------------------------
get_xattr(#ffs_tid{}, _Inode, _Key) ->
    enoent.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   set_xattr(Tid,InodeI,Key,Value) -> ok | enoent
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      Key = term()
%%      Value = term()
%% @end
%%--------------------------------------------------------------------
set_xattr(#ffs_tid{}, _Inode, _Key, _Value) ->
    #ffs_xattr{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   delete_xattr(Tid,InodeI,Key) -> ok | enoent
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      Key = term()
%% @end
%%--------------------------------------------------------------------
delete_xattr(#ffs_tid{}, _Inode, _Key) ->
    enoent.


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
    Inode = lookup(Tid,InodeI),
    NewData = case Inode of
		  #ffs_inode{ write_cache = undefined } when (Offset rem ChunkSize) == 0 ->
		      AppendData;
		  #ffs_inode{ write_cache = OldData } when size(OldData) == (Offset rem ChunkSize) ->
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
	    ets:insert(Tid#ffs_tid.inode,NewInode#ffs_inode{ write_cache = NewData }),
	    Acc
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
    flush_cache(Tid,lookup(Tid,InodeI));
flush_cache(_Tid,#ffs_inode{ write_cache = undefined }) ->
    empty;
flush_cache(Tid,#ffs_inode{ write_cache = Data } = Inode) ->
    {M,F} = ffs_lib:get_value(chunkid_mfa,Tid#ffs_tid.config),
    ChunkId = M:F(Data),
    NewInode = Inode#ffs_inode{ chunkids = [ChunkId|Inode#ffs_inode.chunkids],
				write_cache = undefined },
    ets:insert(Tid#ffs_tid.inode,NewInode),
    {chunk,ChunkId,Data};
flush_cache(_Tid,Else) ->
    Else.


%%--------------------------------------------------------------------
%% @doc
%% Description	
%%
%% @spec
%%   read(Tid,InodeI,Size,Offset) -> {NewOffset,ChunkIds}
%%      Tid = ffs_tid()
%%      InodeI = inodei()
%%      NewOffset = integer()
%%      ChunkIds = [{chunk,ChunkId}] | []
%% @end
%%--------------------------------------------------------------------
read(Tid,InodeI,Size,Offset) ->
    ChunkSize = ffs_lib:get_value(chunk_size,Tid#ffs_tid.config),
    StartChunk = Offset div ChunkSize,
    EndChunk = (Size+(Offset rem ChunkSize)-1) div ChunkSize,
    Inode = lookup(Tid,InodeI),
    {Offset rem ChunkSize,
     read_chunks(0,StartChunk,EndChunk,Inode#ffs_inode.chunkids)}.


read_chunks(Current,Current,Current,[H|_T]) ->
    [{chunk,H}];
read_chunks(Current,Current,EndChunk,[H|T]) ->
    [{chunk,H}|read_chunks(Current+1,Current+1,EndChunk,T)];
read_chunks(Current,StartChunk,EndChunk,[_H|T]) ->
    read_chunks(Current+1,StartChunk,EndChunk,T);
read_chunks(_Current,_StartChunk,_EndChunk,[]) ->
    [].

%%====================================================================
%% Internal functions
%%====================================================================
	
get_path_to_inode(Tid,Inode) ->
    get_path_to_inode(Tid,Inode,"").
get_path_to_inode(_Tid,1,Path) ->
    lists:flatten(["/"|Path]);
get_path_to_inode(Tid,Inode,Acc) ->
    ParentInode = find(Tid,Inode,".."),
    Links = ets:match_object(Tid#ffs_tid.link,
			     #ffs_link{ from = ParentInode, 
					to = Inode, _ = '_' }),
    get_path_to_inode(Tid,ParentInode,[(hd(Links))#ffs_link.name,"/"|Acc]).

path_parse(encrypt,Path,#ffs_inode{ mode = M },[]) when ((M band ?D) =:= 0) ->
    re:replace(Path,"/","-",[global,{return,list}])++"?file.ffs";
path_parse(encrypt,Path,#ffs_inode{ },[]) ->
    re:replace(Path,"/","-",[global,{return,list}])++"?dir.ffs".
