%%%-------------------------------------------------------------------
%%% File    : ffs_fat.erl
%%% Author  : Lukas Larsson <garazdawi@gmail.com>
%%% Description : A representation of a filesystem in erlang using the posix inode way
%%%               and three ets tables.
%%%
%%% Created : 21 Aug 2009 by Lukas Larsson <garazdawi@gmail.com>
%%%-------------------------------------------------------------------
-module(ffs_fat).

%% API
-export([init/2,init/1,
	 init_counters/0,
	 make_dir/6,
	 create/8,
	 lookup/2,
	 lookup/3,
	 find/2,
	 find/3,
	 list/2,
	 list/3,
	 ln/5,
	 unlink/3,
	 rename/4,
	 access/2,
	 modify/4,
	 list_xattr/2,
	 get_xattr/3,
	 set_xattr/4,
	 delete_xattr/3
]).

%% Internal
-export([path_parse/4]).

-include_lib("friendfs/include/friendfs.hrl").

-define(COUNTER_TABLE, ffs_fat_counter).

-ifdef(TEST).
-include("ffs_fat.hrl").
-endif.
%%====================================================================
%% Typedefs
%%====================================================================

%%====================================================================
%% API
%%====================================================================
%% @spec init(Name::atom()) -> ffs_tid()
%% @equiv init(Name, {ffs_fat,path_parse,[]})ter
init(Name) ->
	init(Name,{ffs_fat,path_parse,[]}).
	
%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec init(Name::atom(),PathEncryptionCallback:MFA) -> ffs_tid() | {error, Error}
%%   MFA = {M::atom(),F::atom(),A::list()}
%% @end
%% @type ffs_tid() = #ffs_tid{}.
%%--------------------------------------------------------------------
init(Name,PathEncryptionCallback) ->
    ets:insert(?COUNTER_TABLE,{Name,0}),
    Tid = #ffs_tid{ 
            name = Name,
		    inode = ets:new(Name,[{keypos,#ffs_inode.inode},set]),
		    link = ets:new(Name,[{keypos,#ffs_link.from},bag]),
		    xattr = ets:new(Name,[{keypos,#ffs_xattr.inode},set]),
		    path_mfa = PathEncryptionCallback },
    create(Tid,1,"..",1,2,?D bor ?U,0,0),
    Tid.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec init(Name::atom(),PathEncryptionCallback:MFA) -> ffs_tid() | {error, Error}
%%   MFA = {M::atom(),F::atom(),A::list()}
%% @end
%% @type ffs_tid() = #ffs_tid{}.
%%--------------------------------------------------------------------
init_counters() ->
    ets:new(?COUNTER_TABLE,[public,named_table,{keypos,1},set]).

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
make_dir(Tid,
         Parent,
         Name,
         Uid,
	     Gid,
	     Mode) -> 
	
	Inode = create(Tid,Parent,Name,Uid,Gid,Mode bor ?D,0,0),
	ln(Tid,Inode#ffs_inode.inode,Parent,"..",hard),
	
	Inode.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
create(#ffs_tid{ inode = InodeTid, xattr = XattrTid, path_mfa = {M,F,A}} = Tid,
       Parent, Name, Uid, Gid, Mode, Hash, Size) ->
	NewInodeI = ets:update_counter(?COUNTER_TABLE,Tid#ffs_tid.name,1),
	Path = get_path_to_inode(Tid,Parent) ++ Name,
	Timestamp = now(),
	
    NewInode = #ffs_inode{
      inode = NewInodeI,
      hash = Hash,
      size = Size,
      uid = Uid,
      gid = Gid,
      mode = Mode,
      ctime = Timestamp,
      atime = Timestamp,
      mtime = Timestamp,
      refcount = 0, 
      ptr = ""
     },
	Ptr = (catch M:F(encrypt,Path,NewInode,A)),
	NewInodeWPtr = NewInode#ffs_inode{ ptr = Ptr},
	ets:insert(InodeTid,NewInodeWPtr),
	ln(Tid,Parent,NewInodeI,Name,hard),
	ets:insert(XattrTid,#ffs_xattr{inode = NewInodeI}),
	NewInodeWPtr.
	

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
lookup(Tid,BaseInode,Path) ->
	Inode = find(Tid,BaseInode,Path),
	if 
		Inode =:= enoent ->
			enoent;
		true ->
			lookup(Tid,Inode)
	end.
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
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
find(Tid,Path) ->
	find(Tid,1,Path).
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
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
list(Tid,BaseInode,Path) ->
	Inode = find(Tid,BaseInode,Path),
	if 
		Inode =:= enoent ->
			enoent;
		true ->
			list(Tid,Inode)
	end.
list(#ffs_tid{ link = LinkTid },Inode) -> 
	Links = ets:lookup(LinkTid,Inode),
	[#ffs_link{ to = Inode, from = Inode, name = ".", type = hard} | Links].

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
ln(#ffs_tid{ inode = InodeTid, link = LinkTid},
   From,
   To,
   Name,
   Type) -> 
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
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
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
unlink(#ffs_tid{ link = LinkTid, inode = InodeTid } = Tid, From,Path,ToInodeI) ->
	ToInode = lookup(Tid,ToInodeI),
	case ToInode of
		#ffs_inode{ refcount = 1 } ->
			ets:delete(InodeTid,ToInodeI);
 		#ffs_inode{ refcount = Cnt } ->
			ets:insert(InodeTid,ToInode#ffs_inode{ refcount = Cnt - 1 })
	end,
	ets:delete_object(LinkTid,#ffs_link{ from = From, 
										 to = ToInode#ffs_inode.inode, 
										 name = filename:basename(Path), _ = '_' }).

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
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
	NewLink = #ffs_link{ to = OldTo, from = NewFrom, name = NewName, type = hard },
	ets:insert(LinkTid,NewLink),
	ets:delete_object(LinkTid,#ffs_link{ to = OldTo, from = OldFrom, name = OldName, _ = '_' }),
	NewLink.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
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
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
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
			NewInode = Inode#ffs_inode{ mtime = now(), hash = NewHash, size = NewSize },
			ets:insert(InodeTid, NewInode),
			NewInode
	end.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
list_xattr(#ffs_tid{},
           _Inode) -> #ffs_xattr{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_xattr(#ffs_tid{},
          _Inode,
	  _Key) -> {error,invalid_inode}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
set_xattr(#ffs_tid{},
	  _Inode,
	  _Key,
	  _Value) -> #ffs_xattr{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
delete_xattr(#ffs_tid{},
	     _Inode,
	     _Key) -> {error,invalid_inode}.


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