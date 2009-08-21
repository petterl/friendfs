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
	 find/2,
	 find/3,
	 list/2,
	 ln/5,
	 unlink/3,
	 delete/2,
	 list_xattr/2,
	 get_xattr/3,
	 set_xattr/4,
	 delete_xattr/3,
	 move/5,
	 rename/4,
	 access/2,
	 modify/4]).

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
%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec init(Name::atom(),PathEncryptionCallback:MFA) -> ffs_tid() | {error, Error}
%%   MFA = {M::atom(),F::atom(),A::list()}
%% @end
%% @type ffs_tid() = #ffs_tid{}.
%%--------------------------------------------------------------------
init(Name) ->
	init(Name,{ffs_fat,path_parse,[]}).
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
       Parent,
       Name,
       Uid,
       Gid,
       Mode,
       Hash,
       Size) ->
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
lookup(#ffs_tid{ inode = InodeTid },Inode) -> 
	ets:lookup(InodeTid,Inode).

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
find(Tid, Inode, []) ->
	lookup(Tid,Inode).

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
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
unlink(#ffs_tid{ link = LinkTid, inode = InodeTid },From,To) -> 
	[#ffs_inode{ refcount = Cnt } = Inode] = ets:lookup(InodeTid,To),
	if 
		Cnt == 1 ->
			ets:delete(InodeTid,To);
		true ->
			ets:insert(InodeTid,Inode#ffs_inode{ refcount = Cnt - 1 })
	end,
	ets:delete_object(LinkTid,#ffs_link{ from = From, to = To, _ = '_' }).

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
delete(#ffs_tid{},
       _Inode) -> false.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
move(#ffs_tid{},
     _OldFrom,
     _OldTo,
     _NewFrom,
     _NewName) -> #ffs_link{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
rename(#ffs_tid{} = Tid,
       From,
       To,
       NewName) -> move(Tid,From,To,From,NewName).

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
access(#ffs_tid{},
       _Inode) -> #ffs_inode{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
modify(#ffs_tid{},
       _Inode,
       _NewHash,
       _NewSize) -> #ffs_inode{}.

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

get_path_to_inode(_Tid,_Inode) ->
	"".
	
path_parse(encrypt,Path,#ffs_inode{ mode = M },[]) when ((M band ?D) =:= 0) ->
	re:replace(Path,"/","-",[global,{return,list}])++"?file.ffs";
path_parse(encrypt,Path,#ffs_inode{ },[]) ->
	re:replace(Path,"/","-",[global,{return,list}])++"?dir.ffs".