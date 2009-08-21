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
-export([init/2,
	 init_counters/0,
	 make_dir/6,
	 create/8,
	 lookup/2,
	 find/2,
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

-include_lib("friendfs/include/friendfs.hrl").

-define(COUNTER_TABLE, ffs_fat_counter).

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
init(Name,PathEncryptionCallback) ->
    ets:insert(?COUNTER_TABLE,{Name,0}),
    Tid = #ffs_tid{ inode = ets:new(Name,[{keypos,#ffs_inode.inode},set]),
		    link = ets:new(Name,[{keypos,#ffs_link.from},set]),
		    xattr = ets:new(Name,[{keypos,#ffs_xattr.inode},set]),
		    path_mfa = PathEncryptionCallback },
    create(Tid,1,".",1,2,?D bor ?U,0,0),
    ln(Tid,1,1,"..",hard),
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
make_dir(#ffs_tid{},
         _Parent,
         _Name,
         _Uid,
	 _Gid,
	 _Mode) -> #ffs_inode{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
create(#ffs_tid{},
       Parent,
       Name,
       Uid,
       Gid,
       Mode,
       Hash,
       Size) ->

    NewInode = #ffs_inode{
      inode = 1,    
      hash = 1,     
      size = 1,     
      uid = 1,      
      gid = 1,      
      mode = 1,
      ctime = 1,
      atime = 1,    
      mtime = 1,    
      refcount = 1, 
      ptr     = 1   
     }.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
lookup(#ffs_tid{},
       _Inode) -> #ffs_inode{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
find(#ffs_tid{},
     _Path) -> #ffs_inode{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
list(#ffs_tid{},
     _Inode) -> [#ffs_link{}].

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
ln(#ffs_tid{},
   _From,
   _To,
   _Name,
   _Type) -> #ffs_link{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
unlink(#ffs_tid{},
       _From,
       _To) -> #ffs_link{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
delete(#ffs_tid{},
       _Inode) -> #ffs_inode{}.

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




      
