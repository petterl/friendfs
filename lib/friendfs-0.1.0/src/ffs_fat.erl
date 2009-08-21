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
	 make_dir/6,
	 create/8,
	 lookup/2,
	 find/2,
	 link/5,
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
    #ffs_tid{ path_mfa = PathEncryptionCallback }.


%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
make_dir(#ffs_tid{},
         Parent,
         Name,
         Uid,
	 Gid,
	 Mode) -> #ffs_inode{}.

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
       Size) -> #ffs_inode{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
lookup(#ffs_tid{},
       Inode) -> #ffs_inode{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
find(#ffs_tid{},
     Path) -> #ffs_inode{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
list(#ffs_tid{},
     Inode) -> [#ffs_link{}].

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
link(#ffs_tid{},
     From,
     To,
     Name,
     Type) -> #ffs_link{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
unlink(#ffs_tid{},
       From,
       To) -> #ffs_link{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
delete(#ffs_tid{},
       Inode) -> #ffs_inode{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
move(#ffs_tid{},
     OldFrom,
     OldTo,
     NewFrom,
     NewName) -> #ffs_link{}.

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
       Inode) -> #ffs_inode{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
modify(#ffs_tid{},
       Inode,
       NewHash,
       NewSize) -> #ffs_inode{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
list_xattr(#ffs_tid{},
           Inode) -> #ffs_xattr{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_xattr(#ffs_tid{},
          Inode,
	  Key) -> {error,invalid_inode}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
set_xattr(#ffs_tid{},
	  Inode,
	  Key,
	  Value) -> #ffs_xattr{}.

%%--------------------------------------------------------------------
%% @doc
%% Description
%%
%% @spec
%%   function(Args) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
delete_xattr(#ffs_tid{},
	     Inode,
	     Key) -> {error,invalid_inode}.


%%====================================================================
%% Internal functions
%%====================================================================




      
