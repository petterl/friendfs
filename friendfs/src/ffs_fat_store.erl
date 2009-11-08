%%%-------------------------------------------------------------------
%%% File    : ffs_fat_store.erl
%%% Author  : Petter Sandholdt <petter@sandholdt.se>
%%% Description : The abstract storage of the fat
%%%
%%% Created : 6 Nov 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_fat_store).

%% API
-export([init/0,new/2,get_new_inode_num/1,
         add_inode/3,lookup_inode/2,delete_inode/2,update_inode/2,
         add_link/2,delete_link/5]).

-include_lib("friendfs/include/friendfs.hrl").

-define(COUNTER_TABLE, ffs_fat_counter).

-ifdef(TEST).
%% -include("ffs_fat.hrl").
-endif.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% @spec new(Name,Config) -> ffs_fat_ctx()
%%     Name = atom()
%%     Config = term()
%% @end
%%--------------------------------------------------------------------
new(Name,Config) ->
    ets:insert(?COUNTER_TABLE,{Name,0}),
	AName = list_to_atom(Name),
    #ffs_fs_ctx{ 
      name = Name,
      inode = ets:new(AName,[{keypos,#ffs_fs_inode.inode},public,set]),
      link = ets:new(AName,[{keypos,#ffs_fs_link.from},public,bag]),
      xattr = ets:new(AName,[{keypos,#ffs_fs_xattr.inode},public,set]),
      config = Config }.

%%--------------------------------------------------------------------
%% @doc
%%
%% @spec init() -> ffs_fat_store_ctx()	
%%     Name = atom()
%% @en
%%--------------------------------------------------------------------
init() ->
	ets:new(?COUNTER_TABLE,[public,named_table,{keypos,1},set]).

%%--------------------------------------------------------------------
%% @doc
%%
%% @spec get_new_inode_num(Ctx) -> integer()
%%     Ctx = ffs_fs_ctx{}
%% @en
%%--------------------------------------------------------------------
get_new_inode_num(Ctx) ->
    ets:update_counter(?COUNTER_TABLE,Ctx#ffs_fs_ctx.name,1).

%%--------------------------------------------------------------------
%% @doc
%%
%% @spec add_inode(Ctx, NewInode, Xattr) -> bool()
%%     Ctx = ffs_fs_ctx{}
%%     NewInode = ffs_fs_inode{}
%%     Xattr = ffs_fs_xattr{}
%% @en
%%--------------------------------------------------------------------
add_inode(#ffs_fs_ctx{ inode = InodeCtx, xattr = XattrCtx}, NewInode, Xattr) ->
    ets:insert(InodeCtx,NewInode),
    ets:insert(XattrCtx,Xattr).

%%--------------------------------------------------------------------
%% @doc
%%
%% @spec lookup_inode(Ctx, InodeI) -> ffs_fs_inode{}
%%     Ctx = ffs_fs_ctx{}
%%     InodeI = integer()
%% @en
%%--------------------------------------------------------------------
lookup_inode(#ffs_fs_ctx{ inode = InodeCtx }, InodeI) ->
	ets:lookup( InodeCtx, InodeI).
	
%%--------------------------------------------------------------------
%% @doc
%%
%% @spec delete_inode(Ctx, InodeI ) -> bool()
%%     Ctx = ffs_fs_ctx{}
%%     NewInode = integer()
%% @en
%%--------------------------------------------------------------------
delete_inode(#ffs_fs_ctx{ inode = InodeCtx }, InodeI) ->
    ets:delete(InodeCtx, InodeI).

%%--------------------------------------------------------------------
%% @doc	
%%
%% @spec update_inode( Ctx, Inode ) -> bool()
%%     Ctx = ffs_fs_ctx{}
%%     Inode = ffs_fs_inode{}
%% @en
%%--------------------------------------------------------------------
update_inode(#ffs_fs_ctx{ inode = InodeCtx }, Inode) ->
	ets:insert( InodeCtx, Inode).

%%--------------------------------------------------------------------	
%% @doc	
%%
%% @spec add_link( Ctx, Link ) -> bool()
%%     Ctx = ffs_fs_ctx{}
%%     Link = ffs_fs_link{}
%% @en
%%--------------------------------------------------------------------
add_link(#ffs_fs_ctx{ link = LinkCtx }, Link) ->
    ets:insert( LinkCtx, Link).

%%--------------------------------------------------------------------
%% @doc	
%%
%% @spec delete_link( Ctx, From, To, Name, Type ) -> bool()
%%     Ctx = ffs_fs_ctx{}
%%     From = integer()
%%     To = integer()
%%     Name = string()
%%     Type = hard | soft
%% @en
%%--------------------------------------------------------------------
delete_link(#ffs_fs_ctx{ link = LinkCtx }, From, To, Name, Type) ->
    ets:delete_object(LinkCtx,#ffs_fs_link{ from = From, 
					 to = To, 
					 name = Name,
					 type = Type }).