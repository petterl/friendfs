%%%-------------------------------------------------------------------
%%% File    : ffs_fat_store.erl
%%% Author  : Petter Sandholdt <petter@sandholdt.se>
%%% Description : The abstract storage of the fat
%%%
%%% Created : 6 Nov 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_fat_store).

%% API
-export([new/1,
	 get_new_inode/1,
	 store_inode/2,
	 lookup_inode/2,
	 delete_inode/2,
	 store_xattr/2,
	 store_xattr/4,
	 lookup_xattr/2,
	 lookup_xattr/3,
	 delete_xattr/2,
	 store_link/2,
	 lookup_links/2,
	 delete_link/2
]).

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
%% @spec new(Name) -> ffs_fat_store_ctx()
%%     Name = atom()
%% @end
%%--------------------------------------------------------------------
new(Name) ->
    case ets:info(?COUNTER_TABLE) of
	undefined ->
	    ets:new(?COUNTER_TABLE,[public,named_table,{keypos,1},set]);
	_ ->
	    ok
    end,

    ets:insert(?COUNTER_TABLE,{Name,0}),
    AName = list_to_atom(Name),
    Ctx = #ffs_tid{ 
      name = Name,
      inode = ets:new(AName,[{keypos,#ffs_inode.inode},public,set]),
      link = ets:new(AName,[{keypos,#ffs_link.from},public,bag]),
      xattr = ets:new(AName,[{keypos,#ffs_xattr.inode},public,set]),
      config = [{chunkid_mfa,{ffs_lib,get_chunkid}}]},
    Ctx.

get_new_inode(#ffs_tid{ name = Name}) ->
    ets:update_counter(?COUNTER_TABLE,Name,1).

store_inode(#ffs_tid{ inode = InodeTid}, Inode) ->
    ets:insert(InodeTid,Inode).

lookup_inode(#ffs_tid{ inode = InodeTid}, InodeI) ->
    ets:lookup(InodeTid, InodeI).

delete_inode(#ffs_tid{ inode = InodeTid}, InodeI) ->
    ets:delete(InodeTid,InodeI).


store_xattr(#ffs_tid{xattr = XattrTid}, Xattr) ->
    ets:insert(XattrTid,Xattr).
    
store_xattr(#ffs_tid{xattr = _XattrTid}, _InodeI, _Key, _Value) ->
    enoent.
    
lookup_xattr(#ffs_tid{xattr = XattrTid}, InodeI) ->
    ets:lookup(XattrTid, InodeI).    

lookup_xattr(#ffs_tid{xattr = _XattrTid}, _InodeI, _Key) ->
    enoent.

delete_xattr(#ffs_tid{ xattr = _XattrTid}, _Key) ->
    enoent.


store_link(#ffs_tid{ link = LinkTid}, Link) ->
    ets:insert(LinkTid, Link).

lookup_links(#ffs_tid{ link = LinkTid}, InodeI) ->
    ets:lookup(LinkTid, InodeI).

delete_link(#ffs_tid{ link = LinkTid }, Link) ->
    ets:delete_object(LinkTid,Link).

