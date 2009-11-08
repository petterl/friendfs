%%%-------------------------------------------------------------------
%%% File    : ffs_fat_store.erl
%%% Author  : Petter Sandholdt <petter@sandholdt.se>
%%% Description : The abstract storage of the fat
%%%
%%% Created : 6 Nov 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_fat_store).

%% API
-export([init/1,
	 stop/1,
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
init(Name) ->
    case ets:info(?COUNTER_TABLE) of
	undefined ->
	    ets:new(?COUNTER_TABLE,[public,named_table,{keypos,1},set]);
	_ ->
	    ok
    end,

    ets:insert(?COUNTER_TABLE,{Name,0}),
    dets:open_file(list_to_atom(Name++"I"),
		   [{file, "./"++Name++"I"},
		    {keypos,#ffs_inode.inode},{type, set}]),
    dets:open_file(list_to_atom(Name++"L"),
		   [{file, "./"++Name++"L"},
		    {keypos,#ffs_link.from},{type, bag}]),
    dets:open_file(list_to_atom(Name++"X"),
		   [{file, "./"++Name++"X"},
		    {keypos,#ffs_xattr.inode},{type, set}]),
    Ctx = #ffs_tid{ 
      name = Name,
      inode = list_to_atom(Name++"I"),
      link = list_to_atom(Name++"L"),
      xattr = list_to_atom(Name++"X"),
      config = [{chunkid_mfa,{ffs_lib,get_chunkid}}]},
    Ctx.

stop(#ffs_tid{name = Name}) ->
      dets:close(list_to_atom(Name++"I")),
      dets:close(list_to_atom(Name++"L")),
      dets:close(list_to_atom(Name++"X")).

get_new_inode(#ffs_tid{ name = Name}) ->
    ets:update_counter(?COUNTER_TABLE,Name,1).

store_inode(#ffs_tid{ inode = InodeTid}, Inode) ->
    dets:insert(InodeTid,Inode).

lookup_inode(#ffs_tid{ inode = InodeTid}, InodeI) ->
    dets:lookup(InodeTid, InodeI).

delete_inode(#ffs_tid{ inode = InodeTid}, InodeI) ->
    dets:delete(InodeTid,InodeI).


store_xattr(#ffs_tid{xattr = XattrTid}, Xattr) ->
    dets:insert(XattrTid,Xattr).
    
store_xattr(#ffs_tid{xattr = _XattrTid}, _InodeI, _Key, _Value) ->
    enoent.
    
lookup_xattr(#ffs_tid{xattr = XattrTid}, InodeI) ->
    dets:lookup(XattrTid, InodeI).    

lookup_xattr(#ffs_tid{xattr = _XattrTid}, _InodeI, _Key) ->
    enoent.

delete_xattr(#ffs_tid{ xattr = _XattrTid}, _Key) ->
    enoent.


store_link(#ffs_tid{ link = LinkTid}, Link) ->
    dets:insert(LinkTid, Link).

lookup_links(#ffs_tid{ link = LinkTid}, InodeI) ->
    dets:lookup(LinkTid, InodeI).

delete_link(#ffs_tid{ link = LinkTid }, Link) ->
    dets:delete_object(LinkTid,Link).

