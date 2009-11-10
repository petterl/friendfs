%%%-------------------------------------------------------------------
%%% File    : ffs_fat_store.erl
%%% Author  : Petter Sandholdt <petter@sandholdt.se>
%%% Description : The abstract storage of the fat
%%%
%%% Created : 6 Nov 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_fat_store).

%% API
-export([init/2,
	 stop/1,
	 get_new_inode_num/1,
	 store_inode/2,
	 lookup_inode/2,
	 delete_inode/2,
	 store_xattr/2,
	 store_xattr/4,
	 lookup_xattr/2,
	 lookup_xattr/3,
	 delete_xattr/2,
	 delete_xattr/3,
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
%% @spec init(Name,Config) -> ffs_fat_ctx()
%%     Name = atom()
%%     Config = term()
%% @end
%%--------------------------------------------------------------------
init(Name, Config) ->
    case ets:info(?COUNTER_TABLE) of
	undefined ->
	    ets:new(?COUNTER_TABLE,[public,named_table,{keypos,1},set]);
	_ ->
	    ok
    end,

    ets:insert(?COUNTER_TABLE,{Name,0}),
    dets:open_file(list_to_atom(Name++"I"),
		   [{file, "./"++Name++"I"},
		    {keypos,#ffs_fs_inode.inode},{type, set}]),
    dets:open_file(list_to_atom(Name++"L"),
		   [{file, "./"++Name++"L"},
		    {keypos,#ffs_fs_link.from},{type, bag}]),
    dets:open_file(list_to_atom(Name++"X"),
		   [{file, "./"++Name++"X"},
		    {keypos,#ffs_fs_xattr.inode},{type, set}]),
    Ctx = #ffs_fs_ctx{ 
      name = Name,
      inode = list_to_atom(Name++"I"),
      link = list_to_atom(Name++"L"),
      xattr = list_to_atom(Name++"X"),
      config = [{chunkid_mfa,{ffs_lib,get_chunkid}}]++Config},
    Ctx.

stop(#ffs_fs_ctx{name = Name}) ->
      dets:close(list_to_atom(Name++"I")),
      dets:close(list_to_atom(Name++"L")),
      dets:close(list_to_atom(Name++"X")).


%%--------------------------------------------------------------------
%% @doc
%%
%% @spec get_new_inode_num(Ctx) -> integer()
%%     Ctx = ffs_fs_ctx{}
%% @en
%%--------------------------------------------------------------------
get_new_inode_num(#ffs_fs_ctx{ name = Name}) ->
    ets:update_counter(?COUNTER_TABLE,Name,1).

store_inode(#ffs_fs_ctx{ inode = InodeTid}, Inode) ->
    dets:insert(InodeTid,Inode).

lookup_inode(#ffs_fs_ctx{ inode = InodeTid}, InodeI) ->
    dets:lookup(InodeTid, InodeI).

delete_inode(#ffs_fs_ctx{ inode = InodeTid}, InodeI) ->
    dets:delete(InodeTid,InodeI).


store_xattr(#ffs_fs_ctx{xattr = XattrTid}, Xattr) ->
    dets:insert(XattrTid,Xattr).
    
store_xattr(#ffs_fs_ctx{xattr = _XattrTid}, _InodeI, _Key, _Value) ->
    enoent.
    
lookup_xattr(#ffs_fs_ctx{xattr = XattrTid}, InodeI) ->
    dets:lookup(XattrTid, InodeI).    

lookup_xattr(#ffs_fs_ctx{xattr = _XattrTid}, _InodeI, _Key) ->
    enoent.

delete_xattr(#ffs_fs_ctx{ xattr = XattrTid}, InodeI) ->
    dets:delete(XattrTid,InodeI).

delete_xattr(#ffs_fs_ctx{ xattr = _XattrTid}, _InodeI, _Key) ->
    enoent.

store_link(#ffs_fs_ctx{ link = LinkTid}, Link) ->
    dets:insert(LinkTid, Link).

lookup_links(#ffs_fs_ctx{ link = LinkTid}, InodeI) ->
    dets:lookup(LinkTid, InodeI).

delete_link(#ffs_fs_ctx{ link = LinkTid }, Link) ->
    dets:delete_object(LinkTid,Link).
