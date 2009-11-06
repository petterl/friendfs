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
    ets:insert(?COUNTER_TABLE,{Name,0}),
	AName = list_to_atom(Name),
    Tid = #ffs_tid{ 
      name = Name,
      inode = ets:new(AName,[{keypos,#ffs_inode.inode},public,set]),
      link = ets:new(AName,[{keypos,#ffs_link.from},public,bag]),
      xattr = ets:new(AName,[{keypos,#ffs_xattr.inode},public,set]),
      config = [{chunkid_mfa,{ffs_lib,get_chunkid}}]},
    create(Tid,1,"..",Uid,Gid,?D bor Mode,0,0),
    Tid.

get_new_inode(Tid) ->
    ets:update_counter(?COUNTER_TABLE,Tid#ffs_tid.name,1).


add_node(ffs_tid{ inode = InodeTid, xattr = XattrTid}, Inode, Xattr) ->
    ets:insert(InodeTid,NewInode),
    ets:insert(XattrTid,Xattr).
    

