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
%% Typedefs
%%====================================================================
%% @type ffs_fs_ctx() = #ffs_fs_ctx{}. 
%%     Contains information needed by the fs store
%% @end
%%

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% @spec init(Name, Config) -> ffs_fat_ctx()
%%     Name = atom()
%%     Config = [term()]
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
		    {keypos,#ffs_fs_xattr.inode},{type, bag}]),
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
%% Get next free inode number in filesystem
%%
%% @spec get_new_inode_num(Ctx) -> integer()
%%     Ctx = ffs_fs_ctx()
%% @end
%%--------------------------------------------------------------------
get_new_inode_num(#ffs_fs_ctx{ name = Name }) ->
    ets:update_counter(?COUNTER_TABLE,Name,1).

%%--------------------------------------------------------------------
%% @doc
%% Store the inode information
%%
%% @spec store_inode(Ctx, Inode) -> ok
%%     Ctx = ffs_fs_ctx()
%% @end
%%--------------------------------------------------------------------
store_inode(#ffs_fs_ctx{ inode = InodeCtx }, Inode = #ffs_fs_inode{}) ->
    dets:insert(InodeCtx,Inode).

%%--------------------------------------------------------------------
%% @doc
%% Lookup inode information
%%
%% @spec lookup_inode(Ctx, InodeId) -> Inode | {error, not_found}
%%     Ctx = ffs_fs_ctx()
%%     InodeId = integer()
%% @end
%%--------------------------------------------------------------------
lookup_inode(#ffs_fs_ctx{ inode = InodeCtx }, all) ->
	dets:match_object(InodeCtx, #ffs_fs_inode{ _ = '_' });
lookup_inode(#ffs_fs_ctx{ inode = InodeCtx }, InodeId) ->
    case dets:lookup(InodeCtx, InodeId) of
        [] ->
            {error, not_found};
        [Object] ->
            Object
    end.

%%--------------------------------------------------------------------
%% @doc
%% Delete inode
%%
%% @spec delete_inode(Ctx, InodeId) -> ok | {error, not_found}
%%     Ctx = ffs_fs_ctx()
%%     InodeId = integer()
%% @end
%%--------------------------------------------------------------------
delete_inode(#ffs_fs_ctx{ inode = InodeCtx }, InodeId) ->
    case dets:delete(InodeCtx, InodeId) of
        {error, _} ->
            {error, not_found};
        ok -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Store the Value under Key for this inode information,
%%
%% @spec store_inode(Ctx, InodeId, Key, Value) -> ok
%%     Ctx = ffs_fs_ctx()
%%     InodeId = integer()
%% @end
%%--------------------------------------------------------------------
store_xattr(#ffs_fs_ctx{xattr = XattrCtx }, InodeId, Key, Value) ->
    dets:match_delete(XattrCtx, #ffs_fs_xattr{inode=InodeId,
                                              key=Key,_='_'}),
    dets:insert(XattrCtx, #ffs_fs_xattr{inode=InodeId,
                                        key=Key, value=Value}).

%%--------------------------------------------------------------------
%% @doc
%% Lookup all xattr information for this inode
%%
%% @spec lookup_xattr(Ctx, InodeId) -> [{Key, Value}] | []
%%     Ctx = ffs_fs_ctx()
%%     InodeId = integer()
%% @end
%%--------------------------------------------------------------------
lookup_xattr(#ffs_fs_ctx{xattr = XattrCtx }, InodeId) ->
    dets:lookup(XattrCtx, InodeId).

%%--------------------------------------------------------------------
%% @doc
%% Lookup the value of Key in Xattr for this inode
%%
%% @spec lookup_xattr(Ctx, InodeId, Key) -> Value | {error, not_found}
%%     Ctx = ffs_fs_ctx()
%%     InodeId = integer()
%% @end
%%--------------------------------------------------------------------
lookup_xattr(#ffs_fs_ctx{xattr = XattrCtx }, InodeId, Key) ->
    case dets:match(XattrCtx, #ffs_fs_xattr{inode=InodeId,
                                            key=Key, value='$1',
                                            _='_'}) of
        [] ->
            {error, not_found};
        [[Value]] ->
            Value
    end.

%%--------------------------------------------------------------------
%% @doc
%% Remove a specific xattr key
%%
%% @spec delete_xattr(Ctx, InodeId, Key) -> ok 
%%     Ctx = ffs_fs_ctx()
%%     InodeId = integer()
%% @end
%%--------------------------------------------------------------------
delete_xattr(#ffs_fs_ctx{ xattr = XattrCtx }, InodeId, Key) ->
    dets:match_delete(XattrCtx, #ffs_fs_xattr{inode=InodeId,
                                              key=Key,_='_'}).


%%--------------------------------------------------------------------
%% @doc
%% Remove all xattr for an inode
%%
%% @spec delete_xattr(Ctx, InodeId) -> ok 
%%     Ctx = ffs_fs_ctx()
%%     InodeId = integer()
%% @end
%%--------------------------------------------------------------------
delete_xattr(#ffs_fs_ctx{ xattr = XattrCtx }, InodeId) ->
    dets:match_delete(XattrCtx,#ffs_fs_xattr{inode=InodeId,_='_'}).


%%--------------------------------------------------------------------
%% @doc
%% Store a link
%%
%% @spec store_link(Ctx, Link) -> ok
%%     Ctx = ffs_fs_ctx()
%% @end
%%--------------------------------------------------------------------
store_link(#ffs_fs_ctx{ link = LinkCtx }, Link) ->
    dets:insert(LinkCtx, Link).

%%--------------------------------------------------------------------
%% @doc
%% Lookup links
%%
%% @spec lookup_links(Ctx, InodeId) -> [Link] 
%%     Ctx = ffs_fs_ctx()
%%     InodeId = integer()
%% @end
%%--------------------------------------------------------------------
lookup_links(#ffs_fs_ctx{ link = LinkCtx }, InodeId) ->
    dets:lookup(LinkCtx, InodeId).

%%--------------------------------------------------------------------
%% @doc
%% Remove a link
%%
%% @spec delete_link(Ctx, Link) -> ok 
%%     Ctx = ffs_fs_ctx()
%% @end
%%--------------------------------------------------------------------
delete_link(#ffs_fs_ctx{ link = LinkCtx }, Link) ->
    dets:delete_object(LinkCtx,Link).
