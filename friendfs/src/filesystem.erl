%% @doc Distributed filesystem for Erlang.
%% @end
 
-module(filesystem).
-export([start_link/2, start_link/3]).
%-behaviour (fuserl).
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2,
         
	 access/5,
	 getattr/4,
         lookup/5,
         open/5,
         read/7,
         readdir/7,
         readlink/4,
	 statfs/4 ]).

-include_lib("fuserl/include/fuserl.hrl").

-record(fs, {inodes, names}).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (LinkedIn, MountPoint) ->
  start_link (LinkedIn, MountPoint, "").

start_link (LinkedIn, MountPoint, MountOpts) ->
  io:format("friendfs:start_link ~n"),
  fuserlsrv:start_link (?MODULE, LinkedIn, MountOpts, MountPoint, [], []).


%-=====================================================================-
%-                           fuserl callbacks                          -
%-=====================================================================-

init([]) ->
    io:format("friendfs:init"),
    Inodes = gb_trees:insert(1, "/", gb_trees:empty()),
    Names = gb_trees:insert("/", {1, []}, gb_trees:empty()),
    State = #fs{inodes = Inodes, 
		names = Names},
    { ok, State }.

handle_call(_Request, _From, State) -> {noreply, State}.
handle_cast(_Request, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info({ _, { exit_status, 0 } }, State) ->
    %% well, this is an unmount(8) ... (probably?)
    spawn(fun() -> init:stop() end),
    { stop, normal, State };
handle_info(_Msg, State) -> {noreply, State}.

terminate (_Reason, _State) -> ok.

-define(DIRATTR (X), #stat{st_ino = (X),
			   st_mode = ?S_IFDIR bor 8#0555,
			   st_nlink = 1 }).

-define(LINKATTR, #stat{st_mode = ?S_IFLNK bor 8#0555, 
			st_nlink = 1 }).

-define(DBG(FunName, Str, Vars), 
	io:format(FunName ++ "(Ctx: ~p, "++Str++")~n", 
		  [_Ctx|Vars])).

access(_Ctx, Inode, Mask, _Cont, State) ->
  ?DBG("access", "Inode: ~p, Mask: ~p", [Inode, Mask]),
  {#fuse_reply_err{err=ok}, State}.

getattr(_Ctx, Inode = 1, _Cont, State) ->
    %% Root dir
    ?DBG("getattr", "Inode: ~p", [Inode]),
    {#fuse_reply_attr{attr=?DIRATTR(Inode), attr_timeout_ms=1000}, 
     State};
getattr(_Ctx, Inode, _Cont, State) ->
    ?DBG("getattr", "Inode: ~p", [Inode]),
    {#fuse_reply_err{err = enoent}, State}.

lookup(_Ctx, Parent, BinName, _Cont, State) ->
    Name = erlang:binary_to_list (BinName),
    ?DBG("lookup", "Parent: ~p, Name: ~p", [Parent, Name]),
    case gb_trees:lookup({Parent, Name}, 
			 State#fs.names) of
	Param = #fuse_entry_param{} ->
	    #fuse_reply_entry{ fuse_entry_param = Param };
	{error, Reason} ->
	    #fuse_reply_err{ err = Reason }
    end.

open(_Ctx, X = 1, Fi = #fuse_file_info{}, _Cont, State) ->
    ?DBG("open", "X: ~p, Fi: ~p", [X, Fi]),
  case (Fi#fuse_file_info.flags band ?O_ACCMODE) =/= ?O_RDONLY of
    true ->
      { #fuse_reply_err{ err = eacces }, State };
    false ->
      { #fuse_reply_open{ fuse_file_info = Fi }, State }
  end;
open(_Ctx, X, Fi = #fuse_file_info{}, _Cont, State) ->
    ?DBG("open", "X: ~p, Fi: ~p", [X, Fi]),
    { #fuse_reply_open{ fuse_file_info = Fi }, State }.


read(_Ctx, X, Size, Offset, Fi, _Cont, State) ->
    ?DBG("read", "X: ~p, Size: ~p, Offset: ~p, Fi: ~p", 
	 [X, Size, Offset, Fi]),
    { #fuse_reply_err{ err = enoent }, State }.

readdir(_Ctx, Inode = 1, Size, Offset, Fi, _Cont, State) when Offset < 2 ->
    ?DBG("readdir", "Inode: ~p, Size: ~p, Offset: ~p, Fi: ~p", 
	 [Inode, Size, Offset, Fi]),
    Dir = [#direntry{ name=".", offset=1, stat=?DIRATTR(1)},
	   #direntry{ name="..", offset=2, stat=?DIRATTR(1)}],
    { #fuse_reply_direntrylist{ direntrylist = Dir }, 
      State };
readdir(_Ctx, Inode, Size, Offset, Fi, _Cont, State) ->
    ?DBG("readdir", "Inode: ~p, Size: ~p, Offset: ~p, Fi: ~p", 
	 [Inode, Size, Offset, Fi]),
    { #fuse_reply_direntrylist{ direntrylist = [] }, 
      State }.

readlink(_Ctx, X, _Cont, State) ->
    ?DBG("readlink", "X: ~p", [X]),
    { #fuse_reply_err{ err = enoent }, State }.

statfs(_Ctx, Inode, _Cont, State) ->
    ?DBG("statfs", "Inode: ~p", [Inode]),
    S = #statvfs{ f_bsize=512,
		  f_frsize=512,
		  f_blocks = 0,
		  f_bfree = 0,
		  f_bavail = 0,
		  f_files = 1 bsl 32,
		  f_ffree = 1 bsl 32 - 1,
		  f_favail = 1 bsl 32 - 1,
		  f_fsid = 36#n54,
		  f_flag = 0,
		  f_namemax = 36#sup },
    { #fuse_reply_statfs{ statvfs = S }, State }.
