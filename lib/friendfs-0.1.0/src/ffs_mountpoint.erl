%%%-------------------------------------------------------------------
%%% @author Lukas Larsson <garazdawi@gmail.com>
%%% @doc
%%%   Mount Point
%%%
%%% Represents one mountpoint. 
%%%
%%% @end
%%% Created : 19 Aug 2009 by Lukas Larsson <garazdawi@gmail.com>
%%%-------------------------------------------------------------------
-module(ffs_mountpoint).

-include_lib("fuserl/include/fuserl.hrl").
-include_lib("friendfs/include/friendfs.hrl").

%-behaviour(fuserlsrv).

%% API
-export([start_link/2]).

%% fuserlsrv general callbacks
-export([init/1, handle_info/2,terminate/2, code_change/3]).

%% fuserlsrv filesystem callbacks
-export([ access/5,
	  create/7,
	  flush/5,
	  forget/5,
%	  fsync/6,
	  fsyncdir/6,
	  getattr/4,
	  getlk/6,
	  getxattr/6,
	  link/6,
	  listxattr/5,
	  lookup/5,
	  mkdir/6,
%	  mknod/7,
	  open/5,
	  opendir/5,
	  read/7,
	  readdir/7,
	  readlink/4,
%	  release/5,
%	  releasedir/5,
	  removexattr/5,
	  rename/7,
	  rmdir/5,
	  setattr/7,
	  setlk/7,
	  setxattr/7,
	  statfs/4,
	  symlink/6,
%	  unlink/5,
	  write/7
	]).

-record(state,{ filesystem }).

-define(DBG(Str), io:format("Ctx = ~p\n"++Str++"\n",[_Ctx])).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts a new fuserlsrv which handles a mountpoint.
%%
%% @spec start_link(MountPoint,Options) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(MountPoint,Options) ->
    {MountOpts,FfsOpts} = parse_options(Options),
    {ok,LinkedIn} = application:get_env(friendfs,linked_in),
    fuserlsrv:start_link(?MODULE,LinkedIn,MountOpts,
			 MountPoint,FfsOpts,[]).

%%%===================================================================
%%% fuserlsrv filesystem callbacks
%%%===================================================================

%% @spec access (Ctx::#fuse_ctx{}, Inode::integer (), Mask::access_mode (), Cont::continuation (), State) -> { access_async_reply (), NewState } | { noreply, NewState } 
%%  access_async_reply () = #fuse_reply_err{}
%% @doc Check file access permissions.  Mask is a bitmask consisting of
%% ?F_OK, ?X_OK, ?W_OK, and ?R_OK, which are portably defined in fuserl.hrl .
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type access_async_reply ().
%% @end

access (_Ctx, _Inode, _Mask, _Cont, State) -> ?DBG("access called"),
	{#fuse_reply_err{err=ok}, State}.


%% @spec create (Ctx::#fuse_ctx{}, Parent::integer (), Name::binary (), Mode::create_mode (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { create_async_reply (), NewState } | { noreply, NewState } 
%%  create_async_reply () = #fuse_reply_create{} | #fuse_reply_err{}
%% @doc Create and open a file.
%% Mode is a bitmask consisting of ?S_XXXXX macros portably defined in 
%% fuserl.hrl .
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type create_async_reply ().
%% @end

create (#fuse_ctx{ uid = Uid, gid = Gid} = _Ctx, ParentI,
	BinName, Mode, Fi, _Cont, State) -> 
    ?DBG("create called"),
    NewInode = ffs_filesystem:create(
		 State#state.filesystem,ParentI,binary_to_list(BinName),
		 Uid,Gid,to_ffs_mode(Mode)),
    Param = inode_to_param(NewInode),
    {#fuse_reply_create{fuse_entry_param = Param, fuse_file_info = Fi},State}.

%% @spec flush (Ctx::#fuse_ct x{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { flush_async_reply (), NewState } | { noreply, NewState } 
%%  flush_async_reply () = #fuse_reply_err{}
%% @doc This is called on each close () of an opened file, possibly multiple
%% times per {@link open/4. open} call (due to dup () et. al.).  
%% Fi#fuse_file_info.fh will contain the descriptor
%% set in {@link open/4. open}, if any.  
%% #fuse_reply_err{err = ok} indicates success.
%% This return value is ultimately the return value of close ()
%% (unlike {@link release/4. release}).
%% Does *not* necessarily imply an fsync.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type flush_async_reply ().
%% @end

flush (_Ctx, _Inode, _Fi, _Cont, State) -> ?DBG("flush called"),
  {#fuse_reply_err{err = ok},State}.

%% @spec forget (Ctx::#fuse_ctx{}, Inode::integer (), Nlookup::integer (), Cont::continuation (), State) -> { forget_async_reply (), NewState } | { noreply, NewState } 
%%  forget_async_reply () = #fuse_reply_none{}
%% @doc Forget about an inode.  If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type forget_async_reply ().
%% @end

forget (_Ctx, _Inode, _Nlookup, _Cont, _State) -> ?DBG("forget called"),
  erlang:throw (not_implemented).

%% @spec fsync (Ctx::#fuse_ctx{}, Inode::integer (), IsDataSync::bool (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { fsync_async_reply (), NewState } | { noreply, NewState } 
%%  fsync_async_reply () = #fuse_reply_err{}
%% @doc Ensure all changes are on permanent storage.  If the IsDataSync is
%% true, only the user data should be flushed, not the meta data.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type fsync_async_reply ().
%% @end

fsync (_Ctx, _Inode, _IsDataSync, _Fi, _Cont, _State) -> ?DBG("fsync called"),
  erlang:throw (not_implemented).

%% @spec fsyncdir (Ctx::#fuse_ctx{}, Inode::integer (), IsDataSync::bool (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { fsyncdir_async_reply (), NewState } | { noreply, NewState } 
%%  fsyncdir_async_reply () = #fuse_reply_err{}
%% @doc Ensure all directory changes are on permanent storage.  
%% If the IsDataSync is
%% true, only the user data should be flushed, not the meta data.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type fsyncdir_async_reply ().
%% @end

fsyncdir (_Ctx, _Inode, _IsDataSync, _Fi, _Cont, _State) -> ?DBG("fsyncdir called"),
  erlang:throw (not_implemented).

%% @spec getattr (Ctx::#fuse_ctx{}, Inode::integer (), Cont::continuation (), State) -> { getattr_async_reply (), NewState } | { noreply, NewState } 
%%  getattr_async_reply () = #fuse_reply_attr{} | #fuse_reply_err{}
%% @doc Get the file attributes associated with an inode.  If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type getattr_async_reply ().
%% @end

getattr (_Ctx, InodeI, _Cont, State) -> 
	?DBG("getattr called"),
	case ffs_filesystem:lookup(State#state.filesystem,InodeI) of
		enoent ->
			{#fuse_reply_err{ err = enoent}, State};
		Inode ->
			{#fuse_reply_attr{ attr= stat(Inode), 
							   attr_timeout_ms=1000}, State}
	end.


%% @spec getlk (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Lock::#flock{}, Cont::continuation (), State) -> { getlk_async_reply (), NewState } | { noreply, NewState } 
%%  getlk_async_reply () = #fuse_reply_lock{} | #fuse_reply_err{}
%% @doc Test for POSIX file lock.  If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type getlk_async_reply ().
%% @end

getlk (_Ctx, _Inode, _Fi, _Lock, _Cont, _State) -> ?DBG("getlk called"),
  erlang:throw (not_implemented).

%% @spec getxattr (Ctx::#fuse_ctx{}, Inode::integer (), Name::binary (), Size::integer (), Cont::continuation (), State) -> { getxattr_async_reply (), NewState } | { noreply, NewState } 
%%  getxattr_async_reply () = #fuse_reply_buf{} | #fuse_reply_xattr{} | #fuse_reply_err{}
%% @doc Get the value of an extended attribute.  
%% If Size is zero, the size of the value should be sent with
%% #fuse_reply_xattr{}.
%% If Size is non-zero, and the value fits in the buffer, the
%% value should be sent with #fuse_reply_buf{}.
%% If Size is too small for the value, the erange error should
%% be sent.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type getxattr_async_reply ().
%% @end

getxattr (_Ctx, _Inode, _Name, _Size, _Cont, State) ->
    ?DBG("getxattr called"),
    {#fuse_reply_xattr{ count = 0 },State}.

%% @spec link (Ctx::#fuse_ctx{}, Ino::integer (), NewParent::integer (), NewName::binary (), Cont::continuation (), State) -> { link_async_reply (), NewState } | { noreply, NewState }
%%  link_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Create a hard link.  Ino is the existing inode.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type link_async_reply ().
%% @end

link (_Ctx, _Ino, _NewParent, _NewName, _Cont, _State) -> ?DBG("link called"),
  erlang:throw (not_implemented).

%% @spec listxattr (Ctx::#fuse_ctx{}, Ino::integer (), Size::integer (), Cont::continuation (), State) -> { listxattr_async_reply (), NewState } | { noreply, NewState }
%%  listxattr_async_reply () = #fuse_reply_buf{} | #fuse_reply_xattr{} | #fuse_reply_err{}
%% @doc List extended attribute names.  If Size is zero, the total size 
%% in bytes of the attribute name list (including null terminators)
%% should be sent via #fuse_reply_xattr{}.  
%% If the Size is non-zero, and the null character separated and terminated
%% attribute list is Size or less, the list should be sent with 
%% #fuse_reply_buf{}.
%% If Size is too small for the value, the erange error should
%% be sent.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type listxattr_async_reply ().
%% @end

listxattr (_Ctx, _Ino, _Size, _Cont, _State) -> ?DBG("listxattr called"),
  erlang:throw (not_implemented).

%% @spec lookup (Ctx::#fuse_ctx{}, ParentInode::integer (), Name::binary (), Cont::continuation (), State) -> { lookup_async_reply (), NewState } | { noreply, NewState }
%%  lookup_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Lookup a directory entry by name and get its attributes.  Returning
%% an entry with inode zero means a negative entry which is cacheable, whereas
%% an error of enoent is a negative entry which is not cacheable.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type lookup_async_reply ().
%% @end

lookup (_Ctx, ParentInodeI, BinName, _Cont, State) ->
    ?DBG("lookup called"),
    Name = erlang:binary_to_list (BinName),
    case ffs_filesystem:find(State#state.filesystem,ParentInodeI,Name) of
	enoent ->
	    {#fuse_reply_err{ err = enoent },State};
	Inode ->
	    {#fuse_reply_entry
	     { fuse_entry_param =
	       inode_to_param(Inode) },
	     State}
    end.
    
  

%% @spec mkdir (Ctx::#fuse_ctx{}, ParentInode::integer (), Name::binary (), Mode::stat_mode (), Cont::continuation (), State) -> { mkdir_async_reply (), NewState } | { noreply, NewState }
%%  mkdir_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Make a directory.  Mode is a mask composed of the ?S_XXXXX macros
%% which are (portably) defined in fuserl.hrl.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type mkdir_async_reply ().
%% @end

mkdir (_Ctx, _ParentInode, _Name, _Mode, _Cont, _State) -> ?DBG("mkdir called"),
  erlang:throw (not_implemented).

%% @spec mknod (Ctx::#fuse_ctx{}, ParentInode::integer (), Name::binary (), Mode::stat_mode (), Dev::device (), Cont::continuation (), State) -> { mknod_async_reply (), NewState } | { noreply, NewState }
%%  device () = { Major::integer (), Minor::integer () }
%%  mknod_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Create a file node.  Mode is a mask composed of the ?S_XXXXX macros
%% which are (portably) defined in fuserl.hrl.  Dev is only valid if the 
%% created file is a device.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type mknod_async_reply ().
%% @end

mknod (_Ctx, _ParentInode, _Name, _Mode, _Dev, _Cont, _State) -> ?DBG("mknod called"),
  erlang:throw (not_implemented).

%% @spec open (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { open_async_reply (), NewState } | { noreply, NewState }
%%   open_async_reply () = #fuse_reply_open{} | #fuse_reply_err{}
%% @doc Open an inode. If noreply is used, eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} 
%% should be called with Cont as first argument and the second argument of type
%% open_async_reply ().
%% @end

open (_Ctx, _Inode, _Fi, _Cont, State) -> ?DBG("open called"),
  {#fuse_reply_open{ fuse_file_info = #fuse_file_info{flags = 66,
                                                       writepage = false,
													   direct_io = false,
													   keep_cache = false,
                                                       flush = false,fh = 0,lock_owner = 0} },State}.

%% @spec opendir (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { opendir_async_reply (), NewState } | { noreply, NewState }
%%   opendir_async_reply () = #fuse_reply_open{} | #fuse_reply_err{}
%% @doc Open an directory inode. If noreply is used, eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} 
%% should be called with Cont as first argument and the second argument of type
%% opendir_async_reply ().
%% @end

opendir (_Ctx, _Inode, Fi, _Cont, State) ->
	    ?DBG("opendir called"),
    {#fuse_reply_open{ fuse_file_info = Fi},State}.

%% @spec read (Ctx::#fuse_ctx{}, Inode::integer (), Size::integer (), Offset::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { read_async_reply (), NewState } | { noreply, NewState }
%%   read_async_reply () = #fuse_reply_buf{} | #fuse_reply_err{}
%% @doc Read Size bytes starting at offset Offset. The file descriptor and
%% other flags are available in Fi. If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type read_async_reply ().
%% @end

read (_Ctx, _Inode, _Size, _Offset, _Fi, _Cont, _State) -> ?DBG("read called"),
  erlang:throw (not_implemented).

%% @spec readdir (Ctx::#fuse_ctx{}, Inode::integer (), Size::integer (), Offset::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { readdir_async_reply (), NewState } | { noreply, NewState }
%%   readdir_async_reply () = #fuse_reply_direntrylist{} | #fuse_reply_err{}
%% @doc Read at most Size bytes at offset Offset from the directory identified
%% Inode.  Size is ``real'' and must be honored: the function 
%% fuserlsrv:dirent_size/1 can be used to compute the aligned byte size of 
%% a direntry, and the size of the list is the sum of the individual sizes.
%% Offsets, however,
%% are ``fake'', and are for the convenience of the implementation to find
%% a specific point in the directory stream.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type readdir_async_reply ().
%% @end

readdir (_Ctx, InodeI, _Size, Offset, _Fi, _Cont, State) ->
    ?DBG("readdir called"),
    FfsList = ffs_filesystem:list(State#state.filesystem,InodeI),
    {List,_} = lists:mapfoldl(
		 fun({Name,Inode},Offset) -> 
			 {#direntry{ name = Name,
				     offset = Offset,
				     stat = stat(Inode)},
			  Offset+1}
		 end,Offset+1,lists:nthtail(Offset,FfsList)),
    {#fuse_reply_direntrylist{ direntrylist = List },State}.


%% @spec readlink (Ctx::#fuse_ctx{}, Inode::integer (), Cont::continuation (), State) -> { readlink_async_reply (), NewState } | { noreply, NewState }
%%   readlink_async_reply () = #fuse_reply_readlink{} | #fuse_reply_err{}
%% @doc Read the contents of a symbolic link.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type readlink_async_reply ().
%% @end

readlink (_Ctx, _Inode, _Cont, _State) -> ?DBG("readlink called"),
  erlang:throw (not_implemented).

%% @spec release (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { release_async_reply (), NewState } | { noreply, NewState } 
%%  release_async_reply () = #fuse_reply_err{}
%% @doc Called when there are no more references to a file.
%% For every {@link open/4. open} call there is exactly one release call.
%% Fi#fuse_file_info.fh will contain the descriptor
%% set in {@link open/4. open}, if any.  
%% Fi#fuse_file_info.flags will contain the same flags as for 
%% {@link open/4. open}.
%% #fuse_reply_err{err = ok} indicates success.
%% Errors are not reported anywhere; use {@link flush/4. flush} for that.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type release_async_reply ().
%% @end

release (_Ctx, _Inode, _Fi, _Cont, _State) -> ?DBG("release called"),
  erlang:throw (not_implemented).

%% @spec releasedir (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { releasedir_async_reply (), NewState } | { noreply, NewState } 
%%  releasedir_async_reply () = #fuse_reply_err{}
%% @doc Called when there are no more references to a directory.
%% For every {@link opendir/4. opendir} call there is exactly one releasedir call.
%% Fi#fuse_file_info.fh will contain the descriptor
%% set in {@link opendir/4. opendir}, if any.  
%% Fi#fuse_file_info.flags will contain the same flags as for 
%% {@link opendir/4. opendir}.
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type releasedir_async_reply ().
%% @end

releasedir (_Ctx, _Inode, _Fi, _Cont, _State) -> ?DBG("releasedir called"),
  erlang:throw (not_implemented).

%% @spec removexattr (Ctx::#fuse_ctx{}, Inode::integer (), Name::binary (), Cont::continuation (), State) -> { removexattr_async_reply (), NewState } | { noreply, NewState }
%%   removexattr_async_reply () = #fuse_reply_err{}
%% @doc Remove an extended attribute.  
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type removexattr_async_reply ().
%% @end

removexattr (_Ctx, _Inode, _Name, _Cont, _State) -> ?DBG("removexattr called"),
  erlang:throw (not_implemented).

%% @spec rename (Ctx::#fuse_ctx{}, Parent::integer (), Name::binary (), NewParent::integer (), NewName::binary (), Cont::continuation (), State) -> { rename_async_reply (), NewState } | { noreply, NewState }
%%   rename_async_reply () = #fuse_reply_err{}
%% @doc Rename a file.  #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type rename_async_reply ().
%% @end

rename (_Ctx, _Parent, _Name, _NewParent, _NewName, _Cont, State) ->
    ?DBG("rename called"),
    {#fuse_reply_err{err = ok},State}.

%% @spec rmdir (Ctx::#fuse_ctx{}, Inode::integer (), Name::binary (), Cont::continuation (), State) -> { rmdir_async_reply (), NewState } | { noreply, NewState }
%%   rmdir_async_reply () = #fuse_reply_err{}
%% @doc Remove a directory.  #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type rmdir_async_reply ().
%% @end

rmdir (_Ctx, _Inode, _Name, _Cont, _State) -> ?DBG("rmdir called"),
  erlang:throw (not_implemented).

%% @spec setattr (Ctx::#fuse_ctx{}, Inode::integer (), Attr::#stat{}, ToSet::integer (), Fi::maybe_fuse_file_info (), Cont::continuation (), State) -> { setattr_async_reply (), NewState } | { noreply, NewState }
%%   maybe_fuse_file_info () = #fuse_file_info{} | null
%%   setattr_async_reply () = #fuse_reply_attr{} | #fuse_reply_err{}
%% @doc Set file attributes.  ToSet is a bitmask which defines which 
%% elements of Attr are defined and should be modified.  Possible values
%% are defined as ?FUSE_SET_ATTR_XXXX in fuserl.hrl . Fi will be set
%% if setattr is invoked from ftruncate under Linux 2.6.15 or later.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type setattr_async_reply ().
%% @end

setattr (_Ctx, InodeI, Attr, ToSet, _Fi, _Cont, State) -> ?DBG("setattr called"),
  lists:foreach(fun %(?FUSE_SET_ATTR_MTIME) when ?FUSE_SET_ATTR_MTIME bor ToSet =/= 0 ->
					%	ffs_filesystem:modify(State#state.filesystem,InodeI,unix_to_now(Attr#stat.st_mtime));
					%(?FUSE_SET_ATTR_ATIME) when ?FUSE_SET_ATTR_ATIME bor ToSet =/= 0 ->
					%	ffs_filesystem:access(State#state.filesystem,InodeI,unix_to_now(Attr#stat.st_atime));
					(_) ->
						ok
				end,[	?FUSE_SET_ATTR_MODE,
						?FUSE_SET_ATTR_UID,
						?FUSE_SET_ATTR_GID,
						?FUSE_SET_ATTR_SIZE,
						?FUSE_SET_ATTR_ATIME,
						?FUSE_SET_ATTR_MTIME]),
	Inode = ffs_filesystem:lookup(State#state.filesystem,InodeI),
  {#fuse_reply_attr{ attr = stat(Inode), attr_timeout_ms = 0 },State}.

%% @spec setlk (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Lock::#flock{}, Sleep::bool(), Cont::continuation (), State) -> { setlk_async_reply (), NewState } | { noreply, NewState }
%%   setlk_async_reply () = #fuse_reply_err{}
%% @doc Set a POSIX file lock.  Sleep indicates whether the operation is 
%% blocking (true) or nonblocking (false).
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type setlk_async_reply ().
%% @end

setlk (_Ctx, _Inode, _Fi, _Lock, _Sleep, _Cont, _State) -> ?DBG("setlk called"),
  erlang:throw (not_implemented).

%% @spec setxattr (Ctx::#fuse_ctx{}, Inode::integer (), Name::binary (), Value::binary (), Flags::xattr_flags (), Cont::continuation (), State) -> { setxattr_async_reply (), NewState } | { noreply, NewState }
%%   setxattr_async_reply () = #fuse_reply_err{}
%% @doc Set file attributes.  
%% #fuse_reply_err{err = ok} indicates success.
%% Flags is a bitmask consisting of ?XATTR_XXXXX macros portably defined in
%% fuserl.hrl .
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called 
%% with Cont as first argument
%% and the second argument of type setxattr_async_reply ().
%% @end

setxattr (_Ctx, _Inode, _Name, _Value, _Flags, _Cont, _State) -> ?DBG("setxattr called"),
  erlang:throw (not_implemented).

%% @spec statfs (Ctx::#fuse_ctx{}, Inode::integer (), Cont::continuation (), State) -> { statfs_async_reply (), NewState } | { noreply, NewState }
%%   statfs_async_reply () = #fuse_reply_statfs{} | #fuse_reply_err{}
%% @doc Get file system statistics.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type statfs_async_reply ().
%% @end

statfs (_Ctx, _Inode, _Cont, #state{ filesystem = FS} = State) ->
    ?DBG("statfs called"),
    
    Config = ffs_filesystem:get_config(FS),
    Stats  = ffs_filesystem:get_stats(FS),

    BSize             = ffs_lib:get_value(Config,block_size),
    TotInodes         = ffs_lib:get_value(Config,inode_limit),
    FilesystemId      = ffs_lib:get_value(Config,filesystem_id),
    MntOpts           = ffs_lib:get_value(Config,mnt_opts),
    MaxFilenameLength = ffs_lib:get_value(Config,max_filename_size),
    
    TotMem        = ffs_lib:get_value(Stats,total_mem),
    FreeMem       = ffs_lib:get_value(Stats,free_mem),
    FreeInodes    = ffs_lib:get_value(Stats,free_inodes),
    
    S = #statvfs{ f_bsize=BSize,
		  f_frsize=BSize,
		  f_blocks = TotMem div BSize,
		  f_bfree = FreeMem div BSize,
		  f_bavail = FreeMem div BSize,
		  f_files = TotInodes,
		  f_ffree = FreeInodes,
		  f_favail = FreeInodes,
		  f_fsid = FilesystemId,
		  f_flag = MntOpts,
		  f_namemax = MaxFilenameLength },
    { #fuse_reply_statfs{ statvfs = S }, State }.


%% @spec symlink (Ctx::#fuse_ctx{}, Link::binary (), Inode::integer (), Name::binary (), Cont::continuation (), State) -> { symlink_async_reply (), NewState } | { noreply, NewState }
%%   symlink_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Create a symbolic link.  Link is the contents of the link.  Name is 
%% the name to create.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type symlink_async_reply ().
%% @end

symlink (_Ctx, _Link, _Inode, _Name, _Cont, _State) -> ?DBG("symlink called"),
  erlang:throw (not_implemented).

%% @spec write (Ctx::#fuse_ctx{}, Inode::integer (), Data::binary (), Offset::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { write_async_reply (), NewState } | { noreply, NewState }
%%   write_async_reply () = #fuse_reply_write{} | #fuse_reply_err{}
%% @doc Write data to a file.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type write_async_reply ().
%% @end

write (_Ctx, _Inode, Data, _Offset, _Fi, _Cont, State) -> 
  ?DBG("write called"),
  {#fuse_reply_write{ count = size(Data) },State}.

%% @spec unlink (Ctx::#fuse_ctx{}, Inode::integer (), Name::binary (), Cont::continuation (), State) -> { unlink_async_reply (), NewState } | { noreply, NewState }
%%   unlink_async_reply () = #fuse_reply_err{}
%% @doc Remove a file.  #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type unlink_async_reply ().
%% @end

unlink (_Ctx, _Inode, _Name, _Cont, _State) -> ?DBG("unlink called"),
  erlang:throw (not_implemented).

%%%===================================================================
%%% fuserlsrv general callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(FfsOpts) ->
    Filesystem = list_to_atom(proplists:get_value("fs",FfsOpts)),
    link(whereis(Filesystem)),
    {ok, #state{ filesystem = Filesystem }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT',_Pid,killed},State) ->
    {stop,killed,State};
handle_info(_Info, State) ->
    io:format("~p: Unknown handle_info(~p,~p) call\n",[?MODULE,_Info,State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a fuserlsrv when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the fuserlsrv terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




%%%===================================================================
%%% Internal functions
%%%==================================================================

parse_options(String) ->
    Tokens = string:tokens(String,","),
    parse_options(Tokens,"",[]).
parse_options(["fs="++Filesystem|T],MountAcc,FfsAcc) ->
    parse_options(T,MountAcc,[{"fs",Filesystem}|FfsAcc]);
parse_options([Key|T],MountAcc,FfsAcc) ->
    parse_options(T,Key++MountAcc,FfsAcc);
parse_options([],MountAcc,FfsAcc) ->
    {MountAcc,FfsAcc}.

to_unix({Mega,Secs,_Micro}) ->
	Mega*1000000+Secs.
	
unix_to_now(Secs) ->
	{Secs div 1000000, Secs rem 1000000,0}.
	
inode_to_param(#ffs_inode{inode = InodeI} = Inode) ->
	#fuse_entry_param{ ino = InodeI,
			  generation = 1,%element(3,now()),
			  attr = stat(Inode),
			  attr_timeout_ms = 100,
			  entry_timeout_ms = 100}.
	
stat(#ffs_inode{ inode = Inode, gid = Gid, uid = Uid,
		 atime = Atime, mtime = Mtime,
		 ctime = Ctime, refcount = Links,
		 size = Size, mode = Mode } )  ->					
    #stat{
%      st_dev = { 0, 0 },                  % ID of device containing file 
      st_ino = Inode,                     % inode number 
      st_mode = to_fuse_mode(Mode),       % protection 
      st_nlink = Links,                   % number of hard links 
      st_uid = Uid,                       % user ID of owner 
      st_gid = Gid,                       % group ID of owner 
%      st_rdev = { 0, 0 },                 % device ID (if special file) 
      st_size = Size,                     % total size = 0, in bytes 
      st_blksize = 0,                     % blocksize for filesystem I/O 
      st_blocks = 0,                      % number of blocks allocated 
      st_atime = to_unix(Atime),          % time of last access 
      st_mtime = to_unix(Mtime),          % time of last modification 
      st_ctime = to_unix(Ctime)           % time of last status change
     }.

-define(MAP_BITS,[{?D,?S_IFDIR},
				  {?F,?S_IFREG},
				  {?U_R,?S_IRUSR},
				  {?U_W,?S_IWUSR},
				  {?U_X,?S_IXUSR},
				  {?G_R,?S_IRGRP},
				  {?G_W,?S_IWGRP},
				  {?G_X,?S_IXGRP},
				  {?O_R,?S_IROTH},
				  {?O_W,?S_IWOTH},
				  {?O_X,?S_IXOTH}]).

to_ffs_mode(Mode) ->
	lists:foldl(fun({FfsBit,FuseBit},FuseMode) when (FuseBit band Mode) =/= 0 ->
					FuseMode bor FfsBit;
				   (_Else,FuseMode) ->
					FuseMode
				end,0,?MAP_BITS).
	
	
to_fuse_mode(Mode) ->
	lists:foldl(fun({FfsBit,FuseBit},FuseMode) when (FfsBit band Mode) =/= 0 ->
					FuseMode bor FuseBit;
				   (_Else,FuseMode) ->
					FuseMode
				end,0,?MAP_BITS).
	
	
