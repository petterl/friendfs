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
-include_lib("friendfs/include/debug.hrl").
-include_lib("kernel/include/file.hrl").

%-behaviour(fuserlsrv).

%% API
-export([start_link/2]).

%% fuserlsrv general callbacks
-export([init/1, handle_info/2,terminate/2, code_change/3]).

%% fuserlsrv filesystem callbacks
-export([ access/5,
	  create/7,
	  flush/5,
%	  forget/5,
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
 	  release/5,
 	  releasedir/5,
	  removexattr/5,
	  rename/7,
	  rmdir/5,
	  setattr/7,
	  setlk/7,
	  setxattr/7,
	  statfs/4,
	  symlink/6,
	  unlink/5,
	  write/7
	]).

-record(state,{ filesystem, default_uid, default_gid }).

-define(DBG2(Str), ?DBG("Ctx = ~p~n~p~n", [Ctx, Str])).

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

    {ok,FileInfo} = file:read_file_info(MountPoint),
    
    ParentGid = integer_to_list(FileInfo#file_info.gid),
    ParentUid = integer_to_list(FileInfo#file_info.uid),
    
    DefaultGid = list_to_integer(proplists:get_value("gid",FfsOpts,ParentGid)),
    DefaultUid = list_to_integer(proplists:get_value("uid",FfsOpts,ParentUid)),
    
    {ok,LinkedIn} = application:get_env(friendfs,linked_in),
    fuserlsrv:start_link(?MODULE,LinkedIn,MountOpts,
			 MountPoint,{MountPoint,DefaultUid,DefaultGid,FfsOpts},
			 []).

%%%===================================================================
%%% fuserlsrv filesystem callbacks
%%%===================================================================

%% @spec access(Ctx::#fuseCtx{}, Inode::integer (), Mask::access_mode (), Cont::continuation (), State) -> { access_async_reply (), NewState } | { noreply, NewState } 
%%  access_async_reply () = #fuse_reply_err{}
%% @doc Check file access permissions.  Mask is a bitmask consisting of
%% ?F_OK, ?X_OK, ?W_OK, and ?R_OK, which are portably defined in fuserl.hrl .
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type access_async_reply ().
%% @end

access(Ctx, InodeI, Mask, Cont, State) -> 
    async(fun()-> access_async(Ctx,InodeI,Mask,State) end,Cont,State).

access_async(Ctx,InodeI,Mask,State) ->
    ?DBG2("access called"),
    case ffs_filesystem:lookup(State#state.filesystem,InodeI) of
	enoent ->
	    #fuse_reply_err{err=enoent};
	_Inode when Mask == ?F_OK ->
	    #fuse_reply_err{err=ok};
	Inode ->
	    Uid = get_uid(Inode#ffs_fs_inode.uid,State),
	    Gid = get_gid(Inode#ffs_fs_inode.gid,State),
	    UpdateInode = Inode#ffs_fs_inode{ uid = Uid,
					   gid = Gid },
	    #fuse_reply_err{err=get_access(Ctx,Mask,UpdateInode)}
    end.

%% Check rights for owner
get_access(#fuse_ctx{ uid = Uid },?X_OK,#ffs_fs_inode{ uid = Uid , mode = Mask }) 
  when (Mask band ?U_X) =/= 0 ->
    ok;
get_access(#fuse_ctx{ uid = Uid },?R_OK,#ffs_fs_inode{ uid = Uid , mode = Mask }) 
  when (Mask band ?U_R) =/= 0 ->
    ok;
get_access(#fuse_ctx{ uid = Uid },?W_OK,#ffs_fs_inode{ uid = Uid , mode = Mask }) 
  when (Mask band ?U_W) =/= 0 ->
    ok;
%% Check rights for group
get_access(#fuse_ctx{ gid = Gid },?X_OK,#ffs_fs_inode{ gid = Gid , mode = Mask }) 
  when (Mask band ?G_X) =/= 0 ->
    ok;
get_access(#fuse_ctx{ gid = Gid },?R_OK,#ffs_fs_inode{ gid = Gid , mode = Mask }) 
  when (Mask band ?G_R) =/= 0 ->
    ok;
get_access(#fuse_ctx{ gid = Gid },?W_OK,#ffs_fs_inode{ gid = Gid , mode = Mask }) 
  when (Mask band ?G_W) =/= 0 ->
    ok;
%% Check rights for Others
get_access(#fuse_ctx{ },?X_OK,#ffs_fs_inode{ mode = Mask }) 
  when (Mask band ?O_X) =/= 0 ->
    ok;
get_access(#fuse_ctx{ },?R_OK,#ffs_fs_inode{ mode = Mask }) 
  when (Mask band ?O_R) =/= 0 ->
    ok;
get_access(#fuse_ctx{ },?W_OK,#ffs_fs_inode{ mode = Mask }) 
  when (Mask band ?O_W) =/= 0 ->
    ok;
get_access(_,_,_) ->
    eacces.



%% @spec create(Ctx::#fuseCtx{}, Parent::integer (), Name::binary (), Mode::create_mode (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { create_async_reply (), NewState } | { noreply, NewState } 
%%  create_async_reply () = #fuse_reply_create{} | #fuse_reply_err{}
%% @doc Create and open a file.
%% Mode is a bitmask consisting of ?S_XXXXX macros portably defined in 
%% fuserl.hrl .
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type create_async_reply ().
%% @end

create(Ctx,ParentI,BinName,Mode,Fi,Cont,State) ->
    async(fun()->
		  create_async(Ctx,ParentI,BinName,Mode,Fi,State)
	  end,Cont,State).

create_async(#fuse_ctx{ uid = Uid, gid = Gid} = Ctx, ParentI, 
             BinName, Mode, Fi, State) -> 
    ?DBG2("create called"),
    NewInode = ffs_filesystem:create(
		 State#state.filesystem,ParentI,binary_to_list(BinName),
		 Uid,Gid,to_ffs_mode(Mode)),
    Param = inode_to_param(NewInode,State),
    #fuse_reply_create{fuse_entry_param = Param, fuse_file_info = Fi}.

%% @spec flush(Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { flush_async_reply (), NewState } | { noreply, NewState } 
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
flush(Ctx, InodeI, Fi, Cont, State) ->
    async(fun()-> flush_async(Ctx,InodeI,Fi,State) end,Cont,State).

flush_async(Ctx, InodeI, _Fi, State) -> 
    ?DBG2("flush called"),
    ffs_filesystem:flush(State#state.filesystem,InodeI),
    #fuse_reply_err{err = ok}.

%% @spec forget(Ctx::#fuseCtx{}, Inode::integer (), Nlookup::integer (), Cont::continuation (), State) -> { forget_async_reply (), NewState } | { noreply, NewState } 
%%  forget_async_reply () = #fuse_reply_none{}
%% @doc Forget about an inode.  If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type forget_async_reply ().
%% @end

forget(Ctx, InodeI, Nlookup, Cont, State) ->
	async(fun()-> forget_async(Ctx,InodeI,Nlookup,State) end, Cont, State).

forget_async(Ctx, _Inode, _Nlookup, _State) -> 
  ?DBG2("forget called"),
  #fuse_reply_none{}.

%% @spec fsync(Ctx::#fuseCtx{}, Inode::integer (), IsDataSync::bool (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { fsync_async_reply (), NewState } | { noreply, NewState } 
%%  fsync_async_reply () = #fuse_reply_err{}
%% @doc Ensure all changes are on permanent storage.  If the IsDataSync is
%% true, only the user data should be flushed, not the meta data.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type fsync_async_reply ().
%% @end

fsync(Ctx, Inode, IsDataSync, Fi, Cont, State) -> 
    async(fun() -> fsync_async(Ctx,Inode,IsDataSync,Fi,State) end,Cont,State).

fsync_async(Ctx, _Inode, _IsDataSync, _Fi, _State) -> 
    ?DBG2("fsync called"),
    erlang:throw (not_implemented).

%% @spec fsyncdir(Ctx::#fuseCtx{}, Inode::integer (), IsDataSync::bool (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { fsyncdir_async_reply (), NewState } | { noreply, NewState } 
%%  fsyncdir_async_reply () = #fuse_reply_err{}
%% @doc Ensure all directory changes are on permanent storage.  
%% If the IsDataSync is
%% true, only the user data should be flushed, not the meta data.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type fsyncdir_async_reply ().
%% @end
fsyncdir(Ctx, Inode, IsDataSync, Fi, Cont, State) ->
    async(fun() -> fsyncdir_async(Ctx,Inode,IsDataSync,Fi,State) end,Cont,State).

fsyncdir_async(Ctx, _Inode, _IsDataSync, _Fi, _State) -> 
    ?DBG2("fsyncdir called"),
    erlang:throw (not_implemented).

%% @spec getattr(Ctx::#fuseCtx{}, Inode::integer (), Cont::continuation (), State) -> { getattr_async_reply (), NewState } | { noreply, NewState } 
%%  getattr_async_reply () = #fuse_reply_attr{} | #fuse_reply_err{}
%% @doc Get the file attributes associated with an inode.  If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type getattr_async_reply ().
%% @end

getattr(Ctx, InodeI, Cont, State) -> 
    async(fun() -> getattr_async(Ctx,InodeI,State) end,Cont,State).

getattr_async(Ctx, InodeI, State) -> 
    ?DBG2("getattr called"),
    case ffs_filesystem:lookup(State#state.filesystem,InodeI) of
	enoent ->
	    #fuse_reply_err{ err = enoent};
	Inode ->
	    #fuse_reply_attr{ attr= stat(Inode,State), 
			      attr_timeout_ms=1000}
    end.


%% @spec getlk(Ctx::#fuseCtx{}, Inode::integer (), Fi::#fuse_file_info{}, Lock::#flock{}, Cont::continuation (), State) -> { getlk_async_reply (), NewState } | { noreply, NewState } 
%%  getlk_async_reply () = #fuse_reply_lock{} | #fuse_reply_err{}
%% @doc Test for POSIX file lock.  If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type getlk_async_reply ().
%% @end
getlk(Ctx, Inode, Fi, Lock, Cont, State) -> 
    async(fun() -> getlk_async(Ctx, Inode, Fi, Lock,State) end, Cont, State).


getlk_async(Ctx, _Inode, _Fi, _Lock, _State) -> 
    ?DBG2("getlk called"),
    erlang:throw (not_implemented).

%% @spec getxattr(Ctx::#fuseCtx{}, Inode::integer (), Name::binary (), Size::integer (), Cont::continuation (), State) -> { getxattr_async_reply (), NewState } | { noreply, NewState } 
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
getxattr(Ctx, Inode, Name, Size, Cont, State) ->
    async(fun() ->
		  getxattr_async(Ctx, Inode, Name, Size, State)
	  end, Cont, State).

getxattr_async(Ctx, _Inode, _Name, _Size, _State) ->
    ?DBG2("getxattr called"),
    #fuse_reply_xattr{ count = 0 }.

%% @spec link(Ctx::#fuseCtx{}, Ino::integer (), NewParent::integer (), NewName::binary (), Cont::continuation (), State) -> { link_async_reply (), NewState } | { noreply, NewState }
%%  link_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Create a hard link.  Ino is the existing inode.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type link_async_reply ().
%% @end
link(Ctx, Ino, NewParent, NewName, Cont, State) -> 
    async(fun() ->
		  link_async(Ctx, Ino, NewParent, NewName,State)
	  end, Cont, State).


link_async(Ctx, _Ino, _NewParent, _NewName, _State) -> 
    ?DBG2("link called"),
    erlang:throw (not_implemented).

%% @spec listxattr(Ctx::#fuseCtx{}, Ino::integer (), Size::integer (), Cont::continuation (), State) -> { listxattr_async_reply (), NewState } | { noreply, NewState }
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
listxattr(Ctx, Ino, Size, Cont, State) -> 
    async(fun() -> listxattr_async(Ctx, Ino, Size,State) end, Cont, State).

listxattr_async(Ctx, _Ino, _Size, _State) -> 
    ?DBG2("listxattr called"),
    #fuse_reply_xattr{ count = 0 }.

%% @spec lookup(Ctx::#fuseCtx{}, ParentInode::integer (), Name::binary (), Cont::continuation (), State) -> { lookup_async_reply (), NewState } | { noreply, NewState }
%%  lookup_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Lookup a directory entry by name and get its attributes.  Returning
%% an entry with inode zero means a negative entry which is cacheable, whereas
%% an error of enoent is a negative entry which is not cacheable.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type lookup_async_reply ().
%% @end
lookup(Ctx, ParentInodeI, BinName, Cont, State) ->
    async(fun() ->
		  lookup_async(Ctx, ParentInodeI, BinName, State)
	  end, Cont, State).

lookup_async(Ctx, ParentInodeI, BinName, State) ->
    ?DBG2("lookup called"),
    Name = erlang:binary_to_list (BinName),
    case ffs_filesystem:find(State#state.filesystem,ParentInodeI,Name) of
	enoent ->
	    #fuse_reply_err{ err = enoent };
	Inode ->
	    #fuse_reply_entry
	  { fuse_entry_param =
	    inode_to_param(Inode,State) }
    end.
    
%% @spec mkdir(Ctx::#fuseCtx{}, ParentInode::integer (), Name::binary (), Mode::stat_mode (), Cont::continuation (), State) -> { mkdir_async_reply (), NewState } | { noreply, NewState }
%%  mkdir_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Make a directory.  Mode is a mask composed of the ?S_XXXXX macros
%% which are (portably) defined in fuserl.hrl.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type mkdir_async_reply ().
%% @end
mkdir(Ctx, ParentInodeI, Name, Mode, Cont, State) ->
    async(fun() ->
		  mkdir_async(Ctx, ParentInodeI, Name, Mode, State)
	  end, Cont, State).

mkdir_async(Ctx, ParentInodeI, Name, Mode, State) ->
    ?DBG2("mkdir called"),
    
    Inode = ffs_filesystem:make_dir(State#state.filesystem,
				    ParentInodeI,
				    binary_to_list(Name),to_ffs_mode(Mode)),
    
    #fuse_reply_entry{ fuse_entry_param =
		       inode_to_param(Inode,State) }.

%% @spec mknod(Ctx::#fuseCtx{}, ParentInode::integer (), Name::binary (), Mode::stat_mode (), Dev::device (), Cont::continuation (), State) -> { mknod_async_reply (), NewState } | { noreply, NewState }
%%  device () = { Major::integer (), Minor::integer () }
%%  mknod_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Create a file node.  Mode is a mask composed of the ?S_XXXXX macros
%% which are (portably) defined in fuserl.hrl.  Dev is only valid if the 
%% created file is a device.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type mknod_async_reply ().
%% @end
mknod(Ctx, ParentInode, Name, Mode, Dev, Cont, State) -> 
    async(fun() ->
		  mknod_async(Ctx, ParentInode, Name, Mode, Dev,State)
	  end, Cont, State).

mknod_async(Ctx, _ParentInode, _Name, _Mode, _Dev, _State) -> 
    ?DBG2("mknod called"),
    erlang:throw (not_implemented).

%% @spec open(Ctx::#fuseCtx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { open_async_reply (), NewState } | { noreply, NewState }
%%   open_async_reply () = #fuse_reply_open{} | #fuse_reply_err{}
%% @doc Open an inode. If noreply is used, eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} 
%% should be called with Cont as first argument and the second argument of type
%% open_async_reply ().
%% @end
open(Ctx, _Inode, Fi, Cont, State) ->
    async(fun() -> open_async(Ctx, _Inode, Fi,State) end, Cont, State).

open_async(Ctx, _Inode, _Fi, _State) ->
    ?DBG2("open called"),
    #fuse_reply_open{ fuse_file_info = #fuse_file_info{flags = 66,
						       writepage = false,
						       direct_io = false,
						       keep_cache = false,
						       flush = false,
						       fh = 0,
						       lock_owner = 0} }.

%% @spec opendir(Ctx::#fuseCtx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { opendir_async_reply (), NewState } | { noreply, NewState }
%%   opendir_async_reply () = #fuse_reply_open{} | #fuse_reply_err{}
%% @doc Open an directory inode. If noreply is used, eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} 
%% should be called with Cont as first argument and the second argument of type
%% opendir_async_reply ().
%% @end
opendir(Ctx, Inode, Fi, Cont, State) ->
    async(fun() -> opendir_async(Ctx, Inode, Fi,State) end, Cont, State).

opendir_async(Ctx, _Inode, Fi, _State) ->
    ?DBG2("opendir called"),
    #fuse_reply_open{ fuse_file_info = Fi}.

%% @spec read(Ctx::#fuseCtx{}, Inode::integer (), Size::integer (), Offset::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { read_async_reply (), NewState } | { noreply, NewState }
%%   read_async_reply () = #fuse_reply_buf{} | #fuse_reply_err{}
%% @doc Read Size bytes starting at offset Offset. The file descriptor and
%% other flags are available in Fi. If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type read_async_reply ().
%% @end
read(Ctx, InodeI, Size, Offset, Fi, Cont, State) ->
    async(fun() ->
		  read_async(Ctx, InodeI, Size, Offset, Fi,State)
	  end, Cont, State).

read_async(Ctx, InodeI, Size, Offset, _Fi, State) ->
    ?DBG2("read called"),
    {ok,Data} = ffs_filesystem:read(State#state.filesystem,
				    InodeI, Size, Offset),
    #fuse_reply_buf{ buf = Data,
		     size = size(Data) }.

%% @spec readdir(Ctx::#fuseCtx{}, Inode::integer (), Size::integer (), Offset::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { readdir_async_reply (), NewState } | { noreply, NewState }
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
readdir(Ctx, InodeI, Size, Offset, Fi, Cont, State) ->
    async(fun() ->
		  readdir_async(Ctx, InodeI, Size, Offset, Fi, State)
	  end, Cont, State).

readdir_async(Ctx, InodeI, _Size, Offset, _Fi, State) ->
    ?DBG2("readdir called"),
    FfsList = ffs_filesystem:list(State#state.filesystem,InodeI),
    {List,_} = lists:mapfoldl(
		 fun({Name,Inode},Offset) -> 
			 {#direntry{ name = Name,
				     offset = Offset,
				     stat = stat(Inode,State)},
			  Offset+1}
		 end,Offset+1,lists:nthtail(Offset,FfsList)),
    #fuse_reply_direntrylist{ direntrylist = List }.


%% @spec readlink(Ctx::#fuseCtx{}, Inode::integer (), Cont::continuation (), State) -> { readlink_async_reply (), NewState } | { noreply, NewState }
%%   readlink_async_reply () = #fuse_reply_readlink{} | #fuse_reply_err{}
%% @doc Read the contents of a symbolic link.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type readlink_async_reply ().
%% @end
readlink(Ctx, Inode, Cont, State) -> 
    async(fun() -> readlink_async(Ctx, Inode, State) end, Cont, State).

readlink_async(Ctx, _Inode, _State) -> 
    ?DBG2("readlink called"),
    erlang:throw (not_implemented).

%% @spec release(Ctx::#fuseCtx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { release_async_reply (), NewState } | { noreply, NewState } 
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
release(Ctx, Inode, Fi, Cont, State) -> 
    async(fun() -> release_async(Ctx, Inode, Fi, State) end, Cont, State).

release_async(Ctx, _Inode, _Fi, _State) -> 
    ?DBG2("release called"),
    #fuse_reply_err{err = ok}.

%% @spec releasedir(Ctx::#fuseCtx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { releasedir_async_reply (), NewState } | { noreply, NewState } 
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
releasedir(Ctx, Inode, Fi, Cont, State) -> 
    async(fun() -> releasedir_async(Ctx, Inode, Fi, State) end, Cont, State).

releasedir_async(Ctx, _Inode, _Fi, _State) -> 
    ?DBG2("releasedir called"),
    #fuse_reply_err{err = ok}.

%% @spec removexattr(Ctx::#fuseCtx{}, Inode::integer (), Name::binary (), Cont::continuation (), State) -> { removexattr_async_reply (), NewState } | { noreply, NewState }
%%   removexattr_async_reply () = #fuse_reply_err{}
%% @doc Remove an extended attribute.  
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type removexattr_async_reply ().
%% @end
removexattr(Ctx, Inode, Name, Cont, State) -> 
    async(fun() -> removexattr_async(Ctx, Inode, Name, State) end, Cont, State).

removexattr_async(Ctx, _Inode, _Name, _State) -> 
    ?DBG2("removexattr called"),
    erlang:throw (not_implemented).

%% @spec rename(Ctx::#fuseCtx{}, Parent::integer (), Name::binary (), NewParent::integer (), NewName::binary (), Cont::continuation (), State) -> { rename_async_reply (), NewState } | { noreply, NewState }
%%   rename_async_reply () = #fuse_reply_err{}
%% @doc Rename a file.  #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type rename_async_reply ().
%% @end
rename(Ctx, Parent, Name, NewParent, NewName, Cont, State) ->
    async(fun() ->
		  rename_async(Ctx, Parent, Name, NewParent, NewName, State)
	  end, Cont, State).

rename_async(Ctx, _Parent, _Name, _NewParent, _NewName, _State) ->
    ?DBG2("rename called"),
    #fuse_reply_err{err = ok}.

%% @spec rmdir(Ctx::#fuseCtx{}, Inode::integer (), Name::binary (), Cont::continuation (), State) -> { rmdir_async_reply (), NewState } | { noreply, NewState }
%%   rmdir_async_reply () = #fuse_reply_err{}
%% @doc Remove a directory.  #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type rmdir_async_reply ().
%% @end
rmdir(Ctx, Inode, Name, Cont, State) -> 
    async(fun() -> rmdir_async(Ctx, Inode, Name, State) end, Cont, State).

rmdir_async(Ctx, _Inode, _Name, _State) -> 
    ?DBG2("rmdir called"),
    erlang:throw (not_implemented).

%% @spec setattr(Ctx::#fuseCtx{}, Inode::integer (), Attr::#stat{}, ToSet::integer (), Fi::maybe_fuse_file_info (), Cont::continuation (), State) -> { setattr_async_reply (), NewState } | { noreply, NewState }
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
setattr(Ctx, InodeI, Attr, ToSet, Fi, Cont, State) -> 
    async(fun() ->
		  setattr_async(Ctx, InodeI, Attr, ToSet, Fi, State)
	  end, Cont, State).

setattr_async(Ctx, InodeI, _Attr, _ToSet, _Fi, State) -> 
  ?DBG2("setattr called"),
    lists:foreach(fun %%(?FUSE_SET_ATTR_MTIME) when ?FUSE_SET_ATTR_MTIME bor ToSet =/= 0 ->
		  %%	ffs_filesystem:modify(State#state.filesystem,InodeI,unix_to_now(Attr#stat.st_mtime));
		  %%(?FUSE_SET_ATTR_ATIME) when ?FUSE_SET_ATTR_ATIME bor ToSet =/= 0 ->
		  %%	ffs_filesystem:access(State#state.filesystem,InodeI,unix_to_now(Attr#stat.st_atime));
		      (_) ->
			  ok
		  end,[	?FUSE_SET_ATTR_MODE,
			?FUSE_SET_ATTR_UID,
			?FUSE_SET_ATTR_GID,
			?FUSE_SET_ATTR_SIZE,
			?FUSE_SET_ATTR_ATIME,
			?FUSE_SET_ATTR_MTIME]),
    Inode = ffs_filesystem:lookup(State#state.filesystem,InodeI),
    #fuse_reply_attr{ attr = stat(Inode,State), attr_timeout_ms = 0 }.

%% @spec setlk(Ctx::#fuseCtx{}, Inode::integer (), Fi::#fuse_file_info{}, Lock::#flock{}, Sleep::bool(), Cont::continuation (), State) -> { setlk_async_reply (), NewState } | { noreply, NewState }
%%   setlk_async_reply () = #fuse_reply_err{}
%% @doc Set a POSIX file lock.  Sleep indicates whether the operation is 
%% blocking (true) or nonblocking (false).
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type setlk_async_reply ().
%% @end
setlk(Ctx, Inode, Fi, Lock, Sleep, Cont, State) -> 
    async(fun() ->
		  setlk_async(Ctx, Inode, Fi, Lock, Sleep, State)
	  end, Cont, State).

setlk_async(Ctx, _Inode, _Fi, _Lock, _Sleep, _State) -> 
    ?DBG2("setlk called"),
    erlang:throw (not_implemented).

%% @spec setxattr(Ctx::#fuseCtx{}, Inode::integer (), Name::binary (), Value::binary (), Flags::xattr_flags (), Cont::continuation (), State) -> { setxattr_async_reply (), NewState } | { noreply, NewState }
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
setxattr(Ctx, Inode, Name, Value, Flags, Cont, State) -> 
    async(fun() ->
		  setxattr_async(Ctx, Inode, Name, Value, Flags, State)
	  end, Cont, State).

setxattr_async(Ctx, _Inode, _Name, _Value, _Flags, _State) -> 
    ?DBG2("setxattr called"),
    #fuse_reply_err{err = ok}.

%% @spec statfs(Ctx::#fuseCtx{}, Inode::integer (), Cont::continuation (), State) -> { statfs_async_reply (), NewState } | { noreply, NewState }
%%   statfs_async_reply () = #fuse_reply_statfs{} | #fuse_reply_err{}
%% @doc Get file system statistics.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type statfs_async_reply ().
%% @end
statfs(Ctx, Inode, Cont, State) ->
    async(fun() -> statfs_async(Ctx, Inode, State) end, Cont, State).

statfs_async(Ctx, _Inode, #state{ filesystem = FS}) ->
    ?DBG2("statfs called"),
    
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
    #fuse_reply_statfs{ statvfs = S }.


%% @spec symlink(Ctx::#fuseCtx{}, Link::binary (), Inode::integer (), Name::binary (), Cont::continuation (), State) -> { symlink_async_reply (), NewState } | { noreply, NewState }
%%   symlink_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Create a symbolic link.  Link is the contents of the link.  Name is 
%% the name to create.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type symlink_async_reply ().
%% @end
symlink(Ctx, Link, Inode, Name, Cont, State) -> 
    async(fun() ->
		  symlink_async(Ctx, Link, Inode, Name, State)
	  end, Cont, State).

symlink_async(Ctx, _Link, _Inode, _Name, _State) -> 
    ?DBG2("symlink called"),
    erlang:throw (not_implemented).

%% @spec write(Ctx::#fuseCtx{}, Inode::integer (), Data::binary (), Offset::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { write_async_reply (), NewState } | { noreply, NewState }
%%   write_async_reply () = #fuse_reply_write{} | #fuse_reply_err{}
%% @doc Write data to a file.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type write_async_reply ().
%% @end
write(Ctx, InodeI, Data, Offset, Fi, Cont, State) -> 
    async(fun() ->
		  write_async(Ctx, InodeI, Data, Offset, Fi, State)
	  end, Cont, State).

write_async(Ctx, InodeI, Data, Offset, _Fi, State) -> 
    ?DBG2("write called"),	
  ffs_filesystem:write(State#state.filesystem,InodeI,Data,Offset),
  #fuse_reply_write{ count = size(Data) }.

%% @spec unlink(Ctx::#fuseCtx{}, Inode::integer (), Name::binary (), Cont::continuation (), State) -> { unlink_async_reply (), NewState } | { noreply, NewState }
%%   unlink_async_reply () = #fuse_reply_err{}
%% @doc Remove a file.  #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type unlink_async_reply ().
%% @end
unlink(Ctx, ParentI, Name, Cont, State) ->
    async(fun() -> unlink_async(Ctx, ParentI, Name, State) end, Cont, State).

unlink_async(Ctx, ParentI, Name, State) ->
    ?DBG2("unlink called"),
    ffs_filesystem:delete(State#state.filesystem,ParentI,binary_to_list(Name)),
    #fuse_reply_err{err = ok}.

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
init({_MountPoint,DefUid,DefGid,FfsOpts}) ->
    Filesystem = proplists:get_value("fs",FfsOpts),

    {ok, #state{ filesystem = Filesystem,
		 default_gid = DefGid,
		 default_uid = DefUid }}.

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
parse_options(["gid="++Filesystem|T],MountAcc,FfsAcc) ->
    parse_options(T,MountAcc,[{"gid",Filesystem}|FfsAcc]);
parse_options(["uid="++Filesystem|T],MountAcc,FfsAcc) ->
    parse_options(T,MountAcc,[{"uid",Filesystem}|FfsAcc]);
parse_options([Key|T],MountAcc,FfsAcc) ->
    parse_options(T,Key++MountAcc,FfsAcc);
parse_options([],MountAcc,FfsAcc) ->
    {MountAcc,FfsAcc}.

to_unix({Mega,Secs,_Micro}) ->
	Mega*1000000+Secs.
	
unix_to_now(Secs) ->
	{Secs div 1000000, Secs rem 1000000,0}.
	
async(Fun,Cont,State) ->
    spawn(fun() ->
		  Res = Fun(),
		  fuserlsrv:reply(Cont,Res)
	  end
	 ),
    {noreply,State}.
	
	
inode_to_param(#ffs_fs_inode{inode = InodeI} = Inode,State) ->
    #fuse_entry_param{ ino = InodeI,
		       generation = 0,%element(3,now()),
		       attr = stat(Inode,State),
		       attr_timeout_ms = 100,
		       entry_timeout_ms = 100}.

stat(#ffs_fs_inode{ inode = Inode, gid = Gid, uid = Uid,
		 atime = Atime, mtime = Mtime,
		 ctime = Ctime, refcount = Links,
		 size = Size, mode = Mode },State )  ->
    
    Config = ffs_filesystem:get_config(State#state.filesystem),
    BSize = ffs_lib:get_value(Config,block_size),
    
    NewUid = get_uid(Uid,State),
    NewGid = get_gid(Gid,State),
    
    #stat{
%      st_dev = { 0, 0 },                  % ID of device containing file 
      st_ino = Inode,                     % inode number 
      st_mode = to_fuse_mode(Mode),       % protection 
      st_nlink = Links,                   % number of hard links 
      st_uid = NewUid,                    % user ID of owner 
      st_gid = NewGid,                    % group ID of owner 
%      st_rdev = { 0, 0 },                % device ID (if special file) 
      st_size = Size,                     % total size = 0, in bytes 
      st_blksize = BSize,                 % blocksize for filesystem I/O 
      st_blocks = round(Size/BSize),      % number of blocks allocated 
      st_atime = to_unix(Atime),          % time of last access 
      st_mtime = to_unix(Mtime),          % time of last modification 
      st_ctime = to_unix(Ctime)           % time of last status change
     }.

get_uid(-1,#state{ default_uid = Uid }) ->
    Uid;
get_uid(Uid,_) ->
    Uid.

get_gid(-1,#state{ default_gid = Gid }) ->
    Gid;
get_gid(Gid,_) ->
    Gid.

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

	
