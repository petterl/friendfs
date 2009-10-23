-define(NOTEST, true).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(ffs_inode,{
	  inode,              %% int(): The inode number, unique per filesystem.
	  hash,               %% int(): A hash of the contents of the node
	  size,               %% int(): The size of the contents of this node
	  uid,                %% int(): The user which owns this node.
	  gid,                %% int(): The group which ownds this node.
	  mode,               %% int(): The access mode of this node, here information if
	                      %% this is a directory or file can be found
	                      %% fdrwxrwxrwx, 12 bits
	  ctime,              %% now(): When the node was created
	  atime,              %% now(): When the node was last accessed
	  mtime,              %% now(): When the node was last modified
	  refcount,           %% int(): How many links this node has to it
	  chunks = [],        %% [#ffs_chunk()]: A list of all the chunks of this inode
	  write_cache         %% {chunkid,bin()}: The current write cache
	 }).

-record(ffs_link,{
	  from,     %% int(): The inode directory from which this link comes.
	  to,       %% int(): The inode to which this link points.
	  name,     %% str(): The name of this link
	  type      %% atom(): What type of link this is, i.e. soft or hard
	 }).

-record(ffs_xattr,{
	  inode,    %% int(): The inode for which these attributes apply
	  attr      %% list(): A {key,value} tuple list containing extra
	            %% information about each inode.
	 }).

-record(ffs_tid,{
      name,     %% The name of this fat, used for the inode counter.
	  inode,    %% Table ID of the inode ets table
	  link,     %% Table ID of the link ets table
	  xattr,    %% Table ID of the xattr ets table
	  config    %% Configuration for the fat
	 }).

-record(ffs_chunk,{
	  id,       %% The place and id of this chunk within a inode
	  size,     %% The size of this chunk
	  chunkid   %% The chunkid of this chunk used by the ffs_chunk_server
	 }).


-define(U, ?U_R bor ?U_W bor ?U_X).
-define(G, ?G_R bor ?G_W bor ?G_X).
-define(O, ?O_R bor ?O_W bor ?O_X).
-define(A, ?U bor ?G bor ?O).

-define(D,  2#1000000000).
-define(F,  2#10000000000).

-define(U_R,2#0100000000).
-define(U_W,2#0010000000).
-define(U_X,2#0001000000).

-define(G_R,2#0000100000).
-define(G_W,2#0000010000).
-define(G_X,2#0000001000).

-define(O_R,2#0000000100).
-define(O_W,2#0000000010).
-define(O_X,2#0000000001).


%% Config keys
-define(BLOCK_SIZE,"BlockSize").
-define(INODE_LIMIT,"MaxInodes").
-define(FILESYSTEM_ID,"FilesystemId").
-define(MNT_OPTS,"MountOptions").
-define(MAX_FILENAME_SIZE,"MaxFilenameSize").

%% Statistics keys
-define(TOT_MEM,tot_mem).
-define(FREE_MEM,free_mem).
-define(FREE_INODES,free_inodes).

