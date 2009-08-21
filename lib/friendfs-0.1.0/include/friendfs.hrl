-define(NOTEST, true).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(ffs_inode,{
	  inode,    %% int(): The inode number, unique per filesystem.
	  hash,     %% int(): A hash of the contents of the node
	  size,     %% int(): The size of the contents of this node
	  uid,      %% int(): The user which owns this node.
	  gid,      %% int(): The group which ownds this node.
	  mode,     %% int(): The access mode of this node, here information if
	            %% this is a directory or not can be found
	            %% drwxrwxrwx, 11 bits
	  ctime,    %% now(): When the node was created
	  atime,    %% now(): When the node was last accessed
	  mtime,    %% now(): When the node was last modified
	  refcount, %% int(): How many links this node has to it
	  ptr       %% str(): The name of this node on the remote side
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
	  path_mfa  %% The {M,F,A} to call with [Action,String|A] where Action is
	            %% encrypt or decrypt and String is a file or directory path.
	 }).


-define(U, ?U_R bor ?U_W bor ?U_X).
-define(G, ?G_R bor ?G_W bor ?G_X).
-define(O, ?O_R bor ?O_W bor ?O_X).
-define(A, ?U bor ?G bor ?O).

-define(D,  2#1000000000).

-define(U_R,2#0100000000).
-define(U_W,2#0010000000).
-define(U_X,2#0001000000).

-define(G_R,2#0000100000).
-define(G_W,2#0000010000).
-define(G_X,2#0000001000).

-define(O_R,2#0000000100).
-define(O_W,2#0000000010).
-define(O_X,2#0000000001).



