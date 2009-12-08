%%%-------------------------------------------------------------------
%%% @author Petter Sandholdt <petter@sandholdt.se>
%%% @doc
%%% == Debug Module ==
%%%
%%% Debug helper module
%%%
%%% @end
%%% Created : 20 Oct 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_debug).

-include("friendfs.hrl").
-include_lib("fuserl/include/fuserl.hrl").

%% Log API
-export([log_debug/4, log_typed/5, log_error/4]).

%% Fuse Help API
-export([fuse_mode_info/1]).


log_error(Mod, Line, Fmt, Vars) ->
    io:format("~p:~p: **ERROR**:"++Fmt, [Mod, Line|Vars]),
    ok.

log_debug(Mod, Line, Fmt, Vars) ->
    io:format("~p:~p:"++Fmt, [Mod, Line|Vars]),
    ok.

log_typed(Type, Mod, Line, Fmt, Vars) ->
    io:format("~p:~p: #~p# "++Fmt, [Mod, Line, Type|Vars]),
    ok.

fuse_mode_info(Octet) ->
    io:format("bitmask for the file type bitfields: "),
    io:format("~p~n",[(?S_IFMT band Octet) bsr 12]),
    io:format("socket: "),
    io:format("~p~n",[(?S_IFSOCK band Octet) /= 0]), 
    io:format("symbolic link: "),
    io:format("~p~n",[(?S_IFLNK band Octet) /= 0]), 
    io:format("regular file: "),
    io:format("~p~n",[(?S_IFREG band Octet) /= 0]), 
    io:format("block device: "),
    io:format("~p~n",[(?S_IFBLK band Octet) /= 0]), 
    io:format("directory: "),
    io:format("~p~n",[(?S_IFDIR band Octet) /= 0]), 
    io:format("character device: "),
    io:format("~p~n",[(?S_IFCHR band Octet) /= 0]), 
    io:format("FIFO: "),
    io:format("~p~n",[(?S_IFIFO band Octet) /= 0]), 
    io:format("set UID bit: "),
    io:format("~p~n",[(?S_ISUID band Octet) /= 0]), 
    io:format("set-group-ID bit (see below): "),
    io:format("~p~n",[(?S_ISGID band Octet) /= 0]), 
    io:format("sticky bit (see below): "),
    io:format("~p~n",[(?S_ISVTX band Octet) /= 0]), 
    io:format("mask for file owner permissions: "),
    io:format("~p~n",[(?S_IRWXU band Octet) /= 0]), 
    io:format("owner has read permission: "),
    io:format("~p~n",[(?S_IRUSR band Octet) /= 0]), 
    io:format("owner has write permission: "),
    io:format("~p~n",[(?S_IWUSR band Octet) /= 0]), 
    io:format("owner has execute permission: "),
    io:format("~p~n",[(?S_IXUSR band Octet) /= 0]), 
    io:format("mask for group permissions: "),
    io:format("~p~n",[(?S_IRWXG band Octet) /= 0]), 
    io:format("group has read permission: "),
    io:format("~p~n",[(?S_IRGRP band Octet) /= 0]), 
    io:format("group has write permission: "),
    io:format("~p~n",[(?S_IWGRP band Octet) /= 0]), 
    io:format("group has execute permission: "),
    io:format("~p~n",[(?S_IXGRP band Octet) /= 0]), 
    io:format("mask for permissions for others (not in group): "),
    io:format("~p~n",[(?S_IRWXO band Octet) /= 0]), 
    io:format("others have read permission: "),
    io:format("~p~n",[(?S_IROTH band Octet) /= 0]), 
    io:format("others have write permission: "),
    io:format("~p~n",[(?S_IWOTH band Octet) /= 0]), 
    io:format("others have execute permission: "),
    io:format("~p~n",[(?S_IXOTH band Octet) /= 0]).
