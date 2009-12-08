%%%-------------------------------------------------------------------
%%% @author Petter Sandholdt <petter@sandholdt.se>
%%% @author Lukas Larsson <garazdawi@gmail.com>
%%% @copyright 2009 Petter Sandholdt & Lukas Larsson
%%%
%%% @doc
%%% Main Application file
%%%
%%% Starts the application and the fuserl application
%%% Fetches the configuration and reads it and starts the supervisors.
%%%
%%% @end
%%%
%%% Created : 10 Aug 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------

-module (friendfsctl).
-include("debug.hrl").
-include("chunk_server.hrl").
-export ([ cmd/1,
           handle_cmd/1
          ]).

-define(NODE, 'friendfs@localhost').


%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%%--------------------------------------------------------------------
%% @doc
%% Run command. function will never return, it will always run
%% halt(Num) as last action. Whwere Num will be the returned error code.
%% 
%% 
%% @spec cmd(Cmd) -> void()
%%     Cmd = [string()]
%% @end
%%--------------------------------------------------------------------

cmd(["ping"]) ->
    case net_adm:ping(?NODE) of
        pong ->
            halt(0);
        pang ->
            halt(255)
    end;  

cmd(["wait", "start"] = C) ->
    case rpc:call(?NODE, ?MODULE, handle_cmd, [C]) of
        {badrpc, nodedown} ->
	    timer:sleep(50),
	    cmd(C);
        {badrpc, R} ->
            io:format("Failed to communicate with ~p: ~p~n~n", [?NODE, R]),
            usage(),
            halt(100);
        S ->
            halt(S)
    end;

cmd(["stop"]) ->
    case rpc:call(?NODE, ?MODULE, handle_cmd, [["stop"]]) of
        {badrpc, nodedown} ->
            halt(0);
        {badrpc, R} ->
            io:format("Failed to communicate with ~p: ~p~n~n", [?NODE, R]),
            usage(),
            halt(100);
        S ->
            halt(S)
    end;

cmd(Command) ->
    case rpc:call(?NODE, ?MODULE, handle_cmd, [Command]) of
        {badrpc, nodedown} ->
            io:format("FriendFS not started!~n", []),
            halt(255);
        {badrpc, R} ->
            io:format("Failed to communicate with ~p: ~p~n~n", [?NODE, R]),
            usage(),
            halt(100);
        S ->
            halt(S)
    end.

handle_cmd(["wait", "start"]) ->
    wait_started(),
    0;
handle_cmd(["status"]) ->
    MountPoints = ffs_mountpoint_sup:list_mountpoints(),
    io:format("MountPoints:\n", []),
    lists:foreach(
      fun({MountPoint, Filesystem}) ->
              io:format("  ~s -> ~s\n", [MountPoint, Filesystem])
      end,MountPoints),
    Filesystems = ffs_config:get_filesystems(),
    io:format("\nFilesystems:\n", []),
    lists:foreach(
      fun({{"Filesystem", Fs},Config}) ->
              {_, Comment} = lists:keyfind("Comment", 1, Config),
              io:format("~s\n ~s\n\n", [Fs, Comment])
      end,Filesystems),
    {{_storages, S0},{_chunks, C0}} = ffs_chunk_server:info(),
    io:format("Storages:\n", []),
    lists:foreach(
      fun(S) ->
              io:format("~s\n  P:~p R: ~s W: ~s\n",
                        [S#storage.url, S#storage.priority,
                         speed(S#storage.read_speed), speed(S#storage.write_speed)])
      end,S0),
    {Bad, Ok, Undef} = lists:foldl(
                  fun(C, {B, O, U}) when C#chunk.ratio =:= undefined ->
                          {B, O, U+1};
                     (C, {B, O, U}) when C#chunk.ratio > length(C#chunk.storages) ->
                          {B+1, O, U};
                     (_C, {B, O, U}) ->
                          {B, O+1, U}
           end,{0,0, 0},C0),
    io:format("\nChunks ratio\n  Bad: ~B\n  Undefined: ~B\n  Ok: ~B\n", [Bad, Undef, Ok]),
    0;
handle_cmd(["status", "chunks", "ok"]) ->
    {{_storages, _S0},{_chunks, C0}} = ffs_chunk_server:info(),
    io:format("Chunks with ok ratio:\n", []),
    Num = lists:foldl(
            fun(C, N) when C#chunk.ratio =:= undefined ->
                    N;
               (C, N) when C#chunk.ratio > length(C#chunk.storages) ->
                    N;
               (#chunk{storages=S, id=I}, N) ->
                    io:format("~p: ~p\n", [I, S]),
                    N+1
           end,0,C0),
    io:format("Chunks: ~p\n", [Num]),    
    0;
handle_cmd(["status"|Other]) ->
    io:format("Unknown status command: ~p~n~n", [Other]),
    usage(),
    100;
handle_cmd(["stop"]) ->
    %% Spawn off to make sure rpc returns
    spawn(fun() -> timer:sleep(10),init:stop() end),
    0;  
 
handle_cmd(["usage"]) ->  
    usage(),
    10;
handle_cmd(["load", M]) ->
    io:format("Load ~p into node: ", [M]),
    Mod = list_to_atom(M),  
    code:purge(Mod),
    case code:load_file(Mod) of
        {module, Mod} ->
            io:format("ok~n", []),
            0;
        {error, Err} ->
            io:format("error ~p~n", [Err]),
            100
    end;
handle_cmd(["mount", Path]) ->
    case ffs_config:get_filesystems() of
        [{{"Filesystem", Fs},_}] ->
            Options="fs="++Fs,
            handle_cmd(["mount", Path, Options]);
        _ ->
            io:format("More than one filesystem configured, "
                      "you need to define with fs with fs=<name>\n", []),
            100
    end;
handle_cmd(["mount", Path, Options]) ->
    {Filesystem,Uid,Gid,MountOpts} = parse_mount_options(Options),
    case ffs_mountpoint_sup:mount(Path,Filesystem,Uid,Gid,MountOpts) of
	{already_mounted, Fs} ->
	    io:format("Filesystem ~p already mounted at ~p", [Fs, Path]),
	    100;
	_ ->
	    io:format("Mounting friendfs in ~p with ~p\n", [Path, Options]),
	    0
    end;
handle_cmd(["umount", Path]) ->
    case ffs_mountpoint_sup:umount(Path) of
	{error, umount_failed} ->
	    io:format("Failed to unmount ~p", [Path]),
	    100;
	ok ->
	    0
    end;
handle_cmd(Other) ->
    io:format("Unknown command: ~p~n~n", [Other]),
    usage(),
    100.

usage() ->
    io:format( "The valid commands are: \n"
	       "   start - start the friendfs daemon\n"
	       "   stop - stop the friendfs daemon\n"
	       "   status - get status of running daemon\n"
	       "   restart - restart the friendfs daemon\n"
	       "   connect - connect to the friendfs daemon\n"
	       "\n"
	       "To mount a filesystem see 'friendfs'\n", []).

speed(undefined) ->
    "unknown";
speed(Bps) when Bps > 1000000.0 ->
    io_lib:format("~.2f Bb/s", [Bps / 1000000]);
speed(Bps) when Bps > 1024.0 ->
    io_lib:format("~.2f kb/s", [Bps / 1024]);
speed(Bps) ->
    io_lib:format("~.2f b/s", [Bps]).
    
wait_started() ->
    case application:get_env(friendfs, state) of
	{ok, started} -> ok;
	_ ->
	    timer:sleep(500),
	    wait_started()
    end.

parse_mount_options(String) ->
    Tokens = string:tokens(String,","),
    parse_mount_options(Tokens,{undefined,undefined,undefined,[]}).
parse_mount_options(["fs="++Fs|T],{_Fs,Uid,Gid,O}) ->
    parse_mount_options(T,{Fs,Uid,Gid,O});
parse_mount_options(["gid="++Gid|T],{Fs,Uid,_Gid,O}) ->
    parse_mount_options(T,{Fs,Uid,list_to_integer(Gid),O});
parse_mount_options(["uid="++Uid|T],{Fs,_Uid,Gid,O}) ->
    parse_mount_options(T,{Fs,list_to_integer(Uid),Gid,O});
parse_mount_options([Key|T],{Fs,Uid,Gid,O}) ->
    parse_mount_options(T,{Fs,Uid,Gid,O++Key});
parse_mount_options([],{Fs,Uid,Gid,O}) ->
    {Fs,Uid,Gid,O}.
