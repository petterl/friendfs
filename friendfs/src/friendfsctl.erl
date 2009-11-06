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
%% Run command
%%
%% @spec
%%   cmd() -> ok
%% @end
%%--------------------------------------------------------------------

cmd(["ping"]) ->
    case net_adm:ping(?NODE) of
        pong ->
            halt(0);
        pang ->
            halt(255)
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

handle_cmd(["status"]) ->
    {{_storages, S0},{_chunks, C0}} = ffs_chunk_server:info(),
    io:format("Storages:\n", []),
    lists:foreach(
      fun(S) ->
              io:format("~s\n  P:~p R: ~s W: ~s\n",
                        [S#storage.url, S#storage.priority,
                         speed(S#storage.read_speed), speed(S#storage.write_speed)])
      end,S0),
    io:format("\nChunks with bad ratio:\n", []),
    No = lists:foldl(
           fun(C, Acc) when C#chunk.ratio > length(C#chunk.storages) ->
                   io:format("~s, ~p/~p\n",
                             [C#chunk.id, length(C#chunk.storages), C#chunk.ratio]),
                   Acc;
              (_C, Acc) ->
                   Acc+1
           end,0,C0),
    io:format("Chunks with ok ratio: ~B~n", [No]),
    0;
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
    
