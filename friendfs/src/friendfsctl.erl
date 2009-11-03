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
-export ([ cmd/1,
           handle_cmd/1
          ]).

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

cmd(Command) ->
    Node = list_to_atom("friendfs@localhost"),
    case rpc:call(Node, ?MODULE, handle_cmd, [Command]) of
        {badrpc, nodedown} ->
            io:format("FriendFS not started!~n", []),
            halt();
        {badrpc, R} ->
            io:format("Failed to communicate with ~p: ~p~n~n", [Node, R]),
            usage(),
            halt();
        S ->
            halt(S)
    end.

handle_cmd(["status"]) ->
    {{_storages, S},{_chunks, C}} = ffs_chunk_server:info(),
    io:format("Storages:\n~p\nChunks:\n~p\n", [S, C]),
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
