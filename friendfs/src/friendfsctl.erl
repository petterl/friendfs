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
    {ok,ConfigPath} = application:get_env(friendfs,config_path),
    case ffs_config:start(ConfigPath) of
        ok ->
            ok;
        Err ->
            ?ERR("Could not read config file!: ~p", [Err]),
            exit(1)
    end,
    case ffs_config:get_secret() of
	not_found -> 
	    ok;
	Cookie ->
	    erlang:set_cookie(node(), list_to_atom(Cookie))
    end,
    Node = list_to_atom("friendfs@localhost"),
    case rpc:call(Node, ?MODULE, handle_cmd, [Command]) of
        {badrpc, nodedown} ->
            io:format("FriendFS not started!~n", []),
            halt();
        {badrpc, R} ->
            io:format("Failed to communicate with ~p: ~p~n~n", [Node, R]),
            handle_cmd("usage"),
            halt();
        S ->
            halt(S)
    end.

handle_cmd("status") ->
    S = lists:flatten(ffs_chunk_server:info()),
    io:format("~p~n", [S]),
    0;
handle_cmd("stop") ->
    %% Spawn off to make sure rpc returns
    spawn(fun() -> timer:sleep(10),init:stop() end),
    0;

handle_cmd("usage") ->
    io:format( "The valid commands are: \n"
                   "   start - start the friendfs daemon\n"
                   "   stop - stop the friendfs daemon\n"
                   "   status - get status of running daemon\n"
                   "   restart - restart the friendfs daemon\n"
                   "   connect - connect to the friendfs daemon\n"
                   "\n"
                   "To mount a filesystem see 'friendfs'\n", []),
    10;
handle_cmd(Other) ->
    io:format("Unknown command: ~p~n~n", [Other]),
    handle_cmd("usage"),
    100.
