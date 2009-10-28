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

cmd(status) ->
    {ok,ConfigPath} = application:get_env(friendfs,config_path),
    case ffs_config:start(ConfigPath) of
        ok ->
            ?DBG("Loading configuration\n",[]);
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
    Node = list_to_atom("friendfs@"++hd(tl(string:tokens(atom_to_list(node()), "@")))),
    R = rpc:call(Node, friendfsctl, handle_cmd, [status]),
    io:format("RPC: ~p~n", [R]);

cmd(Other) ->
    io:format("Unknown command: ~p~n",[Other]).


handle_cmd(status) ->
    S = ffs_chunk_server:info(),
    io:format("~p~n", [S]),
    ok.
