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

-module (friendfs).
-behaviour (application).
-include("debug.hrl").
-export ([ start/0,
           start/2,
           stop/0,
           stop/1
           %add_config/2
          ]).

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%%--------------------------------------------------------------------
%% @doc
%% Start friendfs application
%%
%% @spec
%%   start() -> ok
%% @end
%%--------------------------------------------------------------------

start() ->
    start(permanent).

%hidden
start(Type) ->
    application:start(fuserl),
    application:start(friendfs, Type).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetch config and start supervisors
%%
%% @spec
%%   start(_Type, _Args) -> ok
%% @end
%%--------------------------------------------------------------------
start(_Type, _Args) ->
    {ok,Cmd} = application:get_env(friendfs,cmd),
    start(Cmd,_Type,_Args).

start(start,_Type,_Args) ->
    io:format("Type = ~p, Args = ~p\n",[_Type,_Args]),
    {ok,ConfigPath} = application:get_env(friendfs,config_path),
    {ok,_DefaultsPath} = application:get_env(friendfs,config_default_path),
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

    
    init_filesystems(),
    friendfs_sup:start_link([]);
start(Cmd,_Type,_Args) ->
    friendfsctl:cmd(Cmd),
    {ok,self()}.

init_filesystems() ->
    ffs_fat:init_counters(),
    lists:foreach(fun({{"Filesystem",Name},_Args}) ->
			  ffs_filesystem:init(Name)
		  end, ffs_config:get_filesystems()).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%   Stop friendfs and fuserl application
%%
%% @spec
%%   stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop () ->
  application:stop(fuserl),
  application:stop(friendfs).

%% @hidden

stop(_State) ->
  ok.

