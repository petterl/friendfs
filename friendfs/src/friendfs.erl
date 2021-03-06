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

    {ok,ConfigPath} = application:get_env(friendfs,config_path),
    {ok,_DefaultsPath} = application:get_env(friendfs,config_default_path),
    case ffs_config:start(ConfigPath) of
        ok ->
            case ffs_config:get_secret() of
                not_found -> 
                    ok;
                Cookie ->
                    erlang:set_cookie(node(), list_to_atom(Cookie))
            end,
            start(Cmd,_Type,_Args);
        Err ->
            ?ERR("Could not read config file!: ~p", [Err]),
            {error, conf_error}
    end.


start(start,_Type,_Args) ->
    Res = friendfs_sup:start_link([]),
    init_filesystems(),
    application:set_env(friendfs, state, started),
    Res;
start(ctl,_Type,_Args) ->
    Cmd = init:get_plain_arguments(),
    friendfsctl:cmd(Cmd),
    {ok,self()};
start(Other,_Type,_Args) ->
    io:format("Bad start mode: ~p~n", [Other]),
    {ok,self()}.

init_filesystems() ->
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
    lists:foreach(fun({{"Filesystem",Name},_Args}) ->
			  ffs_filesystem:stop(Name)
		  end, ffs_config:get_filesystems()).

