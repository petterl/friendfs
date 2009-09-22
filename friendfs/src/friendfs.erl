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
    {ok,ConfigPath} = application:get_env(friendfs,config_path),
    {ok,DefaultsPath} = application:get_env(friendfs,config_default_path),
    
    Config = case ffs_lib:read_config(ConfigPath,DefaultsPath) of
		 {ok,Config0} ->
		     io:format("Loading configuration ~p\n",[Config0]),
		     Config0;
		 _Else ->
		     io:format("Could not read config file!"),
		     exit(1)
	     end,
    
    friendfs_sup:start_link(Config).

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

%%--------------------------------------------------------------------
%% @doc
%%   Mount a filesystem
%%
%% @spec
%%   add_config(MountPoint, Opts) -> ok
%%     MountPoint = string()
%%     Opts = list()
%% @end
%%--------------------------------------------------------------------
%add_config(MountPoint,Opts) ->
%    { ok, LinkedIn } = application:get_env(friendfs, linked_in),
%    { ok, MountOpts } = application:get_env (friendfs, mount_opts),
%    %% Beacuse friendfs is started with start_link it will crash when
%    %% the mounting process finsishes at the moment. But it is all work in
%    %% progress!
%    friendfs_sup:mount(LinkedIn, MountPoint, MountOpts),
%    io:format("Adding a new mount point!\n  Config = ~p with config ~p\n",[MountPoint,Opts]).
    
