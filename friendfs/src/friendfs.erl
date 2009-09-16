-module (friendfs).
-behaviour (application).
-export ([ start/0,
           start/2,
           stop/0,
           stop/1,
	   add_config/2]).

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%% @hidden

start() ->
    start(permanent).

start(Type) ->
    application:start(fuserl),
    application:start(friendfs, Type).

%% @hidden

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

%% @hidden

stop () ->
  application:stop(friendfs).

%% @hidden

stop(_State) ->
  ok.

add_config(MountPoint,Opts) ->
    { ok, LinkedIn } = application:get_env(friendfs, linked_in),
    { ok, MountOpts } = application:get_env (friendfs, mount_opts),
    %% Beacuse friendfs is started with start_link it will crash when
    %% the mounting process finsishes at the moment. But it is all work in
    %% progress!
    friendfs_sup:mount(LinkedIn, MountPoint, MountOpts),
    io:format("Adding a new mount point!\n  Config = ~p with config ~p\n",[MountPoint,Opts]).
    
