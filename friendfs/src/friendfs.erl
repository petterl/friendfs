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
    { ok, LinkedIn } = application:get_env(friendfs, linked_in),
    { ok, MountPoint } = application:get_env(friendfs, mount_point),
    { ok, MountOpts } = application:get_env (friendfs, mount_opts),

    friendfs_sup:start_link(LinkedIn, MountPoint, MountOpts).

%% @hidden

stop () ->
  application:stop(friendfs).

%% @hidden

stop(_State) ->
  ok.

add_config(MountPoint,Opts) ->
    io:format("Adding a new mount point!\n  Config = ~p with config ~p\n",[MountPoint,Opts]).
    
