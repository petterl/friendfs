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
%    { ok, LinkedIn } = application:get_env(friendfs, linked_in),
%    { ok, MountPoint } = application:get_env(friendfs, mount_point),
%   { ok, MountOpts } = application:get_env (friendfs, mount_opts),

    friendfs_sup:start_link(),
    {ok,self()}.

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
    
