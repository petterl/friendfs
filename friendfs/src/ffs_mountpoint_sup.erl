%%%-------------------------------------------------------------------
%%% @author Lukas Larsson <garazdawi@gmail.com>
%%% @doc
%%%   Filesystem Supervisor
%%%
%%% Managaes supervision of filesystems.
%%%
%%% @end
%%% Created : 19 Aug 2009 by Lukas Larsson <garazdawi@gmail.com>
%%%-------------------------------------------------------------------
-module(ffs_mountpoint_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, mount/2,umount/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

mount(MountPoint,Options) ->
    Spec = {MountPoint, {ffs_mountpoint, start_link, [MountPoint,Options]},
	    permanent, 10000, worker, [ffs_mountpoint]},
    supervisor:start_child(?SERVER, Spec).

umount(MountPoint) ->
    Res = (supervisor:terminate_child(?SERVER,MountPoint) == ok)
	andalso (supervisor:delete_child(?SERVER,MountPoint) == ok),

    if
	Res ->
	    ok;
	true ->
	    {error,umount_failed}
    end.
    
    

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the supervisor
%%
%%--------------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, 10, 10},
          []}}.

