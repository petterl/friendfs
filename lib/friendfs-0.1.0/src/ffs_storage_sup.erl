%%%-------------------------------------------------------------------
%%% @author Petter Sandholdt <petterl@lysator.liu.se>
%%% @doc
%%%   Storage Supervisor
%%%
%%% Managaes supervision of storages and restarts dead storages when needed.
%%%
%%% @end
%%% Created : 10 Aug 2009 by Petter Sandholdt <petterl@lysator.liu.se>
%%%-------------------------------------------------------------------
-module(ffs_storage_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, connect_storage/2]).
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

connect_storage(Mod, Args) ->
    supervisor:start_child({local, ?SERVER}, [Mod, Args]).

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
    {ok, {{simple_one_for_one, 10, 10},
          [{ffs_storage, {ffs_storage_mgr, connect_storage, []},
            temporary, brutal_kill, worker, [ffs_storage_mgr]}]}}.

