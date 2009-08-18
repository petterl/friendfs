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
-export([start_link/0]).
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
    supervisor:start_link({local, ?SERVER}, []).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the supervisor
%%
%%--------------------------------------------------------------------
init(_Args) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{call, {call, start_link, []},
            temporary, brutal_kill, worker, [call]}]}}.

