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
-module(ffs_filesystem_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_filesystem/2]).
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

start_filesystem(Args) ->
	Spec = {ffs_filesystem, {ffs_filesysten, start_link, [Args]},
        	temporary, infinity, worker, [ffs_filesystem]},
    supervisor:start_child({local, ?SERVER}, Spec).

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

