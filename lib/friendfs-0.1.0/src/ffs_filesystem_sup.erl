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
-export([start_link/1, start_filesystem/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link(Args) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Args]).

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
init([Args]) ->
    ffs_fat:init_counters(),
    Specs = get_filesystems(Args,[]),
    {ok, {{one_for_one, 10, 10},
          Specs}}.

get_filesystems([{"Filesystem",Name,Args}|T],Acc) ->
	get_filesystems(T,[{Name, {ffs_filesystem, start_link, [list_to_atom(Name),Args]},
        	permanent, 10000, worker, [ffs_filesystem]}|Acc]);
get_filesystems([_|T],Acc) ->
	get_filesystems(T,Acc);
get_filesystems([],Acc) ->
	Acc.
