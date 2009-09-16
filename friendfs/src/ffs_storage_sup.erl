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
-export([start_link/1, connect_storage/2]).
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
init([Args]) ->
	Specs = get_storages(Args,[]),
    {ok, {{one_for_one, 10, 10},Specs}}.


get_storages([{"Filesystem",Name,Args}|T],Acc) ->
	NewAcc = get_storages(list_to_atom(Name),Args,Acc),
	get_storages(T,NewAcc);
get_storages([_|T],Acc) ->
	get_storages(T,Acc);
get_storages([],Acc) ->
	Acc.
get_storages(FSName,[{"Storage",Url}|T],Acc) ->
	Module = get_storage_mod(Url),
	Config = [],
	get_storages(FSName,T,[{{Url, Config}, {Module,start_link,[Url, Config]},
        	permanent, 10000, worker, [Module]} | Acc]);
get_storages(FSName,[_|T],Acc) ->
	get_storages(FSName,T,Acc);
get_storages(_FSName,[],Acc) ->
	Acc.

get_storage_mod(Url) ->
	{Scheme, _Rest} = ffs_lib:urlsplit_scheme(Url),
	list_to_atom(lists:concat(["ffs_storage_",Scheme])).
