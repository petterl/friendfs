%% Storage Manager
-module(ffs_storeage_mgr).
-behavour(gen_server).

%% Storage methods
-export([connect/2, start/0]).
-export([read/2, write/2, delete/2, list/1]).

-export([stores/0]).
-export([init/1, handle_call/3]).
-export([terminate/2, code_change/3, handle_info/2]).

start() ->
    Conf = [{mem, []},
	    {local, [{path, "/tmp/ffs"}]}],
    gen_server:start_link({local, ffsstore}, ?MODULE, Conf, []).

write(Cid, Data) ->
    gen_server:call(ffsstore, {write, Cid, all, Data}).

stores() ->
    gen_server:call(ffsstore, list_stores).

%%
%% Storage API

read(Pid, Cid) ->
    gen_server:call(Pid, {read, Cid}).

delete(Pid, Cid) ->
    gen_server:call(Pid, {delete, Cid}).

list(Pid) ->
    gen_server:call(Pid, list).

%% Gen server callbacks

-record(state,
	{storages   % List of configured stores
	}).

-record(storage,
	{priority = 100,
	 pid, % Pid to storage process
	 type
	}).

init(Config) ->
    S = connect_storages(Config),
    {ok, #state{storages = S}}.

handle_call(list_stores, _From, State) ->
    {reply, State, State};

handle_call({write, Cid, Ratio = all, Data}, _From, State) ->
    Res = lists:map(
	    fun(#storage{pid = Pid}) -> 
		    gen_server:call(Pid, {write, Cid, Data}) end,
	    State#state.storages),
    {reply, Res, State}.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State}.

%% Help functions

connect_storages([]) -> [];
connect_storages([{Type, Opts} | T]) -> 
    {ok, Pid} = connect(Type, Opts),
    [#storage{ pid = Pid, type = Type } | connect_storages(T)].

connect(Type, Args) ->
    case find_module(Type) of
	error ->  throw(no_module);
	Module -> 
	    gen_server:start_link(Module, Args, [])
    end.

find_module(local) -> ffsstore_local;
find_module(mem) -> ffsstore_mem;
find_module(_)     -> error.

