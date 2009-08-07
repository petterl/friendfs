%% Localhost storage
%%
%% Uses the local file system to stora a file
-module(ffs_storage_local).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([terminate/2, code_change/3, handle_info/2]).

-record(state,
	{path   % Path of files
	}).

%% Gen server callback
init(Args) ->
    case lists:keysearch(path, 1, Args) of
	{value, {path, Path}} ->    {ok, #state{path=Path}};
	false -> {stop, path_missing_in_config}
    end.

handle_call({read, Cid}, _From, State) ->
    Data = file:read_file(join(State#state.path, Cid)),
    {reply, Data, State};
handle_call({write, Cid, Data}, _From, State) ->
    Res = file:write_file(join(State#state.path, Cid), Data),
    {reply, Res, State};
handle_call(list, _From, State) ->
    Res = file:list_dir(State#state.path),
    {reply, Res, State};
handle_call({delete, Cid}, _From, State) ->
    Res = file:delete(join(State#state.path, Cid)),
    {reply, Res, State}.

handle_cast(stop, _State) ->
    {noreply, ok}.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State}.


%%% Help Functions
join(Path, Filename) ->
    filename:join(Path, Filename).
