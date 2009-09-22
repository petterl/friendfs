%% Localhost storage
%%
%% Uses the local file system to stora a file
-module(ffs_storage_mem).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([terminate/2, code_change/3, handle_info/2]).

%% Gen server callback
init(_Args) ->
    {ok, []}.

handle_call({read, Cid}, _From, State) ->
    {value, Data} = lists:keysearch(Cid, 1, State),
    {reply, Data, State};

handle_call({write, Cid, Data}, _From, State) ->
    State1 = lists:keystore(Cid, 1, State, {Cid, Data}),
    {reply, ok, State1};

handle_call(list, _From, State) ->
    Res = lists:map(fun({K,_})->K end, State),
    {reply, Res, State};

handle_call({delete, Cid}, _From, State) ->
    State1 = lists:keydelete(Cid, 1, State),
    {reply, ok, State1}.

handle_cast(stop, _State) ->
    {noreply, ok}.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State}.

