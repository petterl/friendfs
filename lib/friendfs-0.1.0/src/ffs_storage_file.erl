%%%-------------------------------------------------------------------
%%% @author Petter Larsson <petterl@lysator.liu.se>
%%% @doc
%%% Storage module handling storage of chunks on local file system
%%% @end
%%% Created : 10 Aug 2009 by Petter Larsson <petter.larsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(ffs_storage_file).

-behaviour(gen_server).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state,
	{path   % Path of files
	}).

start_link(FSName,Url) ->
	gen_server:start_link(?MODULE,[FSName,Url],[]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Url) -> {ok, State} |
%%                    {ok, State, Timeout} |
%%                    ignore |
%%                    {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([FSName,Url]) ->
    {_Scheme, _Host, Path, _Query, _Fragment} =
        friendfs_lib:split_url(Url),
    case file:list_dir(Path) of
        {ok, FileList} -> 
			gen_server:cast(FSName,{connect,self(),FileList}),
			{ok, #state{path=Path}};
        _ -> {stop, path_missing_in_config}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(list, _From, State) ->
    Res = file:list_dir(State#state.path),
    {reply, Res, State};
handle_call({delete, Cid}, _From, State) ->
    Res = file:delete(join(State#state.path, Cid)),
    {reply, Res, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({read, From, Path, Offset}, State) ->
	Data = file:read_file(join(State#state.path, Path)),
	gen_server:reply(From,Data),
    {noreply, State};
handle_cast({write, From, Path, Data}, State) ->
	Result = file:write_file(join(State#state.path, Path),Data),
	gen_server:reply(From,Result),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

join(Path, Filename) ->
    filename:join(Path, Filename).
