%%%-------------------------------------------------------------------
%%% @author Petter Larsson <petterl@lysator.liu.se>
%%% @doc
%%% Storage module handling storage of chunks on local file system
%%% @end
%%% Created : 10 Aug 2009 by Petter Larsson <petter.larsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(ffs_storage_file).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(REFRESH_INTERVAL, 5000).

-record(state,
	{url,         % URL in config file
     path,        % Path of files
     chunks = []  % Chunks in storage
	}).

start_link(Url, Config) ->
	gen_server:start_link(?MODULE,[Url, Config],[]).

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
init([Url, _Config]) ->
    {_Scheme, Host, UrlPath, _Query, _Fragment} =
        ffs_lib:split_url(Url),
	Path = case Host of
		"" ->
			UrlPath;
		_Else ->
			%% non absolute file path
			Host++UrlPath
	end,
    case file:list_dir(Path) of
        {ok, _List} ->
    	    {ok, #state{url=Url, path=Path}, ?REFRESH_INTERVAL};
        {error, _} ->
	        {stop, bad_path_for_storage, Path}
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
    {reply, Res, State, ?REFRESH_INTERVAL};

handle_call({write, Path, Data}, _From, State) ->
    Result = file:write_file(join(State#state.path, Path),Data),
    {reply, Result, State, ?REFRESH_INTERVAL};

handle_call(refresh, _From, State) ->
    {ok, Files} = file:list_dir(State#state.path),
    ffs_chunk_server:update_storage(State#state.url, self(), Files, all),
    {reply, Files, State, ?REFRESH_INTERVAL};

handle_call(info, _From, State) ->
    {reply, State, State, ?REFRESH_INTERVAL}.


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
handle_cast({delete, Cid}, State) ->
    % Delete file from dir
    file:delete(join(State#state.path, Cid)),
    {noreply, State, ?REFRESH_INTERVAL};

handle_cast({read, Path, From}, State) ->
    % Read data from file
    Res = file:read_file(join(State#state.path, Path)),
    % Send it to requesting process
    gen_server:reply(From, Res),
    {noreply, State, ?REFRESH_INTERVAL};

handle_cast({read_async, Path, {M,F,A}}, State) ->
    % Read data from file
    Res = file:read_file(join(State#state.path, Path)),
    % Send it to requesting process
    R = M:F(Res, A),
    io:format("Calling ~p:~p(~p, ~p) -> ~p~n", [M, F, Res, A, R]),

    {noreply, State, ?REFRESH_INTERVAL}.

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
handle_info(timeout, State) ->
    State2 = refresh_storage(State),
    {noreply, State2, ?REFRESH_INTERVAL};

handle_info(_Info, State) ->
    {noreply, State, ?REFRESH_INTERVAL}.


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

refresh_storage(State) ->
    {ok, Files} = file:list_dir(State#state.path),
    Added = lists:subtract(Files, State#state.chunks),
    Removed = lists:subtract(State#state.chunks, Files),
    case {Added, Removed} of
        {[],[]} -> ok;
        _ ->
            ffs_chunk_server:update_storage(State#state.url, self(), Added, Removed)
    end,
    State#state{chunks = Files}.
