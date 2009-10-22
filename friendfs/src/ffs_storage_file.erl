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
-include("friendfs.hrl").

-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(REFRESH_INTERVAL, 10000).

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
        {ok, Files} ->
            % TODO: Verify storage here
            ffs_chunk_server:update_storage(Url, self(), Files),
    	    {ok, #state{url=Url, path=Path, chunks = Files}, ?REFRESH_INTERVAL};
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
handle_cast({delete, Cid}, State0) ->
    % Delete file from dir
    State = 
        case file:delete(join(State0#state.path, Cid)) of
            ok ->
                State0#state{chunks = State0#state.chunks -- Cid};
            {error, _} ->
                State0
        end,
    {noreply, State, ?REFRESH_INTERVAL};

handle_cast({read, Path, From, Ref}, State) ->
    % Read data from file
    case file:read_file(join(State#state.path, Path)) of
	{ok, Data} ->
	    %% Tell chunkserver that we got the data
	    gen_server:cast(ffs_chunk_server, {read_callback, Ref, ok}),
	    %% Send it to requesting process
	    gen_server:reply(From, {ok, Data});
	{error, _} = Err ->
	    %% Tell chunkserver that we got error
	    gen_server:cast(ffs_chunk_server, {read_callback, Ref, Err})
    end,
    {noreply, State, ?REFRESH_INTERVAL};

handle_cast({write, Cid, Data, From, Ref}, State) ->
    ?DBG("write start", []),
    case file:write_file(join(State#state.path, Cid), Data) of
        ok ->	
            ?DBG("write ok", []),
            State1 = State#state{chunks = State#state.chunks ++ [Cid]},
            % Tell chunkserver that we stored the data
            gen_server:cast(ffs_chunk_server, {write_callback, Ref, ok}),
            % Send it to requesting process
            gen_server:reply(From, {ok, Cid});
        {error, _} = Err ->
            ?DBG("write fail: ~p~n", [Err]),
            State1 = State,
            % Tell chunkserver that we stored the data
            gen_server:cast(ffs_chunk_server, {write_callback, Ref, Err})
    end,
    {noreply, State1, ?REFRESH_INTERVAL}.

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
    ffs_chunk_server:update_storage(State#state.url, self(), Files),
    State#state{chunks = Files}.
