%%%-------------------------------------------------------------------
%%% @author Petter Larsson <petterl@lysator.liu.se>
%%% @doc
%%% Config module
%%% @end
%%%-------------------------------------------------------------------
-module(ffs_config).

-export([start/1, parse_config/1, read/1, write/2]).
-export([get_filesystems/0, get_storages/0, get_secret/0]).

start(ConfigFile) ->
    ets:new(ffs_config, [public,named_table]),
    parse_config(ConfigFile).

parse_config(ConfigFile) ->
    case ffs_lib:parse_config(ConfigFile) of
	{ok, Config} ->
	    lists:foreach(fun({Key, Value}) ->
				  write(Key, Value);
			     ({Key1, Key2, Value}) ->
				  write({Key1, Key2}, Value)
			  end, Config),
	    ok;
	{error, Errors} -> 
	    %% io:format(" ** Failed to parse config: ~n~p~n~n", [Errors]),
	    %% [io:format("~p",[A]) || {_,_,A} <- Errors],
	    {error, Errors}
    end.

read(Key) ->
    case ets:lookup(ffs_config, Key) of
	[{Key, Value}] ->
	    Value;
	[] -> 
	    undefined
    end.

get_filesystems() ->
    ets:match_object(ffs_config, {{"Filesystem", '_'}, '_'}).

get_storages() ->
    ets:match_object(ffs_config, {{"Storage", '_'}, '_'}).

get_secret() ->
    case ets:match(ffs_config, {"Secret", '$1'}) of
	[[Secret]] -> Secret;
	[] -> not_found
    end.

write(Key, Value) ->
    ets:insert(ffs_config, {Key, Value}).

	
