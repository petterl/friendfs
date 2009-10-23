%%%-------------------------------------------------------------------
%%% @author Petter Larsson <petterl@lysator.liu.se>
%%% @doc
%%% Config module
%%% @end
%%%-------------------------------------------------------------------
-module(ffs_config).

-export([init/0, parse_config/1, read/1, write/2]).
-export([get_filesystems/0, get_storages/0]).

init() ->
    ets:new(ffs_config, [public,named_table]),
    ok.

parse_config(ConfigFile) ->
    case ffs_lib:parse_config(ConfigFile) of
	{ok, Config} ->
	    lists:foreach(fun({Key, Value}) ->
				  write(Key, Value);
			     ({Key1, Key2, Value}) ->
				  write({Key1, Key2}, Value)
			  end, Config);
	Error -> Error
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

write(Key, Value) ->
    ets:insert(ffs_config, {Key, Value}).

	
