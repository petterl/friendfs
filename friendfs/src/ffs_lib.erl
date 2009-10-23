%%%-------------------------------------------------------------------
%%% @author Petter Larsson <petterl@lysator.liu.se>
%%% @doc
%%% Library of internal functions for FriendFS
%%% @end
%%%-------------------------------------------------------------------
-module(ffs_lib).


-export([split_url/1,urlsplit_scheme/1,read_config/2,parse_config/1,scan_config_str/2]).
-export([path_split/1,get_value/2,get_chunkid/1,get_hash/1,print_base64/1,pmap/2]).
-export([urlunsplit/1,
         urlunsplit_path/1]).
-define(DELIMS,[$ ,$\n,$#,$>,$<,$\t]).

split_url(Url) ->
    {Scheme, Url1} = urlsplit_scheme(Url),
    {Netloc, Url2} = urlsplit_netloc(Url1),
    {Path, Query, Fragment} = urlsplit_path(Url2),
    {Scheme, Netloc, Path, Query, Fragment}.

urlsplit_scheme(Url) ->
    urlsplit_scheme(Url, []).

urlsplit_scheme([], Acc) ->
    {"", lists:reverse(Acc)};
urlsplit_scheme(":" ++ Rest, Acc) ->
    {string:to_lower(lists:reverse(Acc)), Rest};
urlsplit_scheme([C | Rest], Acc) ->
    urlsplit_scheme(Rest, [C | Acc]).

urlsplit_netloc("//" ++ Rest) ->
    urlsplit_netloc(Rest, []);
urlsplit_netloc(Path) ->
    {"", Path}.

urlsplit_netloc(Rest=[C | _], Acc) when C =:= $/; C =:= $?; C =:= $# ->
    {lists:reverse(Acc), Rest};
urlsplit_netloc([],Acc) ->
    {lists:reverse(Acc), []};
urlsplit_netloc([C | Rest], Acc) ->
    urlsplit_netloc(Rest, [C | Acc]).


%% @spec path_split(string()) -> {Part, Rest}
%% @doc Split a path starting from the left, as in URL traversal.
%%      path_split("foo/bar") = {"foo", "bar"},
%%      path_split("/foo/bar") = {"", "foo/bar"}.
path_split(S) ->
    path_split(S, []).

path_split("", Acc) ->
    {lists:reverse(Acc), ""};
path_split("/" ++ Rest, Acc) ->
    {lists:reverse(Acc), Rest};
path_split([C | Rest], Acc) ->
    path_split(Rest, [C | Acc]).

%% @spec urlunsplit({Scheme, Netloc, Path, Query, Fragment}) -> string()
%% @doc Assemble a URL from the 5-tuple. Path must be absolute.
urlunsplit({Scheme, Netloc, Path, Query, Fragment}) ->
    lists:flatten([case Scheme of "" -> "";  _ -> [Scheme, "://"] end,
                   Netloc,
                   urlunsplit_path({Path, Query, Fragment})]).

%% @spec urlunsplit_path({Path, Query, Fragment}) -> string()
%% @doc Assemble a URL path from the 3-tuple.
urlunsplit_path({Path, Query, Fragment}) ->
    lists:flatten([Path,
                   case Query of "" -> ""; _ -> [$? | Query] end,
                   case Fragment of "" -> ""; _ -> [$# | Fragment] end]).

%% @spec urlsplit_path(Url) -> {Path, Query, Fragment}
%% @doc Return a 3-tuple, does not expand % escapes. Only supports HTTP style
%%      paths.
urlsplit_path(Path) ->
    urlsplit_path(Path, []).

urlsplit_path("", Acc) ->
    {lists:reverse(Acc), "", ""};
urlsplit_path("?" ++ Rest, Acc) ->
    {Query, Fragment} = urlsplit_query(Rest),
    {lists:reverse(Acc), Query, Fragment};
urlsplit_path("#" ++ Rest, Acc) ->
    {lists:reverse(Acc), "", Rest};
urlsplit_path([C | Rest], Acc) ->
    urlsplit_path(Rest, [C | Acc]).

urlsplit_query(Query) ->
    urlsplit_query(Query, []).

urlsplit_query("", Acc) ->
    {lists:reverse(Acc), ""};
urlsplit_query("#" ++ Rest, Acc) ->
    {lists:reverse(Acc), Rest};
urlsplit_query([C | Rest], Acc) ->
    urlsplit_query(Rest, [C | Acc]).

%% @spec read_config(Config,Defaults) -> [{key,value}|{key,value,list}]
%% @doc Parses a config file and uses Defaults to fill in the blanks.
%%
read_config(Config,Defaults) ->
    ErlDefaults = parse_defaults(Defaults),
    ErlConfig = parse_config(Config),
    case {ErlDefaults,ErlConfig} of
	{{ok,Def},{ok,Conf}} ->
	    {ok,validate_and_fill(Conf,Def)};
	{{ok,_},Error} ->
	    io:format("Could not parse ~p\nReason: ~p\n",[Config,Error]),
	    invalid_config;
	{Error,{ok,_}} ->
	    io:format("Could not parse ~p\nReason: ~p\n",[Defaults,Error]),
	    invalid_defaults
    end.

	    

parse_config(Path) ->
    case file:read_file(Path) of
	{ok,Bin} -> 
	    Scan = scan_config_str(binary_to_list(Bin),1),
	    ffs_config_parse:parse(Scan);
	Error -> {error,?MODULE,parse_config,Error}
    end.
	
parse_defaults(Path) ->
    case file:read_file(Path) of
	{ok,Bin} -> 
	    Scan = scan_config_str(binary_to_list(Bin),1),
	    ffs_config_parse:parse(Scan);
	Error -> {error,?MODULE,parse_config,Error}
    end.
	
validate_and_fill(Config,Def) ->
    {Global,Local} = get_global_config(Config,[],[]),
    LocalWGlobal = fill_local(Local,Global),
    add_defaults(Def,LocalWGlobal).

get_global_config([{_Var,_Key} = Conf|T],GlobalAcc,LocalAcc) ->
    get_global_config(T,[Conf|GlobalAcc],LocalAcc);
get_global_config([Else|T],GlobalAcc,LocalAcc) ->
    get_global_config(T,GlobalAcc,[Else|LocalAcc]);
get_global_config([],GlobalAcc,LocalAcc) ->
    {GlobalAcc,LocalAcc}.

fill_local([{Key,Val,Children}|T],Global) ->
    NewChildren = lists:foldl(
		    fun({GlobalKey,GlobalVal},ChildList) ->
			    case proplists:get_value(GlobalKey,ChildList) of
				undefined ->
				    [{GlobalKey,GlobalVal}|ChildList];
				_Else ->
				    ChildList
			    end
		    end,Children,Global),
    [{Key,Val,NewChildren}|fill_local(T,Global)];
fill_local([],_Global) ->
    [].

add_defaults(_,Config) ->
    Config.


scan_config_str([$#|T],Line) ->
	{Comment,Tail} = read_until_chars(T,[$\n]),
	[{comment,Line,Comment}|scan_config_str(Tail,Line)];
scan_config_str([$<,$/|T],Line) ->
	[{'</',Line}|scan_config_str(T,Line)];	
scan_config_str([$<|T],Line) ->
	[{'<',Line}|scan_config_str(T,Line)];	
scan_config_str([$>|T],Line) ->
	[{'>',Line}|scan_config_str(T,Line)];
scan_config_str([$\n|T],Line) ->
	scan_config_str(T,Line+1);
scan_config_str([Char|T],Line) when Char == $\t; Char == $ ->
	scan_config_str(T,Line);
scan_config_str([Char|T],Line) when Char > $A, Char < $Z ->
	{Key,Tail} = read_until_chars([Char|T],?DELIMS),
	[{key,Line,Key}|scan_config_str(Tail,Line)];
scan_config_str([$",Char|T],Line) ->
	{Value,[_|Tail]} = read_until_chars([Char|T],[$"]),
	[{value,Line,Value}|scan_config_str(Tail,Line)];	
scan_config_str([Char|T],Line) ->
	{Value,Tail} = read_until_chars([Char|T],?DELIMS),
	[{value,Line,make_int(Value)}|scan_config_str(Tail,Line)];
scan_config_str([],Line) ->
	[{'$end',Line}].
	
read_until_chars(String,Chars) ->
	read_until_chars(String,Chars,[]).
read_until_chars([Char|T],Chars,Acc) ->
	case lists:member(Char,Chars) of
		true ->
			{lists:reverse(Acc),[Char|T]};
		_Else ->
			read_until_chars(T,Chars,[Char|Acc])
	end;
read_until_chars([],_Chars,Acc) ->
	{lists:reverse(Acc),[]}.
	
make_int(Int) ->
	case catch list_to_integer(Int) of
		{'EXIT',{badarg,_}} -> Int;
		RealInt -> RealInt
	end.

get_chunkid(Data) ->
	Sha = crypto:sha(Data),
	get_chunkid16(Sha).
	
	
get_chunkid16(Data) ->
	get_chunkid16(Data,"").
get_chunkid16(<<D/integer,Rest/binary>>,Acc) when D > 15 ->
	[[Hex1,Hex2]] = io_lib:format("~.16B",[D]),
	get_chunkid16(Rest,[Hex2,Hex1|Acc]);
get_chunkid16(<<D/integer,Rest/binary>>,Acc) when D < 16 ->
	[[Hex1]] = io_lib:format("~.16B",[D]),
	get_chunkid16(Rest,[Hex1,$0|Acc]);
get_chunkid16(<<>>,Acc) ->
	lists:reverse(Acc).

%% get_chunkid64(Sha) ->
%% 	get_chunkid64(Sha,"").
%% get_chunkid64(<<D1:12,D2:12,Rest/binary>>,Acc) ->
%% 	RevFirst = int64_to_string(D1),
%% 	RevSecond = int64_to_string(D2),
%% 	RevStr = RevSecond ++ RevFirst,
%% 	get_chunkid64(Rest,RevStr ++ Acc);
%% get_chunkid64(<<>>,Acc) ->
%% 	lists:reverse(lists:flatten(Acc)).

%% int64_to_string(Int) when Int < 64 ->
%% 	[base64_enc(Int),$0];
%% int64_to_string(Int) when Int < (1 bsl 12) ->
%% 	[base64_enc((Int rem (1 bsl 6))),base64_enc(Int bsr 6)].

%% base64_enc(Int) when Int < 10 ->
%% 	$0+Int;
%% base64_enc(Int) when Int < 35 ->
%% 	$a+Int-10;
%% base64_enc(Int) when Int < 60 ->
%% 	$A+Int-35;
%% base64_enc(60) ->
%% 	$_;
%% base64_enc(61) ->
%% 	$-;
%% base64_enc(62) ->
%% 	$.;
%% base64_enc(63) ->
%% 	$%.

get_hash(ChunkId) ->
	get_hash(lists:reverse(ChunkId),<<>>).
get_hash([D1,D2,D3,D4|Rest],Acc) ->
	D1D = base64_dec(D1),
	D2D = base64_dec(D2),
	D3D = base64_dec(D3),
	D4D = base64_dec(D4),
	get_hash(Rest,<<((D3D bsr 4) + (D4D bsl 2))/integer,
					((D2D bsr 2) + ((D3D rem (1 bsl 4)) bsl 4))/integer,
					(D1D+ ((D2D rem (1 bsl 2)) bsl 6))/integer,
					Acc/binary>>);
get_hash([],Acc) ->
	Acc.


base64_dec($_) ->
	60;
base64_dec($-) ->
	61;
base64_dec($.) ->
	62;
base64_dec($%) ->
	63;
base64_dec(Char) when Char < $A ->
	Char - $0;
base64_dec(Char) when Char < $a ->
	Char - $A + 35;
base64_dec(Char) ->
	Char - $a + 10.

print_base64(String) when is_list(String) ->
	[print_base64(Char)|| Char <- String],
	io:format("\n"),
	ok;
print_base64(Char) ->
	io:format("~6.2B",[base64_dec(Char)]).

new_store() ->
    [].
put_value(Store,Key,Value) ->
    [{Key,Value}|Store].
get_value(Store,Key) when is_list(Store) ->
    proplists:get_value(Key, Store);
get_value(Key,Store) ->
	proplists:get_value(Key, Store).

pmap(Fun,List) ->
    pmap_spawn(Fun,0,self(),List).
pmap_spawn(Fun,Id,Pid,[H|T]) ->
    spawn(fun() -> 
		  Res = Fun(H),
		  Pid ! {pmap_ok,Id,Res}
	      end),
    pmap_spawn(Fun,Id+1,Pid,T);
pmap_spawn(_Fun,Id,_Pid,[]) ->
    pmap_receive(Id-1,[]).
pmap_receive(-1,Acc) ->
    Acc;
pmap_receive(Id,Acc) ->
    receive
	{pmap_ok,Id,Data} ->
	    pmap_receive(Id-1,[Data|Acc])
    end.
