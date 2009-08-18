#!/usr/bin/env escript

main(Args) ->
    find_ebin(fun code:add_patha/1),
    load_makeconfig(),
    case hd(Args) of
	"update_rel" ->
	    update_rel(tl(Args));
	"create_rel" ->
	    create_rel(tl(Args));
	"create_tar" ->
	    create_tar(tl(Args));
	"erl_root" ->
		io:format(get_erl_root());
	"get_erts_vsn" ->
		io:format(get_erts_vsn());
	"get_app_vsn" ->
		io:format(get_application_vsn(tl(Args)));
	Else ->
	    info("~p is an unkown function!\n",[Else])
    end.

%%%%%%%%%%%%%%%%%%%5
%%%  create rel functions
%%%%%%%%%%%%%%%%%%%5

create_tar([Relfile]) ->
    RelTarget = hd(string:tokens(Relfile,".")),
    systools:make_script(RelTarget),
    systools:make_tar(RelTarget).

%%%%%%%%%%%%%%%%%%%5
%%%  create rel functions
%%%%%%%%%%%%%%%%%%%5

create_rel([Relfile]) ->

    info("Creating rts structure!\n",[]),

    cmd("mkdir rts.backup",[]),
    cmd("cp rts rts.backup/~s",[date_to_string(calendar:local_time())]),



    {ok,[{release,
	  {Relname,RelVsn},
	  {erts,ErtsVsn},
	  _Apps
	 }]} = consult(Relfile),

    file:make_dir("rts"),

    %% Figure out where to copy erts from

    KernelPath = code:which(kernel),

    [PathtoRuntime,_] = re:split(KernelPath,"lib/kernel",[{return,list}]),

    %% Copy erts

    cmd("cp -LR ~s rts/",[PathtoRuntime ++ "erts-"++ErtsVsn]),

    cmd("cp -LR ~s rts/",[PathtoRuntime ++ "bin"]),

    %% Modify rts/bin/start

    {ok,Bin} = file:read_file("rts/bin/start"),
    cmd("chmod +w rts/bin/start",[]),
    Str = binary_to_list(Bin),
    RootStr = re:replace(Str,"ROOTDIR=[^\\n]*","BINDIR=`pwd`/`dirname $0`\nROOTDIR=`dirname $BINDIR`",[{return,list}]),
    DaemonStr = re:replace(RootStr,"-daemon [^$]*","-daemon $ROOTDIR/pipes/ ",
			   [{return,list}]),
    NameStr = re:replace(DaemonStr,"\\$START_ERL_DATA",
			 "$START_ERL_DATA -pa $ROOTDIR/patches/ -sname "++Relname
			 ++" -setcookie "++Relname,
			 [{return,list}]),

    {ok,StartDev} = open("rts/bin/start",[write]),
    io:fwrite(StartDev,NameStr,[]),
    file:sync(StartDev),
    file:close(StartDev),

    %% create some dirs

    file:make_dir("rts/pipes"),
    file:make_dir("rts/releases"),
    file:make_dir("rts/releases/"++RelVsn),
    file:make_dir("rts/log"),
    file:make_dir("rts/patches"),

    %% create some files
    {ok,StartDataDev} = open("rts/releases/start_erl.data",[write]),
    io:format(StartDataDev,"~s ~s",[ErtsVsn,RelVsn]),
    file:sync(StartDataDev),
    file:close(StartDataDev),

    {ok,SysConfigDev} = open("rts/releases/"++RelVsn++"/sys.config",[write]),
    io:format(SysConfigDev,"[].",[]),
    file:sync(SysConfigDev),
    file:close(SysConfigDev),

    {ok,_} = file:copy("friendfs/script/friendfs","rts/bin/friendfs"),

    cmd("chmod -R 755 rts",[]),

    ok.


cmd(String,Args) ->
    Cmd = lists:flatten(io_lib:format(String,Args)),
    dbg("EXECUTING: ~p\n",[Cmd]),
    Res = os:cmd(Cmd),
    dbg("Res: ~p\n",[Res]),
    Res.


%%%%%%%%%%%%%%%%%%%5
%%%  update .rel functions
%%%%%%%%%%%%%%%%%%%5

update_rel([Relfile]) ->

    dbg("Updating .rel file\n",[]),

    {ok,[{release,
	  {Relname,RelVsn},
	  {erts,ErtsVsn},
	  Apps
	 }]} = consult(Relfile),

    UpdatedApps = [get_app_vsn(App) || App <- Apps],

    UpdatedErtsVsn = case erlang:system_info(version) of
			 ErtsVsn -> ErtsVsn;
			 Other ->
			     dbg("Updating Erts version from ~p to ~p\n",
							     [ErtsVsn,Other]),
			     Other
		     end,
    UpdatedRelVsn = RelVsn,

    {ok,D} = open(re:replace(Relfile,"relSrc","rel",[{return,list}]),[write]),
    io:fwrite(D,"~p.",[{release,{Relname,UpdatedRelVsn},
			{erts,UpdatedErtsVsn},
			UpdatedApps}]),
    file:sync(D),
    file:close(D).



get_app_vsn({AppName,OldVsn}) ->
    case application:load(AppName) of
	ok ->
	    get_app_vsn(AppName,OldVsn);
	{error,{already_loaded,AppName}} ->
	    get_app_vsn(AppName,OldVsn);
	{error,ErrorInfo} ->
	    info("Error while loading ~p:\n ErrorInfo: ~p\n",[AppName,ErrorInfo]),
	    {AppName,OldVsn}
    end.
get_app_vsn(AppName,OldVsn) when is_atom(AppName),is_list(OldVsn) ->
    case lists:keyfind(AppName,1,application:loaded_applications()) of
	{AppName,_AppDescr,OldVsn} ->
	    {AppName,OldVsn};
	{AppName,_AppDescr,NewVsn} ->
	    info("Updating version of ~p from ~p to ~p\n",[AppName,OldVsn,NewVsn]),
	    {AppName,NewVsn}
    end.



find_ebin(Fun) ->
    {ok,Dirs} = file:list_dir("."),
    find_ebin(Dirs,"./",Fun).
find_ebin(["rts"|Tail],Prefix,Fun) ->
    find_ebin(Tail,Prefix,Fun);
find_ebin(["fuserl"|Tail],Prefix,Fun) ->
    find_ebin(Tail,Prefix,Fun);
find_ebin(["ebin"|_Tail],Prefix,Fun) ->
    dbg("Running Fun in ~p~n", Prefix),
    Fun(Prefix++"ebin");
find_ebin([Dir|Tail],Prefix,Fun) ->
    case file:read_file_info(Prefix++Dir) of
	{ok,{file_info,_,directory,_,_,_,_,_,_,_,_,_,_,_}} ->
	    {ok,Dirs} = file:list_dir(Prefix++Dir),
	    find_ebin(Dirs,Prefix++Dir++"/",Fun),
	    find_ebin(Tail,Prefix,Fun);
	_Else ->
	    find_ebin(Tail,Prefix,Fun)
    end;
find_ebin([],_,_Fun) ->
    ok.

%%%%%%%%%%%%%%%%%%%5
%%%  parse Makefile.config functions
%%%%%%%%%%%%%%%%%%%5
load_makeconfig() ->
    case file:consult("Makefile.config") of
	{ok,[Actions]}  ->
	    [apply(M,F,A) || {M,F,A} <- Actions];
	_Else ->
	    ok
    end.

get_application_vsn([AppNameStr]) ->
	AppName = list_to_atom(AppNameStr),
    dbg("Application: ~p~n", [AppName]),
	application:load(AppName),
    LoadedApps = application:loaded_applications(),
    dbg("Applications: ~p~n", [LoadedApps]),

	case lists:keysearch(AppName,1,LoadedApps) of
    	{value,{_,_,Vsn}} -> Vsn;
	    false -> ""
	end.

get_erts_vsn() ->
	erlang:system_info(version).

get_erl_root() ->
    KernelPath = code:which(kernel),

    hd(re:split(KernelPath,"lib/kernel",[{return,list}])).

%%% General functions!
date_to_string({{YY,MM,DD},{HH,Mi,SS}}) ->
    lists:flatten(io_lib:format("~p~p~pT~p~p~p",[YY,MM,DD,HH,Mi,SS])).


consult(Str) ->
    dbg("Trying to open ~s\n",[Str]),
    file:consult(Str).

open(Str,Args) ->
    dbg("Trying to open ~s\n",[Str]),
    file:open(Str,Args).

dbg(_,_) -> ok;
dbg(Str,Args) ->
    io:format("Debug: "++Str,Args).

info(Str,Args) ->
    io:format("Info: "++Str,Args).
