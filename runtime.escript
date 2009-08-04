#!/usr/bin/env escript

%%-define(DBG(Str,Args),ok).

main(Args) ->
    add_paths_to_apps(),
    case hd(Args) of
	"update_rel" ->
	    update_rel(tl(Args));
	"create_rel" ->
	    create_rel(tl(Args));
	"create_tar" ->
	    create_tar(tl(Args));
	Else ->
	    io:format("~p is an unkown function!\n",[Else])
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

    io:format("Creating rts structure!\n",[]),

    cmd("mkdir rts.backup",[]),
    cmd("mv rts rts.backup/~s",[date_to_string(calendar:local_time())]),
    
    {ok,[{release,
	  {_Relname,RelVsn},
	  {erts,ErtsVsn},
	  _Apps
	 }]} = file:consult(Relfile),

    file:make_dir("rts"),
    
    %% Figure out where to copy erts from

    KernelPath = code:which(kernel),
    
    [PathtoRuntime,_] = re:split(KernelPath,"lib/kernel",[{return,list}]),

    %% Copy erts

    cmd("cp -Lr ~s rts/",[PathtoRuntime ++ "erts-"++ErtsVsn]),

    cmd("cp -Lr ~s rts/",[PathtoRuntime ++ "bin"]),

    %% Modify rts/bin/start
    
    {ok,Bin} = file:read_file("rts/bin/start"),
    cmd("chmod +w rts/bin/start",[]),
    Str = binary_to_list(Bin),
    RootStr = re:replace(Str,"ROOTDIR=[^\\n]*",
			 io_lib:format("ROOTDIR=~s",[element(2,file:get_cwd())++"/rts"]),
			 [{return,list}]),
    DaemonStr = re:replace(RootStr,"-daemon [^$]*","-daemon $ROOTDIR/pipes/ ",
			   [{return,list}]),
    
    {ok,StartDev} = file:open("rts/bin/start",[write]),
    io:fwrite(StartDev,DaemonStr,[]),
    file:sync(StartDev),
    file:close(StartDev),

    %% create some dirs

    file:make_dir("rts/pipes"),
    file:make_dir("rts/releases"),
    file:make_dir("rts/releases/"++RelVsn),
    file:make_dir("rts/log"),

    %% create some files
    
    {ok,StartDataDev} = file:open("rts/releases/start_erl.data",[write]),
    io:format(StartDataDev,"~s ~s",[ErtsVsn,RelVsn]),
    file:sync(StartDataDev),
    file:close(StartDataDev),

    {ok,SysConfigDev} = file:open("rts/releases/"++RelVsn++"/sys.config",[write]),
    io:format(SysConfigDev,"[].",[]),
    file:sync(SysConfigDev),
    file:close(SysConfigDev),
    

    
    
    ok.


cmd(String,Args) ->
    Cmd = lists:flatten(io_lib:format(String,Args)),
    %%?DBG("EXECUTING: ~p\n",[Cmd]),
    Res = os:cmd(Cmd),
    %%?DBG("Res: ~p\n",[Res]),
    Res.


%%%%%%%%%%%%%%%%%%%5
%%%  update .rel functions
%%%%%%%%%%%%%%%%%%%5

update_rel([Relfile]) ->

    io:format("Updating .rel file\n",[]),
    
    file:copy(Relfile,Relfile++".backup"),
    
    {ok,[{release,
	  {Relname,RelVsn},
	  {erts,ErtsVsn},
	  Apps
	 }]} = file:consult(Relfile),

    UpdatedApps = [get_app_vsn(App) || App <- Apps],

    UpdatedErtsVsn = ErtsVsn,
    UpdatedRelVsn = RelVsn,

    {ok,D} = file:open(Relfile,[write]),
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
	    io:format("Error while loading ~p:\n ErrorInfo: ~p\n",[AppName,ErrorInfo]),
	    {AppName,OldVsn}
    end.
get_app_vsn(AppName,OldVsn) when is_atom(AppName),is_list(OldVsn) ->
    case lists:keysearch(AppName,1,application:loaded_applications()) of
	{value, {AppName,_AppDescr,OldVsn}} ->
	    {AppName,OldVsn};
	{value, {AppName,_AppDescr,NewVsn}} ->
	    io:format("Updating version of ~p from ~p to ~p\n",[AppName,OldVsn,NewVsn]),
	    {AppName,NewVsn}
    end.
	


add_paths_to_apps() ->
    {ok,Dirs} = file:list_dir("."),
    add_paths_to_apps(Dirs,"./").
add_paths_to_apps(["ebin"|_Tail],Prefix) ->
    code:add_patha(Prefix++"ebin");
add_paths_to_apps([Dir|Tail],Prefix) ->
    case file:read_file_info(Prefix++Dir) of
	{ok,{file_info,_,directory,_,_,_,_,_,_,_,_,_,_,_}} ->
	    {ok,Dirs} = file:list_dir(Prefix++Dir),
	    add_paths_to_apps(Dirs,Prefix++Dir++"/"),
	    add_paths_to_apps(Tail,Prefix);
	_Else ->
	    add_paths_to_apps(Tail,Prefix)
    end;
add_paths_to_apps([],_) ->
    ok.
    

%%% General functions!
date_to_string({{YY,MM,DD},{HH,Mi,SS}}) ->
    lists:flatten(io_lib:format("~p~p~pT~p~p~p",[YY,MM,DD,HH,Mi,SS])).


