
main(Args) ->
    find_ebin(fun code:add_patha/1),
    case hd(Args) of
	"update_rel" ->
	    update_rel(tl(Args));
	"create_rel" ->
	    create_rel(tl(Args));
	"create_tar" ->
	    create_tar(tl(Args));
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
	  {_Relname,RelVsn},
	  {erts,ErtsVsn},
	  _Apps
	 }]} = file:consult(Relfile),

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
    dbg("EXECUTING: ~p\n",[Cmd]),
    Res = os:cmd(Cmd),
    dbg("Res: ~p\n",[Res]),
    Res.


%%%%%%%%%%%%%%%%%%%5
%%%  update .rel functions
%%%%%%%%%%%%%%%%%%%5

update_rel([Relfile]) ->

    info("Updating .rel file\n",[]),
    
    file:copy(Relfile,Relfile++".backup"),
    
    {ok,[{release,
	  {Relname,RelVsn},
	  {erts,ErtsVsn},
	  Apps
	 }]} = file:consult(Relfile),

    UpdatedApps = [get_app_vsn(App) || App <- Apps],

    UpdatedErtsVsn = case erlang:system_info(version) of
						ErtsVsn -> ErtsVsn;
						Other ->
							info("Updating Erts version from ~p to ~p\n",
							[ErtsVsn,Other]),
							Other
					end,
    UpdatedRelVsn = RelVsn,

    {ok,D} = file:open(Relfile,[write]),
    io:fwrite(D,"~p.",[{release,{Relname,UpdatedRelVsn},
			{erts,UpdatedErtsVsn},
			UpdatedApps}]),
    file:sync(D),
    file:close(D),

	find_ebin(fun(Arg) -> update_appfile(Arg) end).
    
    
    
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
find_ebin(["ebin"|_Tail],Prefix,Fun) ->
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

update_appfile(EbinPath) ->
	["ebin",AppNameStr|_] = lists:reverse(string:tokens(EbinPath,"/")),
	AppFile = EbinPath++"/"++AppNameStr++".app",
	dbg("AppFile = ~p\n",[AppFile]),
	{ok,[{application,AppName,AppData}]} = file:consult(AppFile),
	NewModules = get_modules(EbinPath,lists:keyfind(modules,1,AppData),AppName),
	NewAppData = lists:keyreplace(modules,1,AppData,{modules,NewModules}),
	{ok,Dev} = file:open(AppFile,[write]),
	io:fwrite(Dev,"~p.",[{application,AppName,NewAppData}]),
	file:sync(Dev),
	file:close(Dev).
    
get_modules(Dir,OldMods,AppName) ->
	{ok,Files} = file:list_dir(Dir),
	NewMods = [list_to_atom(ModName) || [ModName,End] <- 
	              [string:tokens(File,".") || File <- Files], End == "beam"],
	compare_mods(NewMods,OldMods,AppName),
	NewMods.
	
compare_mods(New,New,_AppName) ->
	ok;
compare_mods(New,{modules,Old},AppName) ->
	lists:map(fun(NewMod) ->
				case lists:member(NewMod,Old) of
					false ->
						info("~p was added to ~p.app\n",[NewMod,AppName]);
					_Else ->
						ok
				end
			  end,New),
	lists:map(fun(OldMod) ->
				case lists:member(OldMod,New) of
					false ->
						info("~p was removed from ~p.app\n",[OldMod,AppName]);
					_Else ->
						ok
				end
			  end,Old).


%%% General functions!
date_to_string({{YY,MM,DD},{HH,Mi,SS}}) ->
    lists:flatten(io_lib:format("~p~p~pT~p~p~p",[YY,MM,DD,HH,Mi,SS])).


dbg(_Str,_Args) ->
    ok.

info(Str,Args) ->
    io:format("Info: "++Str,Args).
