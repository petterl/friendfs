-module(friendfs_sup).
-behaviour(supervisor).

-export([ start_link/0, init/1 , mount/3 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link () ->
  supervisor:start_link({local,?MODULE},?MODULE, [  ]).


%-=====================================================================-
%-                         supervisor callbacks                        -
%-=====================================================================-
%% @hidden

init([ ]) ->

  {ok, {{one_for_one, 3, 10},
	[]
       }
  }.


mount(LinkedIn,MountPoint,MountOpts) ->
    FileSystem =
        {filesystem,
	 {filesystem, start_link, [LinkedIn,MountPoint,MountOpts]},
	 transient, 10000, worker, [filesystem]},
    StorageSup =
        {ffs_storage_sup,
	 {ffs_storage_sup, start_link, []},
	 transient, 10000, worker, [ffs_storage_sup]},

    supervisor:start_child(?MODULE,FileSystem),
    supervisor:start_child(?MODULE,StorageSup).
