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

    supervisor:start_child(?MODULE,FileSystem).