-module(friendfs_sup).
-behaviour(supervisor).

-export([ start_link/1, init/1]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (Args) ->
  supervisor:start_link({local,?MODULE},?MODULE, Args).


%-=====================================================================-
%-                         supervisor callbacks                        -
%-=====================================================================-
%% @hidden

init(Args) ->
    ChunkServer =
        {ffs_chunk_server,
	        {ffs_chunk_server, start, [Args]},
	        permanent, 10000, worker, [ffs_chunk_server]},

    ChunkReplicator =
        {ffs_chunk_replicator,
	        {ffs_chunk_replicator, start, [Args]},
	        permanent, 10000, worker, [ffs_chunk_replicator]},

    Storage =
        {ffs_storage_sup,
	        {ffs_storage_sup, start_link, [Args]},
	        permanent, 10000, supervisor, [ffs_storage_sup]},

    Mountpoint =
	{ffs_mountpoint_sup,
	 {ffs_mountpoint_sup, start_link, []},
	 permanent, 10000, supervisor, [ffs_mountpoint_sup]},
    
    {ok, {{one_for_one, 3, 10},
	    [ChunkServer,Storage,Mountpoint,ChunkReplicator]}}.
