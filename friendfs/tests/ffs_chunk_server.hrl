%%%-------------------------------------------------------------------
%%% File    : ffs_chunk_server.hrl
%%% Author  : Lukas Larsson <garazdawi@gmail.com>
%%% Description : Tests for ffs_chunk_server
%%%
%%% Created : 21 Aug 2009 by Lukas Larsson <garazdawi@gmail.com>
%%%-------------------------------------------------------------------


write_test_() ->
    Setup =
        fun() ->
                crypto:start(),
                ffs_chunk_server:start(eunit_test),
                file:make_dir("/tmp/ffs_eunit"),
                ffs_storage_file:start_link("file:///tmp/ffs_eunit",
                                            eunit_test),
                list_to_binary(lists:flatten(io_lib:format("~p",[element(3,now())])))
        
        end,
    Tests =
        fun(Data) ->
                io:format("Data: ~p~n", [Data]),
                {ok, ChunkId} = ffs_chunk_server:write(Data,1), 
                [?_assertEqual({ok,Data},file:read_file("/tmp/ffs_eunit/"++ChunkId)),
                 ?_assertEqual({ok,Data},ffs_chunk_server:read(ChunkId))]

        end,
    {"Test writing and reading data", {setup,Setup,Tests}}.

    
 
 
 
 
