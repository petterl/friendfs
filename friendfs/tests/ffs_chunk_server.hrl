%%%-------------------------------------------------------------------
%%% File    : ffs_chunk_server.hrl
%%% Author  : Lukas Larsson <garazdawi@gmail.com>
%%% Description : Tests for ffs_chunk_server
%%%
%%% Created : 21 Aug 2009 by Lukas Larsson <garazdawi@gmail.com>
%%%-------------------------------------------------------------------


write_test_() ->
    Setup = fun() ->
		    ffs_chunk_server:start(eunit_test),
		    file:make_dir("/tmp/ffs_eunit"),
		    ffs_storage_file:start_link("file:///tmp/ffs_eunit",
						eunit_test),
		    timer:sleep(11000),
		    lists:flatten(io_lib:format("~p",[element(3,now())]))
	    end,

    Tests =
	fun(FileName) ->
		[?_assertEqual(ok,ffs_chunk_server:write(FileName,<<"testdata">>)),
		 ?_assertEqual({ok,<<"testdata">>},ffs_chunk_server:read(FileName))]
	end,
    {setup,Setup,Tests}.
		     
    
