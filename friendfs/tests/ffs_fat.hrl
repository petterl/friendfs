%%%-------------------------------------------------------------------
%%% File    : ffs_fat.hrl
%%% Author  : Lukas Larsson <garazdawi@gmail.com>
%%% Description : Tests for ffs_fat
%%%
%%% Created : 21 Aug 2009 by Lukas Larsson <garazdawi@gmail.com>
%%%-------------------------------------------------------------------

%% init_test_() ->
%%     Setup = fun() ->
%% 		    catch ets:delete(?COUNTER_TABLE),
%% 		    init_counters(),
%%             Tid = init("test",-1,-1,1 bsl 15),
%% 		    Tid
%% 	    end,
%%     Tests =
%% 	fun(Tid) ->
%% 		inode_check(Tid,#ffs_inode{ inode = 1,
%% 					    hash = 0,
%% 					    size = 0,
%% 					    uid = -1,
%% 					    gid = -1,
%% 					    mode = 8#755 bor ?D,
%% 					    refcount = 1 })
%% 	    end,
%%     {setup,Setup,Tests}.





%% inode_check(Tid,#ffs_inode{ inode = Inode, hash = Hash, size = Size, uid = Uid,
%% 			    gid = Gid, mode = Mode, refcount = Refs }) ->
%%     [
%%      ?_assertEqual((lookup(Tid,Inode))#ffs_inode.inode,Inode),
%%      ?_assertEqual((lookup(Tid,Inode))#ffs_inode.hash,Hash),
%%      ?_assertEqual((lookup(Tid,Inode))#ffs_inode.size,Size),
%%      ?_assertEqual((lookup(Tid,Inode))#ffs_inode.uid,Uid),
%%      ?_assertEqual((lookup(Tid,Inode))#ffs_inode.gid,Gid),
%%      ?_assertEqual((lookup(Tid,Inode))#ffs_inode.mode,Mode),
%%      ?_assertEqual((lookup(Tid,Inode))#ffs_inode.ctime,(lookup(Tid,1))#ffs_inode.atime),
%%      ?_assertEqual((lookup(Tid,Inode))#ffs_inode.atime,(lookup(Tid,1))#ffs_inode.mtime),
%%      ?_assertEqual((lookup(Tid,Inode))#ffs_inode.refcount,Refs)
%%     ].
	


%% write_test_() ->
%%     Setup = fun() ->
%% 		    catch ets:delete(?COUNTER_TABLE),
%% 		    init_counters(),
%% 		    application:start(crypto),
%% 		    Tid = init("test",-1,-1,1 bsl 10),
%% 		    Tid
%% 	    end,
%%     Tests = fun(Tid) ->
%% 		    NewInode = create(Tid,1,"test.txt",-1,-1,8#755,0,0),
%% 		    Chunks1 = write_cache(Tid,NewInode#ffs_inode.inode,
%%                     <<"test text">>,0),
%% 		    Chunks2 = flush_cache(Tid,NewInode#ffs_inode.inode),
%% 		    [?_assertEqual([],Chunks1),
%% 		     ?_assertEqual(
%% 			{chunk,"6AFC05EAE22E994F1C7DD48E58F8895DD9028223",
%% 			 <<"test text">>},Chunks2)]
%% 	    end,
%%     {setup,Setup,Tests}.


