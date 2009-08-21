%%%-------------------------------------------------------------------
%%% File    : ffs_fat.hrl
%%% Author  : Lukas Larsson <garazdawi@gmail.com>
%%% Description : Tests for ffs_fat
%%%
%%% Created : 21 Aug 2009 by Lukas Larsson <garazdawi@gmail.com>
%%%-------------------------------------------------------------------

make_dir_test_() ->
	Setup = fun() ->
				catch ets:delete(?COUNTER_TABLE),
				init_counter(),
				Tid = init(test,{ffs_fat,test_callback,[]}),
				make_dir(Tid,1,"tmp",1,1,?A),
				Tid
			end,
	Tests = fun(#ffs_tid{ inode = Inode, link = Link }) ->
					[?_assert(1 =:= 1)]
			end,
	{setup,Setup,Tests}.