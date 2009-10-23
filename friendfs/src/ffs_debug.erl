%%%-------------------------------------------------------------------
%%% @author Petter Sandholdt <petter@sandholdt.se>
%%% @doc
%%% == Debug Module ==
%%%
%%% Debug helper module
%%%
%%% @end
%%% Created : 20 Oct 2009 by Petter Sandholdt <petter@sandholdt.se>
%%%-------------------------------------------------------------------
-module(ffs_debug).

-include("friendfs.hrl").

%% API
-export([log_debug/4, log_typed/5, log_error/4]).


log_error(Mod, Line, Fmt, Vars) ->
    io:format("~p:~p: **ERROR**:"++Fmt, [Mod, Line|Vars]),
    ok.

log_debug(Mod, Line, Fmt, Vars) ->
    io:format("~p:~p:"++Fmt, [Mod, Line|Vars]),
    ok.

log_typed(Type, Mod, Line, Fmt, Vars) ->
    io:format("~p:~p: #~p# "++Fmt, [Mod, Line, Type|Vars]),
    ok.

