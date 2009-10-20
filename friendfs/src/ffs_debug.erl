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
-export([log_debug/4, log_debug/5, log_error/4]).


log_error(Mod, Line, Fmt, Vars) ->
    ok.

log_debug(Mod, Line, Fmt, Vars) ->
    ok.

log_debug(Type, Mod, Line, Fmt, Vars) ->
    ok.

