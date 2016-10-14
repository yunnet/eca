%%%-------------------------------------------------------------------
%%% @author  <yunnet>
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created : 7 Sep 2014 by  <yunnet>
%%%-------------------------------------------------------------------
-module(ca).

-export([start/0, stop/0, restart/0, ob/0]).

start() ->
  dbg:start(),     % start dbg
  dbg:tracer(),    % start a simple tracer process
  dbg:p(all, c),   % trace calls (c) of that MFA for all processes.
  application:start(mysql),
  application:start(eca),
  ok.

stop() ->
  dbg:stop(),
  application:stop(eca).

restart() ->
  case stop() of
    ok ->
      start();
    {error, {not_started, couch}} ->
      start();
    {error, Reason} ->
      {error, Reason}
  end.

ob()->
  observer:start().

    
