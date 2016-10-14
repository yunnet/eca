%%%-------------------------------------------------------------------
%%% @author <yunnet>
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 十月 2016 14:13
%%%-------------------------------------------------------------------
-module(test_time).
-author("yunnet").

%% API
-export([a/0]).

-export([time2GB/1]).

a()->
  A = time2GB(os:timestamp()),
  io:format("Ap = ~p~n", [A]).

time2GB(DateTime)->
  {{Y, M, D}, {H, Min, S}} = calendar:now_to_local_time(DateTime),
  lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [Y rem 100, M, D, H, Min, S])).
