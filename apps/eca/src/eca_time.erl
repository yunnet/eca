%%%-------------------------------------------------------------------
%%% @author Net
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 十月 2016 13:04
%%%-------------------------------------------------------------------
-module(eca_time).
-author("yunnet").

%% API
-compile(export_all).


%% time format
one_to_two(One) -> io_lib:format("~2..0B", [One]).

time_format(Now) ->
  {{Y, M, D}, {H, MM, S}} = calendar:now_to_local_time(Now),
  lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D), " ",
    one_to_two(H), ":", one_to_two(MM), ":", one_to_two(S)]).
date_format(Now) ->
  {{Y, M, D}, {_H, _MM, _S}} = calendar:now_to_local_time(Now),
  lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D)]).
date_hour_format(Now) ->
  {{Y, M, D}, {H, _MM, _S}} = calendar:now_to_local_time(Now),
  lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D), " ", one_to_two(H)]).
date_hour_minute_format(Now) ->
  {{Y, M, D}, {H, MM, _S}} = calendar:now_to_local_time(Now),
  lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D), " ", one_to_two(H), "-", one_to_two(MM)]).
%% split by -
minute_second_format(Now) ->
  {{_Y, _M, _D}, {H, MM, _S}} = calendar:now_to_local_time(Now),
  lists:concat([one_to_two(H), "-", one_to_two(MM)]).

hour_minute_second_format(Now) ->
  {{_Y, _M, _D}, {H, MM, S}} = calendar:now_to_local_time(Now),
  lists:concat([one_to_two(H), ":", one_to_two(MM), ":", one_to_two(S)]).


%% local time convert to utc time
now2utc() ->
  time2utc(os:timestamp()).

time2utc(X) when is_tuple(X) ->
  {A, B, C} = X,
  erlang:round(A * 1000000000 + B * 1000 + C / 1000).

%%Returns the number of milliseconds since January 1, 1970, 00:00:00 GMT
datetime2utc(DateTime) ->
  [T] = calendar:local_time_to_universal_time_dst(DateTime),
  (calendar:datetime_to_gregorian_seconds(T) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})) * 1000.

time2GB(DateTime) ->
  {{Y, M, D}, {H, Min, S}} = calendar:now_to_local_time(DateTime),
  lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [Y rem 100, M, D, H, Min, S])).
