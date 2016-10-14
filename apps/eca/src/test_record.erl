%%%-------------------------------------------------------------------
%%% @author Net
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十月 2016 16:27
%%%-------------------------------------------------------------------
-module(test_record).
-author("yunnet").

%% API
-export([a/0, b/0]).
-export([now2utc/0]).

-record(objTrack,{
  gpsTime :: integer(),
  longitude :: integer(),
  latitude :: integer(),
  head :: integer(),
  speed :: integer(),
  height :: integer(),
  alarmState :: integer(),
  status :: integer()
}).

now2utc() ->
  time2utc(erlang:timestamp()).

time2utc(X) when is_tuple(X) ->
  {A, B, C} = X,
  Now = erlang:round(A*1000000000 + B*1000 + C/1000),
  Now.

b()->
  A = {{2015,9,1},{7,53,45}},
%%  [B] = calendar:local_time_to_universal_time_dst(A),
%%  C = time2utc(B),
%%  io:format("B=~p~n", [C]).

  B = datetime_to_now(A),
  io:format("B=~p~n", [B]).


%%Returns the number of milliseconds since January 1, 1970, 00:00:00 GMT
datetime_to_now(DateTime) ->
  [T] = calendar:local_time_to_universal_time_dst(DateTime),
  (calendar:datetime_to_gregorian_seconds(T) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})) * 1000.


a()->
  A = [
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3},
    #objTrack{gpsTime = now2utc(), longitude = 108624196, latitude = 27265763, head = 33, speed = 80, height = 5000, alarmState = 0, status = 3}
  ],
  {ok, S} = file:open("d:/track.list", write),
  lists:foreach(fun(X)->io:format(S, "~p~n", [X]) end, A),
  file:close(S),
  io:format("finish.").





