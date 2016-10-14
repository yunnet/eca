%%%-------------------------------------------------------------------
%%% @author  <yunnet>
%%% @copyright (C) 2015,
%%% @doc
%%%  tools
%%% @end
%%% Created :  19 Jan 2015 by  <>
%%%-------------------------------------------------------------------
-module(eca_utils).

-export([checksum/1]).
-export([getChannal_keys/1]).
-export([md5/1]).
-export([to_bin/1]).
-export([to_hex/1]).
-export([to_hex_upper/1]).

-export([binary_search/2]).
-export([sys_info/0]).

-export([now2utc/0]).
-export([time2GB/1, time2utc/1, datetime2utc/1]).

%%累计和
checksum(X) when is_binary(X)->
  checksum(X, 0).
checksum(<<H, T/binary>>, Acc) ->
  A = H bxor Acc,
  checksum(T, A);
checksum(<<>>, Acc) ->
  Acc.

%% @spec to_bin(string()) -> binary()
%% @doc Convert a hexadecimal string to a binary.
to_bin(L) ->
  to_bin(L, []).

to_bin([], Acc) ->
  iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
  to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).

%% @spec to_hex(integer | iolist()) -> string()
%% @doc Convert an iolist to a hexadecimal string.
to_hex(0) ->
  "0";
to_hex(I) when is_integer(I), I > 0 ->
  to_hex_int(I, []);
to_hex(B) ->
  to_hex(iolist_to_binary(B), []).

to_hex(<<>>, Acc) ->
  lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

to_hex_int(0, Acc) ->
  Acc;
to_hex_int(I, Acc) ->
  to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).

to_hex_upper(I)->
  H = to_hex(I),
  string:to_upper(H).

%% @spec hexdigit(integer()) -> char()
%% @doc Convert an integer less than 16 to a hex digit.
hexdigit(C) when C >= 0, C =< 9 ->
  C + $0;
hexdigit(C) when C =< 15 ->
  C + $a - 10.

%% @spec dehex(char()) -> integer()
%% @doc Convert a hex digit to its integer value.
dehex(C) when C >= $0, C =< $9 ->
  C - $0;
dehex(C) when C >= $a, C =< $f ->
  C - $a + 10;
dehex(C) when C >= $A, C =< $F ->
  C - $A + 10.


%%获取连接Key
getChannal_keys(_Socket)->
    {ok, {IP, _Port}} = inet:peername(_Socket),
    F = integer_to_list(_Port), 
    case IP of
	{A, B, C, D} ->
	    lists:concat(["tcp_", A, "_", B, "_", C, "_", D, "_", F]);	
	Str when is_list(Str) ->
	    Str;
	_ ->
	    []
    end.


%% hex ascii md5
-spec md5(binary()) -> string().
md5(S) -> string:to_lower(
	    lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)])).

binary_search(Tuple, Key) ->
  binary_search(Tuple, Key, 1, size(Tuple)).

binary_search(_, _, Low, High) when Low > High -> 0;
binary_search(Tuple, Key, Low, High) ->
  Mid = (Low + High) div 2,
  M = element(Mid, Tuple),
  if
    M > Key -> binary_search(Tuple, Key, Low, Mid-1);
    M < Key -> binary_search(Tuple, Key, Mid+1, High);
    true    -> Mid
  end.


sys_info() ->
  SchedId = erlang:system_info(scheduler_id),
  SchedNum = erlang:system_info(schedulers),
  ProcCount = erlang:system_info(process_count),
  ProcLimit = erlang:system_info(process_limit),
  ProcMemUsed = erlang:memory(processes_used),
  ProcMemAlloc = erlang:memory(processes),
  MemTot = erlang:memory(total),
  io:format("abormal termination:
                     ~n   Scheduler id:                         ~p
                     ~n   Num scheduler:                        ~p
                     ~n   Process count:                        ~p
                     ~n   Process limit:                        ~p
                     ~n   Memory used by erlang processes:      ~p
                     ~n   Memory allocated by erlang processes: ~p
                     ~n   The total amount of memory allocated: ~p
                     ~n",
    [SchedId, SchedNum, ProcCount, ProcLimit,
      ProcMemUsed, ProcMemAlloc, MemTot
    ]),
  ok.

%% local time convert to utc time
now2utc() ->
  time2utc(os:timestamp()).

time2utc(X) when is_tuple(X) ->
  {A, B, C} = X,
  erlang:round(A * 1000000000 + B * 1000 + C/1000).

%%Returns the number of milliseconds since January 1, 1970, 00:00:00 GMT
datetime2utc(DateTime) ->
  [T] = calendar:local_time_to_universal_time_dst(DateTime),
  (calendar:datetime_to_gregorian_seconds(T) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})) * 1000.

time2GB(DateTime)->
  {{Y, M, D}, {H, Min, S}} = calendar:now_to_local_time(DateTime),
  lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [Y rem 100, M, D, H, Min, S])).
