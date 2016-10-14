%%%-------------------------------------------------------------------
%%% @author Net
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 十月 2016 11:58
%%%-------------------------------------------------------------------
-module(eca_code).
-author("yunnet").
-include("eca_proto.hrl").

%% API
-export([encode/4, decode/1]).
-export([is_package/2]).

%%
%% MsgId :: integer 16#0200
%% CommNo :: string 013812345678
%% DataBody :: string
%% CmdSeq :: integer
-spec encode(MsgId :: integer(), CommNo :: string(), DataBody :: string(), CmdSeq :: integer()) -> binary:string().
encode(MsgId, CommNo, DataBody, CmdSeq)->
  Len = erlang:length(DataBody) div 2,
  A = lists:flatten( io_lib:format("~4.16.0B~4.16.0B~s~4.16.0B~s", [MsgId, Len, CommNo, CmdSeq, DataBody]) ),
  B = eca_utils:to_bin(A),
  C = eca_utils:checksum(B),
  D = <<B/bitstring, C:8>>,
  E = binary:replace(D, <<16#7E>>, <<16#7D, 16#02>>, [global]),
  F = binary:replace(E, <<16#7D>>, <<16#7D, 16#01>>, [global]),
  H = <<16#7E, F/bitstring, 16#7E>>,
  H.

decode(Package)->
  case Package of
    <<MsgID:16, _:2, 16#00:1, IsRas:3, DataLen:10, N:48, MsgSeq:16, Data/binary>> ->
      C1 = eca_utils:to_hex(<<N:48>>),
      NewRdt = #rdtMsg{msgId = MsgID, isSubPack = 0, isRas =  IsRas, size = DataLen, commNo = C1, msgSeq = MsgSeq, data = Data},
      {ok, NewRdt};

    <<MsgID:16, _:2, 16#01:1, IsRas:3, DataLen:10, N:48, MsgSeq:16, SubCount:16, SubSeq:16, Data/binary>> ->
      C1 = eca_utils:to_hex(<<N:48>>),
      NewRdt = #rdtMsg{msgId = MsgID, isSubPack = 1, isRas =  IsRas, size = DataLen, commNo = C1, msgSeq = MsgSeq, subCount = SubCount, subSeq = SubSeq, data = Data},
      {ok, NewRdt};

    Other ->{error, Other}
  end.


%%判断一个完整的包  分解出: 7E XX XX XX 7E
is_package(N, Bin)->
  <<_:N/binary, Rest/binary>> = Bin,
  try
    gather(Rest, <<>>, N)
  catch
    _:_-> error
  end .

%%search header
gather(<<16#7E, T/binary>>, L, N) ->
  case T of
    %%7E7E...7E
    <<16#7E, T1/binary>> ->
      {A, T2} = collect(T1, <<16#7E>>),
      gather(T2, <<A/binary, L/binary>>, N + 1);
    _ ->
      {I, _} = collect(T, <<16#7E>>),
      gather(<<>>, I, N)
  end;
gather(<<_, T/binary>>, L, N) ->
  gather(T, L, N + 1);
gather(<<>>, L, N)->
  {ok, N, byte_size(L)}.

%% search tail
collect(<<16#7E, T/binary>>, L)->
  {<<L/binary, 16#7E>>, T};
collect(<<H:8, T/binary>>, L)->
  collect(<<T/binary>>, <<L/binary, H>>);
collect(<<>>, _)->
  {<<>>, <<>>}.