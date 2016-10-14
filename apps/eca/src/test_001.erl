%%%-------------------------------------------------------------------
%%% @author <yunnet>
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 六月 2016 14:48
%%%-------------------------------------------------------------------
-module(test_001).
-author("yunnet").

-compile(export_all).

-record(rdtMsg,{
  msgId :: integer(),
  isSubPack :: boolean(),
  isRas :: boolean(),
  size :: integer(),
  commNo :: char(),
  msgSeq :: integer(),
  subCount :: integer(),
  subSeq :: integer(),
  data :: char()
}).

%% 原始数据是：7E........7E7E........7E    19,68,128,97,126,
a()->
  A = [19,68,128,97,126,126,2,0,0,38,1,49,0,0,0,1,0,150,0,0,0,0,0,0,0,3,1,94,66,154,6,193,206,102,0,0,0,0,0,164,22,6,8,20,70,52,3,2,0,0,1,4,0,19,68,128,97,126,126,2,0,0,38,1,49,0,0,0,1,0,150,0,0,0,0,0,0,0,3,1,94,66,154,6,193,206,102,0,0,0,0,0,164,22,6,8,20,70,52,3,2,0,0,1,4,0,19,68,128,97,126],
  B = list_to_binary(A),
  io:format("B(Hex)=~p~n", [to_hex_upper(B)]),
  processData(B),
  ok.


b()->
A = 16#0,
B = 16#3,
Lat = 26480801,
Lon = 107564051,
Height = 100,
Speed = 40,
Head = 30,
Time = os:timestamp(),

DataBody = lists:flatten( io_lib:format("~8.16.0B~8.16.0B~8.16.0B~8.16.0B~4.16.0B~4.16.0B~4.16.0B~s", [A, B, Lat, Lon, Height, Speed, Head, eca_utils:time2GB(Time)]) ),
error_logger:info_msg("DataBody: ~p~n", [DataBody]),

sendData("013180001234", 16#0200, 1, DataBody).

sendData(CommNO, Cmd, Seq, DataBody) ->
  Bin = eca_code:encode(Cmd, CommNO, DataBody, Seq),
  error_logger:info_msg("send=> ~p~n", [eca_utils:to_hex_upper(Bin)]).


processData(Bin)->
  processData(Bin, 0).

%%
processData(Bin, N)->
  case is_package(N, Bin) of
    {ok, Len1, Len2} ->
      <<_:Len1/binary, C:Len2/binary, Data/binary>> = Bin,

      analysis_PI(C),
      io:format("*********************************************************~n~n"),

      case Data of
        <<>> -> ok;
        _ -> processData(Data, 0)
      end;

    error ->
      processData(Bin, N + 1)
  end.

%%判断一个完整的包  分解出: 7E XX XX XX 7E    ?????????
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
      %% gather(T3, <<A/binary, L/binary>>, N)
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


%% a complete package
analysis_PI(Bin)->
  io:format("Bin:~p~n", [Bin]),
  MsgData_size = byte_size(Bin) - 2,
  %% delete head tail
  <<_, MsgData:MsgData_size/binary, _>> = Bin,

  %%escape
  P = binary:replace(MsgData, <<16#7D, 16#02>>, <<16#7E>>, [global]),
  Package = binary:replace(P, <<16#7D, 16#01>>, <<16#7D>>, [global]),

  io:format("Package:~p~n", [Package]),

  %% check crc
  Body_size = byte_size(Package) - 1,
  <<Body:Body_size/binary, CRC1:8>> = Package,
  CRC2 = checksum(Body),
  io:format("CRC1:~p  CRC2:~p ~n", [CRC1, CRC2]),
  case CRC1 == CRC2 of
    true  -> unpack(Package);
    false -> io:format("CRC16 error. Old:~p <>  New:~p ~n", [CRC1, CRC2])
  end,
  ok.

%%
unpack(Package) when is_binary(Package) ->
  case Package of
    <<MsgID:16, _:2, 16#00:1, IsRas:3, DataLen:10, N:48, MsgSeq:16, Data/binary>> ->
      C1 = to_hex(<<N:48>>),
      NewRdt = #rdtMsg{msgId = MsgID, isSubPack = 0, isRas =  IsRas, size = DataLen, commNo = C1, msgSeq = MsgSeq, data = Data},
      analysis_cmd(NewRdt);

    <<MsgID:16, _:2, 16#01:1, IsRas:3, DataLen:10, N:48, MsgSeq:16, SubCount:16, SubSeq:16, Data/binary>> ->
      C1 = to_hex(<<N:48>>),
      NewRdt = #rdtMsg{msgId = MsgID, isSubPack = 1, isRas =  IsRas, size = DataLen, commNo = C1, msgSeq = MsgSeq, subCount = SubCount, subSeq = SubSeq, data = Data},
      analysis_cmd(NewRdt);

    Other ->{error, Other}
  end
.

analysis_cmd(#rdtMsg{msgId = MsgID } = NewRdt)->
  case MsgID of
    16#0002 -> analysis_cmd_0002(NewRdt);
    16#0003 -> analysis_cmd_0003(NewRdt);
    16#0100 -> analysis_cmd_0100(NewRdt);
    16#0102 -> analysis_cmd_0102(NewRdt);
    16#0200 -> analysis_cmd_0200(NewRdt);
    Other -> {error, Other}
  end.

analysis_cmd_0002(NewRdt) ->
  erlang:error(not_implemented).

analysis_cmd_0003(NewRdt) ->
  erlang:error(not_implemented).

analysis_cmd_0100(NewRdt) ->
  erlang:error(not_implemented).

analysis_cmd_0102(NewRdt) ->
  erlang:error(not_implemented).

analysis_cmd_0200(NewRdt) ->
  #rdtMsg{msgId = MsgID, commNo = CommNO, msgSeq = MsgSeq, size = Size, data = Data} = NewRdt,
  io:format("MsgID:0x~p, CommNO:~p, MsgSeq:~p, Len:~p, Data:~p~n", [integer_to_list(MsgID, 16), CommNO, MsgSeq, Size, Data]),
  analysis_cmd_8001(NewRdt, 16#00),
  ok.

analysis_cmd_8001(#rdtMsg{msgSeq = MsgSeq, msgId = MsgId, commNo = CommNo} = NewRdt, _relpy_result)->
  DataBody = io_lib:format("~4.16.0B~4.16.0B~2.16.0B", [MsgSeq, MsgId, _relpy_result]),
  A = buildCmd(16#8001, CommNo, DataBody),
  io:format("A=~p~n", [eca_utils:to_hex_upper(A)]),
  ok.

buildCmd(MsgId, CommNo, DataBody)->
  Len = erlang:length(DataBody) div 2,
  ComSeq = 1,
  A = lists:flatten( io_lib:format("~4.16.0B~4.16.0B~s~4.16.0B~s", [MsgId, Len, CommNo, ComSeq, DataBody]) ),
  B = eca_utils:to_bin(A),
  C = eca_utils:checksum(B),
  D = <<B/bitstring, C:8>>,
  E = binary:replace(D, <<16#7E>>, <<16#7D, 16#02>>, [global]),
  F = binary:replace(E, <<16#7D>>, <<16#7D, 16#01>>, [global]),
  H = <<16#7E, F/bitstring, 16#7E>>,
  H.


%%累计和
checksum(X) when is_binary(X)->
  checksum(X, 0).
checksum(<<H, T/binary>>, Acc) ->
  A = H bxor Acc,
  checksum(T, A);
checksum(<<>>, Acc) ->
  Acc.


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
