-module(eca_tcp_fsm).
-author("yunnet").

-behaviour(gen_fsm).
-include("eca_proto.hrl").

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4
]).

%% FSM States
-export([
  'WAIT_FOR_SOCKET'/2,
  'WAIT_FOR_DATA'/2
]).

-record(state, {
  socket,            % client socket
  commno,
  terminaltype,
  gathered = <<>>,   % buffer
  addr,              % client address
  port,              % client port
  timeout,
  cmdseq = 0,
  process_pid = undefined
}).

-define(TIMEOUT, 360000).

%%-------------------------------------------------------------------------
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
  gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  {ok, 'WAIT_FOR_SOCKET', #state{timeout = ?TIMEOUT}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
  % Now we own the socket
  inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
  {ok, {Addr, Port}} = inet:peername(Socket),
  error_logger:info_msg("terminal: ~p:~p connect ok. ~n", [Addr, Port]),

  {next_state, 'WAIT_FOR_DATA', State#state{socket = Socket, addr = Addr, port = Port}, ?TIMEOUT};

'WAIT_FOR_SOCKET'(Other, State) ->
  error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p~n", [Other]),
  %% Allow to receive async messages
  {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, State) ->
  {ok, NewState} = handle_data({tcp, Data}, State),
  {next_state, 'WAIT_FOR_DATA', NewState, ?TIMEOUT};

'WAIT_FOR_DATA'(timeout, State) ->
  error_logger:error_msg("~p Client connection timeout - closing.~n", [self()]),
  {stop, normal, State};

'WAIT_FOR_DATA'(Data, State) ->
  error_logger:info_msg("~p Ignoring data: ~p~n", [self(), Data]),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info(timeout, StateName, #state{socket = Socket} = State) ->
  self() ! {tcp, Socket, <<>>},
  {noreply, StateName, State};

handle_info({tcp, Socket, Bin}, StateName, #state{socket = Socket} = StateData) ->
  % Flow control: enable forwarding of next TCP message
  inet:setopts(Socket, [{active, once}]),
  ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName, #state{socket = Socket, addr = Addr, port = Port} = State) ->
  error_logger:info_msg("~p Client ~p:~p disconnected.~n", [self(), Addr, Port]),
  {stop, normal, State};

handle_info(_Info, StateName, StateData) ->
  {noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket = Socket}) ->
  (catch gen_tcp:close(Socket)),
  ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%%-------------------------------------------------------------------------
%% handle_data
%% Returns: {ok, State}
%%-------------------------------------------------------------------------
handle_data({tcp, Bin}, #state{gathered = Gathered} = State) ->
  RawData = <<Gathered/binary, Bin/binary>>,
  if
    byte_size(RawData) < 13 ->
      {ok, State#state{gathered = RawData}};
    true ->
      {ok, NewState} = doRawParse(RawData, 0, State),
      {ok, NewState}
  end.

%%
doRawParse(Bin, N, #state{commno = CommNO, terminaltype = Terminal_Type} = State) ->
  case eca_code:is_package(N, Bin) of
    {ok, Len1, Len2} ->
      <<_:Len1/binary, C:Len2/binary, Data/binary>> = Bin,
      if
        CommNO /= undefined ->
          NewState = State;
        true ->
          case analysis_PI(C, State) of
            {ok, #state{} = NewState} -> ok;
            _ -> NewState = State
          end
      end,

      NewObj = #objUpRaw{genTime = os:timestamp(),
                          commNO = CommNO,
                          terminalType = Terminal_Type,
                          tunnel = 1,
                          dataHex = eca_utils:to_hex_upper(C)
                          },
      dispatchKafka(NewObj, NewState),

      case Data of
        <<>> -> {ok, NewState#state{gathered = <<>>}};
        _ -> doRawParse(Data, 0, NewState)
      end;

    error ->
      doRawParse(Bin, N + 1, State)
  end.

%% a complete package
analysis_PI(Bin, State)->
  MsgData_size = byte_size(Bin) - 2,
  %% delete head tail
  <<_, MsgData:MsgData_size/binary, _>> = Bin,

  %%escape
  P = binary:replace(MsgData, <<16#7D, 16#02>>, <<16#7E>>, [global]),
  Package = binary:replace(P, <<16#7D, 16#01>>, <<16#7D>>, [global]),

  %% check crc
  Body_size = byte_size(Package) - 1,
  <<Body:Body_size/binary, CRC1:8>> = Package,
  CRC2 = eca_utils:checksum(Body),

  case CRC1 == CRC2 of
    false -> error_logger:error_msg("CRC16 error. Old:~p <>  New:~p ~n", [CRC1, CRC2]);
    true  -> NewState = unpack_one(Package, State),
            {ok, NewState}
  end.

%%
unpack_one(Package, State) when is_binary(Package) ->
  case eca_code:decode(Package) of
    {ok,#rdtMsg{commNo = CommNO} = NewRdt} ->
      %% get terminal type
      [{_, _, R}] = eca_terminal:find(CommNO),
      NewState = State#state{commno = CommNO, terminaltype = R},

      case analysis_cmd(NewRdt, NewState) of
        {ok, NewState2 = #state{}} ->
          {ok, NewState2};
        Other -> Other
      end,

      NewState;

    _ -> State
  end.

analysis_cmd(#rdtMsg{msgId = MsgID } = NewRdt, State)->
  case MsgID of
    16#0002 -> analysis_cmd_0002(NewRdt, State);
    16#0003 -> analysis_cmd_0003(NewRdt, State);
    16#0100 -> analysis_cmd_0100(NewRdt, State);
    16#0102 -> analysis_cmd_0102(NewRdt, State);
    16#0200 -> analysis_cmd_0200(NewRdt, State);
    Other -> {error, Other}
  end.

analysis_cmd_0002(NewRdt, State) ->
  error_logger:info_msg("analysis_cmd_~p: CommNO:~p~n", [NewRdt#rdtMsg.msgSeq, State#state.commno]).

analysis_cmd_0003(NewRdt, State) ->
  error_logger:info_msg("analysis_cmd_~p: CommNO:~p~n", [NewRdt#rdtMsg.msgSeq, State#state.commno]).

analysis_cmd_0100(NewRdt, State) ->
  error_logger:info_msg("analysis_cmd_~p: CommNO:~p~n", [NewRdt#rdtMsg.msgSeq, State#state.commno]).

analysis_cmd_0102(NewRdt, State) ->
  error_logger:info_msg("analysis_cmd_~p: CommNO:~p~n", [NewRdt#rdtMsg.msgSeq, State#state.commno]).

analysis_cmd_0200(NewRdt, State) ->
  error_logger:info_msg("analysis_cmd_0200: CommNO:~p~n", [State#state.commno]),
  #rdtMsg{msgId = MsgID, commNo = CommNO, msgSeq = MsgSeq, size = Size} = NewRdt,
  error_logger:info_msg("MsgID:0x~4.16.0B CommNO:~p MsgSeq:~p Len:~p~n", [MsgID, CommNO, MsgSeq, Size]),
  analysis_cmd_8001(NewRdt, State, 16#00).

analysis_cmd_8001(#rdtMsg{msgSeq = MsgSeq, msgId = MsgId, commNo = CommNo} = NewRdt, #state{socket = Socket, cmdseq = CmdSeq} = State, _reply_result)->
  Bin = io_lib:format("~4.16.0B~4.16.0B~2.16.0B", [MsgSeq, MsgId, _reply_result]),
  Seq = (CmdSeq + 1) band 16#FFFF,
  Body = eca_code:encode(16#8001, CommNo, Bin, Seq),
  error_logger:info_msg("Reply:~p~n", [eca_utils:to_hex_upper(Body)]),
  gen_tcp:send(Socket, Body),
  State#state{cmdseq = Seq}.

dispatchKafka(NewObj, #state{terminaltype = TerminalType, commno = CommNO} = State)->
  eca_sender:dispatch(NewObj).
 %% error_logger:info_msg("dispatchKafka<~p:~p>: ~p~n", [TerminalType, CommNO, NewObj#objUpRaw.dataHex]).

