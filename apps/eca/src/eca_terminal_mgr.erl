%%%-------------------------------------------------------------------
%%% @author  <yunnet>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 21 Jul 2016 by  <>
%%%-------------------------------------------------------------------
-module(eca_terminal_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([query/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("eca_commno.hrl").

-record(state, {pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
query(Args)->
    gen_server:call({query, Args}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Options = [{host, "localhost"}, {user, "root"}, {password, "root"}, {database, "etbasedata"}],
    {ok, Pid} = mysql:start_link(Options),

    erlang:send_after(10, self(), try_to_connect),
    {ok, #state{pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({query, Args}, _From, #state{pid = Pid} = State) ->
    io:format("Args: ~p~n", [Args]),
    Reply = mysql:query(Pid, Args),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(try_to_connect, #state{pid = Pid} = State) ->
    {ok, _, Rows} = mysql:query(Pid, <<"select commNO, terminalType from v_terminal_simple">>),
    [addTerminal(X, Y) || [X, Y] <- Rows],
    error_logger:info_msg("load terminal count: ~p~n", [length(Rows)]),
    {noreply, State}.

addTerminal(_CommNO, _TerminalType)->
    eca_terminal:add(#terminalInfo{commno = _CommNO, terminalType = _TerminalType}).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

