%%%-------------------------------------------------------------------
%%% @author  <yunnet>
%%% @copyright (C) 2016, 
%%% @
%%% @end
%%% Created : 21 Jul 2016 by  <>
%%%-------------------------------------------------------------------
-module(eca_terminal).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([add/1, delete/1, find/1]).

-define(SERVER, ?MODULE).
-define(ETS_TERMINAL, terminalinfo).


-include("eca_commno.hrl").

-record(state, {}).

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
add(Terminal)->
    gen_server:call(?MODULE, {add, Terminal}).

delete(Key)->
    gen_server:call(?MODULE, {delete, Key}).

find(Key)->
    gen_server:call(?MODULE, {find, Key}).

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
    process_flag(trap_exit, true),
    ets:new(?ETS_TERMINAL, [public,
			                      ordered_set,
		                        named_table,
			                     {keypos, #terminalInfo.commno}
		                        ]),
    {ok, #state{}}.

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
handle_call({add, Terminal}, _From, State) ->
    ets:insert(?ETS_TERMINAL, Terminal),
    Reply = ok,
    {reply, Reply, State};
handle_call({delete, Key}, _From, State) ->
    ets:delete(?ETS_TERMINAL, Key),
    Reply = ok,
    {reply, Reply, State};
handle_call({find, Key}, _From, State) ->
    Reply = getinfo(Key),
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
handle_info(_Info, State) ->
    {noreply, State}.

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
getinfo([])->    
    case ets:first(?ETS_TERMINAL) of
      '$end_of_table'->
          [];
      Key->
        ets:lookup(?ETS_TERMINAL, Key)
    end;
getinfo(Key)->	
    case ets:next(?ETS_TERMINAL, Key) of
      '$end_of_table'->
          [];
      Next->
          ets:lookup(?ETS_TERMINAL, Next)
    end.

