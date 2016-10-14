%%%-------------------------------------------------------------------
%% @doc eca top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eca_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(TCP_PORT, 6000).    %%listen terminal send data.  侦听客户端发来的数据

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).


-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
%%    {ok, { {one_for_all, 0, 1}, []} }.
    Tcp_sup = {eca_tcp_sup, {eca_tcp_sup, start_link, [?TCP_PORT, eca_tcp_fsm]},
        permanent,
        2000,
        supervisor,
        [tcp_sup]
    },

    Process_sup = {eca_process_sup, {eca_process_sup, start_link, []},
        permanent,
        2000,
        supervisor,
        [eca_process_sup]
    },

    Send_sup = {eca_send_sup, {eca_send_sup, start_link, []},
        permanent,
        2000,
        supervisor,
        [eca_send_sup]
    },

    Children = [Tcp_sup, Process_sup, Send_sup],
    RestartStrategy = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
