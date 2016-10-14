%%%-------------------------------------------------------------------
%%% @author  <yunnet>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2013 by  <yunnet>
%%%-------------------------------------------------------------------
-module(eca_tcp_sup).

-behaviour(supervisor).

-export([start_link/2, start_client/0]).
-export([count_children/0]).
-export([init/1]).

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).



start_link(ListenPort, ServiceMod) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, ServiceMod]).

%%----------------------------------------------------------------------
%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
%%----------------------------------------------------------------------
start_client() ->
  supervisor:start_child(tcp_client_sup, []).

count_children() ->
  supervisor:count_children(tcp_client_sup).


%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, Module]) ->
  % TCP Listener
  Tcp_sup = {eca_tcp_sup,                                % Id       = internal id
    {eca_tcp_listener, start_link, [Port, Module]},    % StartFun = {M, F, A}
    permanent,                                            % Restart  = permanent | transient | temporary
    2000,                                                  % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                               % Type     = worker | supervisor
    [eca_tcp_listener]                                  % Modules  = [Module] | dynamic
  },

  % Client instance supervisor
  Client_mgr = {tcp_client_sup,
    {supervisor, start_link, [{local, tcp_client_sup}, ?MODULE, [Module]]},
    permanent,                               % Restart  = permanent | transient | temporary
    infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
    supervisor,                              % Type     = worker | supervisor
    []                                        % Modules  = [Module] | dynamic
  },

  Children = [Tcp_sup, Client_mgr],
  RestartStrategy = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
  {ok, {RestartStrategy, Children}};

init([Module]) ->
  % TCP Client
  Client_mgr = {undefined,                   % Id       = internal id
    {Module, start_link, []},               % StartFun = {M, F, A}
    temporary,                              % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
  },

  Children = [Client_mgr],
  RestartStrategy = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
  {ok, {RestartStrategy, Children}}.

