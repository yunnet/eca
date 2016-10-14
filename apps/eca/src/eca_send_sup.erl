%%%-------------------------------------------------------------------
%%% @author Net
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 五月 2016 23:13
%%%-------------------------------------------------------------------
-module(eca_send_sup).
-author("yunnet").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Sender = ?CHILD(eca_sender, worker),
  {ok, { {one_for_one, 5, 10}, [Sender]} }.

