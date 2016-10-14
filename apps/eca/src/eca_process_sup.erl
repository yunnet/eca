%%%-------------------------------------------------------------------
%%% @author Net
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 五月 2016 23:12
%%%-------------------------------------------------------------------
-module(eca_process_sup).
-author("yunnet").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     temporary, brutal_kill, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(CommNO) ->
  supervisor:start_child(?MODULE, [CommNO]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {
    {simple_one_for_one, 0, 1},
    [?CHILD(eca_process_child, eca_process_child, worker, [])]
  }}.
