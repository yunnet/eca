%%%-------------------------------------------------------------------
%% @doc eca public API
%% @end
%%%-------------------------------------------------------------------

-module(eca_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  %% load ets
  case eca_terminal:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
  end,

  case eca_terminal_mgr:start_link() of
    {ok, Pid2} ->
      io:format(":::::: mysql mgr start ok. ~p~n", [Pid2]),
      {ok, Pid2};
    Other2 ->
      io:format("mysql mgr faild.~p~n", [Other2]),
      {error, Other2}
  end,

%% load root
  case eca_sup:start_link() of
    {ok, Pid3} ->
      io:format(":::::: eca_sup start ok. ~p~n", [Pid3]),
      {ok, Pid3};
    Other3 ->
      {error, Other3}
  end.


%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
