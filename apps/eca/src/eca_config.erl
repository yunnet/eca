%%%-------------------------------------------------------------------
%%% @author <yunnet>
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 十月 2016 10:21
%%%-------------------------------------------------------------------
-module(eca_config).
-author("yunnet").

%% API
-export([get_mysql_config/1, get_tcp_listener/1]).

get_tcp_listener(App)->
  case application:get_env(App, tcp_listener) of
    {ok, false} -> throw(undefined);
    {ok, Tcp_listener} ->
      try
        {_, Port} = lists:keyfind(port, 1, Tcp_listener),
        [Port]
      catch
        _:_ -> exit({bad_config, {server, {tcp_listener, config_error}}})
      end;
    undefined -> throw(undefined)
  end.

get_mysql_config(App) ->
  case application:get_env(App, mysql_config) of
    {ok, false} -> throw(undefined);
    {ok, Mysql_config} ->
      {_, Host} = lists:keyfind(host, 1, Mysql_config),
      {_, Port} = lists:keyfind(port, 1, Mysql_config),
      {_, User} = lists:keyfind(user, 1, Mysql_config),
      {_, Password} = lists:keyfind(password, 1, Mysql_config),
      {_, DB} = lists:keyfind(db, 1, Mysql_config),
      {_, Encode} = lists:keyfind(encode, 1, Mysql_config),
      [Host, Port, User, Password, DB, Encode];
    undefined -> throw(undefined)
  end.
