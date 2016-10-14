%%% @author  <yunnet>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 21 Jul 2016 by  <>

-module(test_mysql).

-compile(export_all).

a()->
    Options = [{host, "localhost"}, {user, "root"}, {password, "root"}, {database, "etbasedata"}],
    {ok, Pid} = mysql:start_link(Options),
    {ok, Columns, Rows} = mysql:query(Pid, <<"select commNO, terminalType from v_terminal_simple limit 100">>),
    io:format("Columns: ~p~n", [Columns]),
    [io:format("commno:~s terminalType:~p~n", [X, Y]) || [X, Y] <- Rows]
    .

time2utc(X) when is_tuple(X) ->
    {A, B, C} = X,
    Now = erlang:round(A*1000000000 + B*1000 + C/1000),
    Now.

%%Returns the number of milliseconds since January 1, 1970, 00:00:00 GMT
datetime2utc(DateTime) ->
    [T] = calendar:local_time_to_universal_time_dst(DateTime),
    (calendar:datetime_to_gregorian_seconds(T) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})) * 1000.

b()->
    io:format("==========b starting=========="),
    Options = [{host, "10.10.1.128"}, {port, 3309}, {user, "etbs"}, {password, "ycadmin@%1001"}, {database, "ettrack"}],
    case mysql:start_link(Options) of
        {ok, Pid}->
            {ok, _, Rows} = mysql:query(Pid, <<"SELECT vehicle_id, gps_time, longitude, latitude, head, gps_speed, height FROM gps_2015_09_01 WHERE vehicle_id=3358">>),

%%          [1764,{{2015,9,1},{7,53,45}},109242460,27108585,0,0,0]
            ResFun = fun(Info)->
                [A1, A2, A3, A4, A5, A6, A7] = Info,
                Gps_time = datetime2utc(A2),
                io:format("vehicle_id: ~p, gps_time: ~p, longitude: ~p, latitude: ~p, head: ~p, gps_speed: ~p, height: ~p~n",
                    [A1, Gps_time, A3, A4, A5, A6, A7])
                end,
            lists:foreach(ResFun, Rows),

            {ok, S} = file:open("d:/track.dat", write),
            lists:foreach(fun(X)->io:format(S, "~p~n", [X]) end, Rows),
            file:close(S);

        {error, Reason}->
            io:format("connect is failed:~p~n", [Reason])
    end,
    io:format("==========b finish==========").



