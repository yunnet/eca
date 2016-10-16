%%% @author  <>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 21 Jul 2016 by  <>

%%ets read-write 属性
-define(ETSRC, {read_concurrency, true}).
-define(ETSWC, {write_concurrency, true}).

-record(terminalInfo, {commno, terminalType}).


