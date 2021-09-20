%%%-------------------------------------------------------------------
%%% @author WeiMengHuan
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 五月 2020 21:50
%%%-------------------------------------------------------------------

-define(LOG(D), wx_log_server:log(wx_log, ?MODULE, ?LINE, D)).
-define(ERR(D), wx_log_server:log(wx_err, ?MODULE, ?LINE, D)).
-define(ErrDb(D), wx_log_server:log(wx_db_err, ?MODULE, ?LINE, D)).
-ifdef(DEBUG).
-define(SOUT(D),  io:format("[~p] [~p] | ~p~n", [?MODULE, ?LINE, D])).
-define(DEBUG(D), wx_log_server:log(wx_debug, ?MODULE, ?LINE, D)).
-else.
-define(SOUT(D), ok).
-define(DEBUG(D), ok).
-endif.