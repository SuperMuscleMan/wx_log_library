%%%-------------------------------------------------------------------
%% @doc wx_log_library public API
%% @end
%%%-------------------------------------------------------------------

-module(wx_log_library_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    wx_log_library_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
