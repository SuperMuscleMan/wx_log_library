%%%-------------------------------------------------------------------
%% @doc wx_log_library top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wx_log_library_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init(_) ->
	SupFlags = #{strategy => simple_one_for_one,
		intensity => 0,
		period => 1},
	ChildSpecs = [
		#{id => ?SERVER,
			start => {wx_log_server, start_link, []},
			restart => permanent,
			shutdown => infinity,
			type => worker,
			modules => [wx_log_server]}
	],
	{ok, {SupFlags, ChildSpecs}}.

start_child(Args)->
	supervisor:start_child(?SERVER, [Args]).

%% internal functions
