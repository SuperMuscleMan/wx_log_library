-module(wx_log_server).
%%%=======================STATEMENT====================
-description("wx_log_server").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
-vsn(1).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export([log/4]).
%%%=======================INCLUDE======================
-include_lib("wx_log_library/include/wx_log.hrl").
%%%=======================DEFINE=======================
-define(SERVER, ?MODULE).
-define(Cache_TimeOut, 5000).  %%缓存时间(毫秒)
-define(Cache_Size, 0). %%缓存大小
-define(Hibernate_TimeOut, 10000). %%休眠超时时间(毫秒)
-define(Sync_Disc_TimeOut, 200). %% 同步超时时间

%%%=======================RECORD=======================
-record(state, {
	file,%%文件名称
	interval,%%切换文件间隔
	num_limit,%%文件个数限制
	
	queue,%%缓存队列
	len,%%缓存长度
	next_time,%%切换文件的下一个时间点
	
	io_device, %%文件io句柄
	
	idle
}).

%%%===================================================================
%%% API
%%%===================================================================

%%%=================EXPORTED FUNCTIONS=================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Cfg :: [term()]) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Cfg) ->
	WordSize = erlang:system_info(wordsize),
	gen_server:start_link(?MODULE, Cfg,
		[{spawn_opt, [{min_heap_size, 128 * 1024 div WordSize}, {fullsweep_after, 128},
			{max_heap_size, #{size => 256 * 1024 div WordSize,
				kill => false,
				error_logger => false}}]}]).

%% -----------------------------------------------------------------
%% Func:
%% Description:记录日志
%% Returns:
%% -----------------------------------------------------------------
log(Type, Mod, Line, Data) ->
	gen_server:cast(Type, {log, wx_time:local_millisec(), self(),
		Mod, Line, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init(Cfg) ->
	case validate_cfg(Cfg) of
		ok ->
			Name = wx_lib:get_value(Cfg, name),
			register(Name, self()),
			File = proplists:get_value(file, Cfg),
			Interval = proplists:get_value(interval, Cfg),
			NumLimit = proplists:get_value(num_limit, Cfg),
			{IoDevice, NextSecond} = wx_log_file:init(File, Interval, NumLimit),
			{ok, #state{file = File, interval = Interval, num_limit = NumLimit, queue = [],
				len = 0, next_time = NextSecond, io_device = IoDevice, idle = true}, ?Hibernate_TimeOut};
		Error ->
			{stop, Error}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast({log, _Time, _Pid, _Mod, _Line, _Data} = Msg, #state{file = FileDir,
	queue = Queue, len = Len, io_device = Io, next_time = NextSecond, interval = Interval,
	num_limit = NumLimit} = State) ->
	if
		Len + 1 < ?Cache_Size ->
			{noreply, State#state{queue = [Msg | Queue], len = Len + 1, idle = true}, ?Cache_TimeOut};
		true ->
			%% 进行写入文件
			{NewIo, NextTime} = wx_log_file:log(Io, FileDir, NextSecond, Interval, NumLimit, [Msg | Queue]),
			{noreply, State#state{queue = [], len = 0, next_time = NextTime, io_device = NewIo,
				idle = false}, ?Sync_Disc_TimeOut}
	end;
handle_cast(_Request, State) ->
	{noreply, State, ?Hibernate_TimeOut}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(timeout, #state{queue = [], io_device = Io, idle = false} = State) ->
	wx_log_file:sync_disc(Io),
	{noreply, State#state{idle = true}, ?Hibernate_TimeOut};
handle_info(timeout, #state{queue = []} = State) ->
	{noreply, State, hibernate};
handle_info(timeout, #state{queue = Queue, next_time = NextSecond, io_device = Io,
	file = FileDir, interval = Interval, num_limit = NumLimit} = State) ->
	%% 进行写入文件
	{NewIo, NextTime} = wx_log_file:log(Io, FileDir, NextSecond, Interval, NumLimit, Queue),
	{noreply, State#state{queue = [], len = 0, next_time = NextTime, io_device = NewIo,
		idle = false}, ?Sync_Disc_TimeOut};
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% -----------------------------------------------------------------
%% Func:
%% Description:验证配置
%% Returns:
%% -----------------------------------------------------------------
validate_cfg(Cfg) ->
	case do_validate_cfg(Cfg, [name, file, interval, num_limit]) of
		ok ->
			ok;
		Error ->
			Error
	end.
do_validate_cfg(_, []) ->
	ok;
do_validate_cfg(Cfg, [Prop | Rest]) ->
	case proplists:get_value(Prop, Cfg) of
		undefined ->
			{error, {"miss_cfg_prop", Prop}};
		_ ->
			do_validate_cfg(Cfg, Rest)
	end.