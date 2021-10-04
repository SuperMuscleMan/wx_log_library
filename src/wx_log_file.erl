-module(wx_log_file).
%%%=======================STATEMENT====================
-description("wx_file").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
-vsn(1).
%%%=======================EXPORT=======================
-export([init/3, write/3, log/6, sync_disc/1]).
%%%=======================INCLUDE======================

%%%=======================RECORD=======================

%%%=======================DEFINE=======================
-define(File_Name_Suffix, ".log").
%%正则匹配 例如：2019—8-16_23-02 结果：{match,["2019-8-16_23-02","2019","8","16","23","02"]}
-define(Rex_Time, "(\\d\+)-(\\d\+)-(\\d\+)_(\\d\+)-(\\d\+)").
%%一天的秒数
-define(Day_Seconds, 86400).
%%一天的分钟数
-define(Day_Minute, 1440).
%%1970年的天数
-define(Days_1970, 719528).
%%东八区秒数(3600*8)
-define(DongBaDistrct, 28800).
%%%=================EXPORTED FUNCTIONS=================

%% -----------------------------------------------------------------
%% Func:
%% Description: 获取下一个循环时间点(整点)
%% Returns:
%% -----------------------------------------------------------------
get_cycle_second(Second, Interval) ->
	Rest = Second rem ?Day_Seconds,
	(Second - Rest) + ((Rest div Interval + 1) * Interval).

%% -----------------------------------------------------------------
%% Func:
%% Description: 日志文件初始化
%% Returns:
%% -----------------------------------------------------------------
init(FileDir, Interval, NumLimit) ->
	case filelib:ensure_dir(FileDir) of
		ok ->
			create_file(FileDir, wx_time:local_second(), Interval, NumLimit);
		Error ->
			erlang:error({"make_dir_fail", Error, {fileDir, FileDir}}, [FileDir, Interval])
	end.
%% -----------------------------------------------------------------
%% Func:
%% Description:创建文件
%% Returns:
%% -----------------------------------------------------------------
create_file(FileDir, LocalSecond, Interval, NumLimit) ->
	Dir = filename:dirname(FileDir),
	BaseName = filename:basename(FileDir),
	FileName = make_time_name(LocalSecond, BaseName),
	case file_io_open(Dir, FileName) of
		{ok, IoDevice} ->
			NextSecond = get_cycle_second(LocalSecond, Interval),
			del_file_out(Dir, BaseName, NumLimit),
			{IoDevice, NextSecond};
		Error ->
			erlang:error({"file_open_fail", Error, {filename, FileName}})
	end.

%% -----------------------------------------------------------------
%% Func:
%% Description:记录日志
%% Returns:
%% -----------------------------------------------------------------
log(IoDevice, FileDir, NextSecond, Interval, NumLimit, LogList) ->
	LocalSecond = wx_time:local_second(),
	{NewIoDevice, NewNextSecond} =
		if
			LocalSecond < NextSecond ->
				{IoDevice, NextSecond};
			true ->
				close(IoDevice),
				create_file(FileDir, get_cycle_second(LocalSecond - Interval, Interval), Interval, NumLimit)
		end,
	write(NewIoDevice, FileDir, LogList),
	{NewIoDevice, NewNextSecond}.
%% -----------------------------------------------------------------
%% Func:
%% Description:关闭文件io
%% Returns:
%% -----------------------------------------------------------------
close(IoDevice) ->
	file:datasync(IoDevice),
	file:close(IoDevice).

%% -----------------------------------------------------------------
%% Func:
%% Description:写入文件
%% Returns:
%% -----------------------------------------------------------------
write(_, _, []) ->
	ok;
write(IoDevice, FileDir, [Msg | T]) ->
	case file:write(IoDevice, format_2(Msg)) of
		ok ->
			write(IoDevice, FileDir, T);
		Error ->
			erlang:error({"wrtie_log_fail", Error}, [IoDevice, FileDir, Msg])
	end.

%% -----------------------------------------------------------------
%% Func:
%% Description:删除过期的
%% Returns:
%% -----------------------------------------------------------------
del_file_out(Dir, BaseName, Limit) ->
	Prefix = filename:rootname(BaseName),
	{ok, FileList} = file:list_dir(Dir),
	ValiList = [E || E <- FileList, string:str(E, Prefix) > 0],
	Len = length(ValiList),
	if
		Len < Limit ->
			ok;
		true ->
			{DelList, _} = lists:split(Len - Limit, ValiList),
			del_file_out_(Dir, DelList)
	end.

del_file_out_(_Dir, []) ->
	ok;
del_file_out_(Dir, [FileName | T]) ->
	catch file:delete(filename:join(Dir, FileName)),
	del_file_out_(Dir, T).

%% -----------------------------------------------------------------
%% Func:
%% Description:格式化log格式
%% Returns:
%% -----------------------------------------------------------------
%%format({log, MilliSecond, Pid, Mod, Line, Data}) ->
%%	{{Y, M, D}, {H, Minute, S}, MilliSec} = wx_time:millisecond_to_datetime(MilliSecond),
%%	[
%%		wx_lib:integer_to_list(Y, 4), $-,
%%		wx_lib:integer_to_list(M, 2), $-,
%%		wx_lib:integer_to_list(D, 2), $\s,
%%		wx_lib:integer_to_list(H, 2), $:,
%%		wx_lib:integer_to_list(Minute, 2), $:,
%%		wx_lib:integer_to_list(S, 2), $.,
%%		wx_lib:integer_to_list(MilliSec, 3), $\s,
%%		pid_to_list(Pid), $\s,
%%		atom_to_list(Mod), $:,
%%		integer_to_list(Line), $\s,
%%		wx_lib:term_to_string(Data),
%%%%		io_lib:write(Data),
%%		$\n
%%	].
%% 使用io_lib进行转义
format_2({log, MilliSecond, Pid, Mod, Line, Data}) ->
	{{Y, M, D}, {H, Minute, S}, MilliSec} = wx_time:millisecond_to_datetime(MilliSecond),
	io_lib:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0w ~w ~w:~4.. w ~0p~n",
		[Y, M, D, H, Minute, S, MilliSec, Pid, Mod, Line, Data]).
%%format_3({log, MilliSecond, Pid, Mod, Line, Data}) ->
%%	{{Y, M, D}, {H, Minute, S}, MilliSec} = wx_time:millisecond_to_datetime(MilliSecond),
%%	io_lib:format("~w-~w-~w ~w:~w:~w.~w ~w ~w:~w ~w~n", [Y, M, D, H, Minute, S, MilliSec, Pid, Mod, Line, Data]).
%%format_4({log, MilliSecond, Pid, Mod, Line, Data}) ->
%%	{{Y, M, D}, {H, Minute, S}, MilliSec} = wx_time:millisecond_to_datetime(MilliSecond),
%%	[
%%		wx_lib:integer_to_list(Y, 4), $-,
%%		wx_lib:integer_to_list(M, 2), $-,
%%		wx_lib:integer_to_list(D, 2), $\s,
%%		wx_lib:integer_to_list(H, 2), $:,
%%		wx_lib:integer_to_list(Minute, 2), $:,
%%		wx_lib:integer_to_list(S, 2), $.,
%%		wx_lib:integer_to_list(MilliSec, 3), $\s,
%%		pid_to_list(Pid), $\s,
%%		atom_to_list(Mod), $:,
%%		integer_to_list(Line), $\s,
%%		io_lib:write(Data),
%%		$\n
%%	].
%% -----------------------------------------------------------------
%% Func:
%% Description:创建文件时间名
%% Returns:
%% -----------------------------------------------------------------
make_time_name(Second, BaseName) ->
	Prefix = filename:rootname(BaseName),
	Suffix = filename:extension(BaseName),
	{{Year, Month, Day}, {Hour, Minute, _}} = wx_time:second_to_datetime(Second),
	[Prefix,
		wx_lib:integer_to_list(Year, 4), $-,
		wx_lib:integer_to_list(Month, 2), $-,
		wx_lib:integer_to_list(Day, 2), $_,
		wx_lib:integer_to_list(Hour, 2), $-,
		wx_lib:integer_to_list(Minute, 2),
		Suffix].

%% -----------------------------------------------------------------
%% Func:
%% Description: 开启文件io句柄
%% Returns:
%% -----------------------------------------------------------------
file_io_open(Dir, FileName) ->
	FileDir = filename:join(Dir, FileName),
	case file:open(FileDir, [delayed_write, append, raw]) of
		{ok, IoDevice} ->
			{ok, IoDevice};
		Error ->
			{error, Error}
	end.

%% -----------------------------------------------------------------
%% Func:
%% Description:同步到磁盘
%% Returns:
%% -----------------------------------------------------------------
sync_disc(Fd) ->
	file:datasync(Fd).

%==========================DEFINE=======================