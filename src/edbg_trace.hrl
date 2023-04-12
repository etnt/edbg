-ifndef(_EDBG_TRACE_HRL_).
-define(_EDBG_TRACE_HRL_, true).

-ifdef(USE_COLORS).
-define(info_msg(Fmt,Args), edbg_color_srv:info_msg(Fmt,Args)).
-define(att_msg(Fmt,Args), edbg_color_srv:att_msg(Fmt,Args)).
-define(warn_msg(Fmt,Args), edbg_color_srv:warn_msg(Fmt,Args)).
-define(err_msg(Fmt,Args), edbg_color_srv:err_msg(Fmt,Args)).
-define(cur_line_msg(Fmt,Args), edbg_color_srv:cur_line_msg(Fmt,Args)).
-define(c_hi(Str), edbg_color_srv:c_hi(Str)).
-define(c_warn(Str), edbg_color_srv:c_warn(Str)).
-define(c_err(Str), edbg_color_srv:c_err(Str)).
-define(help_hi(Str), edbg_color_srv:help_hi(Str)).
-define(edbg_color_srv_init(), edbg_color_srv:init()).
-else.
-define(info_msg(Fmt,Args), io:format(Fmt,Args)).
-define(att_msg(Fmt,Args), io:format(Fmt,Args)).
-define(warn_msg(Fmt,Args), io:format(Fmt,Args)).
-define(err_msg(Fmt,Args), io:format(Fmt,Args)).
-define(cur_line_msg(Fmt,Args), io:format(Fmt,Args)).
-define(c_hi(Str), Str).
-define(c_warn(Str), Str).
-define(c_err(Str), Str).
-define(help_hi(Str), Str).
-define(edbg_color_srv_init(), ok).
-endif.


%% Access to trace message items
-define(is_trace_msg(Trace), (element(1,Trace) == trace)).
-define(is_trace_ts_msg(Trace), (element(1,Trace) == trace_ts)).
-define(trace_pid(Trace), element(2,Trace)).
-define(trace_type(Trace), element(3,Trace)).
-define(is_trace_call(Trace), (?trace_type(Trace) == 'call')).
-define(is_trace_return_from(Trace), (?trace_type(Trace) == 'return_from')).
-define(is_trace_send(Trace), (?trace_type(Trace) == 'send')).
-define(is_trace_receive(Trace), (?trace_type(Trace) == 'receive')).

%% TRACE MESSAGES
-define(CALL(Pid,MFA,As),       {trace,    Pid, call, MFA, As}).
-define(CALL_TS(Pid,MFA,TS,As), {trace_ts, Pid, call, MFA, TS, As}).

-define(RETURN_FROM(Pid,MFA,Value,As),
        {trace, Pid, return_from, MFA, Value, As}).
-define(RETURN_FROM_TS(Pid,MFA,Value,TS,As),
        {trace_ts, Pid, return_from, MFA, Value, TS, As}).

-define(SEND(FromPid,Msg,ToPid,As),
        {trace, FromPid, send, Msg, ToPid, As}).
-define(SEND_TS(FromPid,Msg,ToPid,TS,As),
        {trace_ts, FromPid, send, Msg, ToPid, TS, As}).

-define(RECEIVE(FromPid,Msg,As),
        {trace, FromPid, 'receive', Msg, As}).
-define(RECEIVE_TS(FromPid,Msg,TS,As),
        {trace_ts, FromPid, 'receive', Msg, TS, As}).

-endif.
