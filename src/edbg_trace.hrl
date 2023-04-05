-ifndef(_EDBG_TRACE_HRL_).
-define(_EDBG_TRACE_HRL_, true).

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
