%%% @private
%%%-------------------------------------------------------------------
%%% Created : 24 May 2018 by kruskakli@gmail.com
%%%
%%% This is a new version of the edbg tracer that logs trace
%%% messages to file. It is no longer using the dbg.erl module;
%%% instead it is using the trace BIFs directly.
%%%
%%% The way to set the config goes like this:
%%%
%%%   MF = new_mf(),
%%%   set_config([log_file_f("./my.log"),
%%%               max_msgs_f(500),
%%%               add_mf_f(fname(mname(MF, lists), reverse),
%%%               add_mf_f(mname(MF, mymod))
%%%              ], get_config()).
%%%
%%% Then start stop the tracing as:
%%%
%%%   start_trace()
%%%   stop_trace()
%%%
%%%-------------------------------------------------------------------
-module(edbg_file_tracer).

-behaviour(gen_server).

%% API
-export([add_mf_f/1
         , cfg_file_f/1
         , dump_output_eager_f/0
         , dump_output_lazy_f/0
         , fname/2
         , get_config/0
         , get_trace_spec/0
         , load_config/0
         , log_file_f/1
         , max_msgs_f/1
         , memory_f/0
         , mname/2
         , monotonic_ts_f/0
         , new_mf/0
         , send_receive_f/0
         , set_config/2
         , start/0
         , start_link/0
         , start_trace/0
         , stop/0
         , stop_trace/0
         , trace_spec_f/1
         , trace_time_f/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% debug export
-export([log/2]).


-include("edbg_trace.hrl").


-define(SERVER, ?MODULE).

-define(DOT_EDBG, ".edbg").

%%-define(log(Fmt,Args), log("~p: "++Fmt,[?MODULE|Args])).
-define(log(Fmt,Args), ok).

-define(cfg_file, "ftrace.edbg").

-define(DEFAULT_MAX_MSGS, 1000).
-define(DEFAULT_TRACE_TIME, 10). % seconds


-record(m, {
          mname = '_',
          fname = '_'
         }).

-record(state, {
          log_file = "./edbg.trace_result",
          cfg_file = "./ftrace.edbg",
          max_msgs = ?DEFAULT_MAX_MSGS,
          trace_time = ?DEFAULT_TRACE_TIME,
          trace_spec = all,
          modules = []  :: [#m{}],
          which_pid = all,  % all | first

          dump_output = false :: boolean(),

          tracer  :: pid(),
          srv_pid :: pid(),

          %% use the 'monotonoc_timestamp' trace option
          monotonic_ts = false,

          %% list of 'known' pids, i.e a list containing the
          %% Pids that has been seen in trace-call messages.
          known_pids = ordsets:new() :: [pid()],

          %% trace on send/receive msgs from 'known' pids
          %% (to avoid drowning in send/receive trace calls,
          %% we only save those trace messages that are sent or
          %% received from a Pid in a previously seen trace-call message)
          send_receive = false,

          %% trace memory via the process_info/2 BIF.
          memory = false

         }).

%%% --------------------------------------------------------------------
%%% A P I
%%%
%%% --------------------------------------------------------------------

%% @private
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% @private
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
stop() ->
    gen_server:stop(?SERVER).

%% @private
start_trace() ->
    call(start_trace).

%% @private
stop_trace() ->
    call(stop_trace).

%% @private
get_config() ->
    call(get_config).

%% @private
load_config() ->
    call(load_config).

%% @private
set_config(Funs, State)
  when is_list(Funs) andalso
       is_record(State, state) ->
    NewState = lists:foldl(fun(F,S) -> F(S) end, State, Funs),
    call({set_config, NewState}).

%% @private
get_trace_spec() ->
    call(get_trace_spec).

%% @private
log_file_f(LogFile)
  when is_list(LogFile) ->
    fun(State) -> State#state{log_file = LogFile} end.

%% @private
cfg_file_f(CfgFile)
  when is_list(CfgFile) ->
    fun(State) -> State#state{cfg_file = CfgFile} end.

%% @private
dump_output_lazy_f() ->
    fun(State) -> State#state{dump_output = false} end.

%% @private
dump_output_eager_f() ->
    fun(State) -> State#state{dump_output = true} end.

%% @private
monotonic_ts_f() ->
    fun(State) -> State#state{monotonic_ts = true} end.

%% @private
send_receive_f() ->
    fun(State) -> State#state{send_receive = true} end.

%% @private
memory_f() ->
    fun(State) -> State#state{memory = true} end.

%% @private
max_msgs_f(Max)
  when is_integer(Max) andalso Max >= 0 ->
    fun(State) -> State#state{max_msgs = Max} end.

%% @private
trace_time_f(Time)
  when is_integer(Time) andalso Time >= 0 ->
    fun(State) -> State#state{trace_time = Time} end.

%% @private
trace_spec_f(Spec)
  when is_atom(Spec) orelse is_pid(Spec) ->
    fun(State) -> State#state{trace_spec = Spec} end.

%% @private
add_mf_f(M)
  when is_record(M, m) ->
    fun(#state{modules = Ms} = State) -> State#state{modules = [M|Ms]} end.

%% @private
new_mf() -> #m{}.

%% @private
mname(M, Mname) when is_record(M, m) ->
    M#m{mname = l2a(Mname)}.

%% @private
fname(M, Fname)
  when is_record(M, m) ->
    M#m{fname = l2a(Fname)}.

call(Msg) ->
     gen_server:call(?SERVER, Msg, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
init([]) ->
    process_flag(trap_exit, true),
    {ok, setup_init_state()}.

-define(is_bool(B), ((B == true) orelse (B == false))).

setup_init_state() ->
    try
        Home = os:getenv("HOME"),
        Fname = filename:join([Home, ?DOT_EDBG]),
        L = consult_file(Fname),
        F = fun({log_file, LogFile}, S) when is_list(LogFile)  ->
                    S#state{log_file = LogFile};
               ({cfg_file, CfgFile}, S) when is_list(CfgFile)  ->
                    S#state{cfg_file = CfgFile};
               ({max_msgs, MaxMsgs}, S) when is_integer(MaxMsgs) ->
                    S#state{max_msgs = MaxMsgs};
               ({trace_time, TraceTime}, S) when is_integer(TraceTime) ->
                    S#state{trace_time = TraceTime};
               ({monotonic_ts, MonoTS}, S) when ?is_bool(MonoTS) ->
                    S#state{monotonic_ts = MonoTS};
               ({send_receive, SendRec}, S) when ?is_bool(SendRec) ->
                    S#state{send_receive = SendRec};
               ({memory, Memory}, S) when ?is_bool(Memory) ->
                    S#state{memory = Memory};
               (Opt, S) ->
                    ?err_msg("~nIgnoring unknown default option: ~p~n",[Opt]),
                    S
            end,
        lists:foldl(F, #state{}, L)
    catch
        undefined ->
            #state{};
        _:Err ->
            ?err_msg("~nError reading: ~p, ignoring it: ~p~n",[?DOT_EDBG,Err]),
            #state{}
    end.

consult_file(Fname) ->
    case file:consult(Fname) of
        {ok,L} -> L;
        _      -> throw(undefined)
    end.

%%--------------------------------------------------------------------
%% handle_call(ping, _From, State) ->
%%     Reply = pong,
%%     {reply, Reply, State};
%% @private
handle_call(get_config, _From, State) ->
    Reply = State,
    {reply, Reply, State};

handle_call({set_config, State}, _From, _State) ->
    save_config(State, ?cfg_file),
    Reply = ok,
    {reply, Reply, State};

handle_call(load_config, _From, _State) ->
    State = get_file_config(?cfg_file),
    Reply = ok,
    {reply, Reply, State};

handle_call(get_trace_spec, _From, State) ->
    Reply = State#state.trace_spec,
    {reply, Reply, State};

handle_call(start_trace, _From, #state{tracer = Pid, trace_time = Time} = State)
  when not(is_pid(Pid)) ->
    Tracer = start_tracer(State),
    ?log("Starting Tracer(~p)...~n",[Tracer]),
    timer:apply_after(timer:seconds(Time),gen_server,cast,[?SERVER,stop_dbg]),
    ?log("Tracer started: ~p", [Tracer]),
    {reply, ok, State#state{tracer = Tracer}};

handle_call(stop_trace, _From, #state{tracer = Tracer} = State)
  when is_pid(Tracer) ->
    ?log("Stopping Tracer(~p)...~n",[Tracer]),
    Ref = erlang:trace_delivered(all),
    receive
        {trace_delivered,all,Ref} -> ok
    end,
    Tracer ! {self(),stop},
    receive
        {Tracer, stopped}   -> ok;
        {'EXIT', Tracer, _} -> ok
    end,
    ?log("Server(~p): stopping Tracer(~p)...DONE~n",[self(), Tracer]),
    {reply, ok, State#state{tracer = undefined}};

handle_call(_Req, _From, State) ->
    Reply = error,
    {reply, Reply, State}.


%%--------------------------------------------------------------------

%% @private
handle_cast(_Msg, State) ->
    ?log("Got unexpected cast: ~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
handle_info({'EXIT', Tracer, _Reason}, #state{tracer = Tracer} = State) ->
    ?log("Tracer exited, reason: ~p", [_Reason]),
    {noreply, State#state{tracer = undefined}};

handle_info(_Info, State) ->
    ?log("Got unexpected info: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
terminate(_Reason, _State) ->
    ?log("Server stopped - ~p", [_Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

save_config(X, CfgFile) ->
    {ok,Fd} = file:open(CfgFile, [write]),
    try
        io:format(Fd, "~p.~n", [X])
    after
        file:close(Fd)
    end.

get_file_config(CfgFile) ->
    try
        {ok,[X]} = file:consult(CfgFile),
        X
    catch
        _:_ ->
            #state{}
    end.


start_tracer(State0) ->
    State = State0#state{srv_pid = self()},
    spawn_opt(fun() -> run_tracer(State) end, [link,{priority,max}]).


run_tracer(#state{modules = Modules, trace_spec = TraceSpec} = State) ->

    if length(Modules) > 0 ->
            %% Setup which Modules we want to do call-trace on.
            [code:ensure_loaded(M#m.mname) || M <- Modules],
            [erlang:trace_pattern({M,F,'_'},[{'_',[],[{return_trace}]}],[local])
             || #m{mname=M, fname=F} <- Modules];
       true ->
            erlang:trace_pattern({'_','_','_'},
                                 [{'_',[],[{return_trace}]}],
                                 [local])
    end,

    %% Start tracing!
    erlang:trace(TraceSpec,true,
                 [call,procs,{tracer,self()}] ++
                     monotonic_ts(State) ++
                     send_receive(State)),
    tloop(State, 0, []).

l2a(L) when is_list(L) ->
    erlang:list_to_atom(L);
l2a(A) when is_atom(A) ->
    A.




tloop(#state{srv_pid    = SrvPid,
             trace_spec = TraceSpec,
             known_pids = KnownPids0} = State,
      N,
      Tmsgs) ->
    {Suspended, Traces, MaybeStop, KnownPids} =
        recv_all_traces(State, SrvPid, KnownPids0),
    {NewN, NewTmsgs} = tmsgs(State, Traces, N, Tmsgs),
    resume(Suspended),

    case {NewN >= State#state.max_msgs, MaybeStop} of

        {true, _} ->
            ?log("Tracer(~p): max reached, stopping N=~p ...~n", [self(),NewN]),
            %% Max amount of trace msgs; stop tracing!
            erlang:trace(TraceSpec,false,
                         [call,procs,{tracer,self()}] ++
                             monotonic_ts(State) ++
                             send_receive(State)),
            dump_tmsgs(State#state{dump_output = true,
                                   known_pids  = KnownPids}, NewTmsgs),
            exit(max_msgs);

        {_, {From, stop}} ->
            ?log("Tracer(~p): stopping N=~p ...~n", [self(),NewN]),
            erlang:trace(TraceSpec,false,
                         [call,procs,{tracer,self()}] ++
                             monotonic_ts(State) ++
                             send_receive(State)),
            dump_tmsgs(State#state{dump_output = true,
                                   known_pids  = KnownPids}, Tmsgs),
            From ! {self(), stopped},
            exit(normal);

       _ ->
            dump_tmsgs(State, NewTmsgs),
            tloop(State#state{known_pids = KnownPids}, NewN, NewTmsgs)
    end.

monotonic_ts(#state{monotonic_ts = true}) -> [monotonic_timestamp];
monotonic_ts(_State)                      -> [].

send_receive(#state{send_receive = true}) -> [send,'receive'];
send_receive(_State)                      -> [].

dump_tmsgs(#state{dump_output = false}, _Tmsgs) ->
    ok;
dump_tmsgs(#state{log_file = Fname}, Tmsgs) ->
    %% We just overwrite any existing file in order to ensure
    %% we have the latest on disk in case we should crash.
    %% FIXME: Appending data would save memory that now is
    %%        held in this process by all the trace messages.
    ok = file:write_file(Fname,term_to_binary(Tmsgs)).


tmsgs(#state{max_msgs = Max, send_receive = SendReceive} = State,
      [Trace|Traces],
      N,            % Max allowed number of collected trace messages!
      Tmsgs)
  when N < Max andalso
       (?is_trace_call(Trace) orelse
        ?is_trace_return_from(Trace) orelse
        (SendReceive == true
         andalso
           (?is_trace_send(Trace) orelse
            ?is_trace_receive(Trace)))
       ) ->
    tmsgs(State, Traces, N+1, [{N,Trace}|Tmsgs]);
%%
%% Anything else should not be collected!
tmsgs(#state{max_msgs = Max} = State, [_|Traces], N, Tmsgs) when N < Max ->
    tmsgs(State, Traces, N, Tmsgs);
%%
%% Max amount of trace messages reached!
tmsgs(#state{max_msgs = Max}, _Traces, N, Tmsgs) when (N >= Max) ->
    {N, Tmsgs};
%%
tmsgs(_State, [], N, Tmsgs) ->
    {N, Tmsgs}.


%% @private
log(Format, Args) ->
    io:format(Format, Args).


%% ---------------------------------------------------------------------
%% CODE TAKEN FROM THE OTP dbg.erl MODULE!
%% (slightly modified...)
%%
%% So why are they doing it like this, I mean the suspend/resume thing...?
%% Probably some sort of throttling mechanism?
%%
recv_all_traces(State, SrvPid, KnownPids) ->
    recv_all_traces(State, SrvPid, KnownPids, [], [], infinity).

recv_all_traces(State, SrvPid, KnownPids, Suspended0, Traces, Timeout) ->
    receive
        Trace when is_tuple(Trace) andalso
                   ((?is_trace_msg(Trace) orelse
                     ?is_trace_ts_msg(Trace)) andalso
                    (?trace_pid(Trace) =/= SrvPid)) ->
            Suspended = suspend(Trace, Suspended0),
            case save_trace_p(Trace, KnownPids) of
                true ->
                    NewKnownPids = maybe_add_to_known_pids(Trace, KnownPids),
                    recv_all_traces(State,
                                    SrvPid,
                                    NewKnownPids,
                                    Suspended,
                                    [x(State,Trace)|Traces], 0);
                false ->
                    recv_all_traces(State,
                                    SrvPid,
                                    KnownPids,
                                    Suspended,
                                    Traces, 0)
            end;

        {_From, stop} = Msg ->
            {Suspended0, lists:reverse(Traces), Msg, KnownPids};

        _Other ->
            recv_all_traces(State, SrvPid, KnownPids, Suspended0, Traces, 0)

    after Timeout ->
            {Suspended0, lists:reverse(Traces), false, KnownPids}
    end.

%% Attach an attribute list to every trace message.
%% So, for example, change:
%%    {trace, Pid, call, MFA}
%% to:
%%    {trace, Pid, call, MFA, [{memory,Memory}]}
x(#state{memory = true}, Trace) when is_pid(element(2,Trace)) ->
    Memory = pinfo(?trace_pid(Trace), memory),
    list_to_tuple(tuple_to_list(Trace)++[[{memory,Memory}]]);
x(_, Trace) ->
    list_to_tuple(tuple_to_list(Trace)++[[]]).


maybe_add_to_known_pids(Trace, KnownPids) when element(3, Trace) == call ->
    ordsets:add_element(?trace_pid(Trace), KnownPids);
maybe_add_to_known_pids(_, KnownPids) ->
    KnownPids.

save_trace_p(Trace, KnownPids)
  when ?is_trace_send(Trace) orelse
       ?is_trace_receive(Trace) ->
    ordsets:is_element(?trace_pid(Trace), KnownPids);
save_trace_p(_, _) ->
    true.


suspend({trace,From,call,_Func}, Suspended) when node(From) == node() ->
    case (catch erlang:suspend_process(From, [unless_suspending,
                                              asynchronous])) of
        true ->
            [From | Suspended];
        _ ->
            Suspended
    end;
suspend(_Other, Suspended) -> Suspended.

resume([Pid|Pids]) when node(Pid) == node() ->
    (catch erlang:resume_process(Pid)),
    resume(Pids);
resume([]) -> ok.

pinfo(Pid, Item) ->
    case process_info(Pid, Item) of
        {_Item, X} ->
            X;
        undefined ->
            '-'
    end.
