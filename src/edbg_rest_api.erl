%%%-------------------------------------------------------------------
%%% @doc edbg REST API server.
%%%
%%% A lightweight HTTP/JSON API exposing edbg tracing operations.
%%% Uses OTP's inets httpd with httpd_router for declarative routing.
%%% Binds to 127.0.0.1 only (no authentication needed).
%%%
%%% Start with: edbg:start_api() or edbg:start_api(Port)
%%% Default port: 4242
%%% @end
%%%-------------------------------------------------------------------
-module(edbg_rest_api).

-export([start/0, start/1, stop/0]).
-export([handle_trace_start/1,
         handle_trace_stop/1,
         handle_trace_status/1,
         handle_trace_result/1,
         handle_trace_summary/1,
         handle_trace_config_get/1,
         handle_trace_config_set/1]).

-define(DEFAULT_PORT, 4242).

%%--------------------------------------------------------------------
%% @doc Start the REST API on the default port (4242).
%% @end
%%--------------------------------------------------------------------
start() ->
    start(?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc Start the REST API on the given port.
%% @end
%%--------------------------------------------------------------------
start(Port) when is_integer(Port), Port > 0 ->
    ok = ensure_inets(),
    {ok, _} = application:ensure_all_started(httpd_router),

    %% Create a per-port route table
    TableName = httpd_router:mk_table_name({127,0,0,1}, Port),
    {ok, _} = httpd_router:start(TableName),

    %% Register routes
    httpd_router:table_add_route(
      TableName, "POST", "/api/trace/start",
      fun edbg_rest_api:handle_trace_start/1, []),
    httpd_router:table_add_route(
      TableName, "POST", "/api/trace/stop",
      fun edbg_rest_api:handle_trace_stop/1, []),
    httpd_router:table_add_route(
      TableName, "GET", "/api/trace/status",
      fun edbg_rest_api:handle_trace_status/1, []),
    httpd_router:table_add_route(
      TableName, "GET", "/api/trace/result",
      fun edbg_rest_api:handle_trace_result/1, []),
    httpd_router:table_add_route(
      TableName, "GET", "/api/trace/summary",
      fun edbg_rest_api:handle_trace_summary/1, []),
    httpd_router:table_add_route(
      TableName, "GET", "/api/trace/config",
      fun edbg_rest_api:handle_trace_config_get/1, []),
    httpd_router:table_add_route(
      TableName, "POST", "/api/trace/config",
      fun edbg_rest_api:handle_trace_config_set/1, []),

    %% Start httpd with httpd_router
    DocRoot = "/tmp/edbg_rest_api",
    ok = filelib:ensure_dir(DocRoot ++ "/dummy"),
    {ok, _Pid} = inets:start(httpd, [
        {port, Port},
        {server_name, "edbg_rest_api"},
        {server_root, "/tmp"},
        {document_root, DocRoot},
        {bind_address, {127,0,0,1}},
        {modules, [httpd_router]},
        {httpd_router_table, TableName}
    ]),
    io:format("edbg REST API started on http://127.0.0.1:~p/api/...~n", [Port]),
    ok.

%%--------------------------------------------------------------------
%% @doc Stop the REST API server.
%% @end
%%--------------------------------------------------------------------
stop() ->
    case inets:services_info() of
        [] -> ok;
        Services ->
            lists:foreach(
              fun({httpd, Pid, Props}) ->
                      case proplists:get_value(port, Props) of
                          undefined -> ok;
                          _ -> inets:stop(httpd, Pid)
                      end;
                 (_) -> ok
              end, Services)
    end,
    ok.

%%--------------------------------------------------------------------
%% Route handlers
%%--------------------------------------------------------------------

handle_trace_start(#{body := Body}) ->
    Decoded = decode_body(Body),
    Modules = maps:get(<<"modules">>, Decoded, []),
    TraceTime = maps:get(<<"trace_time">>, Decoded, 10),
    MaxMsgs = maps:get(<<"max_msgs">>, Decoded, 1000),
    MonotonicTs = maps:get(<<"monotonic_ts">>, Decoded, false),
    Memory = maps:get(<<"memory">>, Decoded, false),
    SendReceive = maps:get(<<"send_receive">>, Decoded, false),

    ModAtoms = [binary_to_atom(M, utf8) || M <- Modules],

    %% Build options list
    Opts0 = [{max_msgs, MaxMsgs},
             {trace_time, TraceTime},
             {cfg_file, false}],
    Opts1 = case MonotonicTs of true -> [monotonic_ts | Opts0]; _ -> Opts0 end,
    Opts2 = case Memory of true -> [memory | Opts1]; _ -> Opts1 end,
    Opts3 = case SendReceive of true -> [send_receive | Opts2]; _ -> Opts2 end,

    %% Stop any ongoing trace before starting a new one
    catch edbg:fstop(),

    %% Start tracing via edbg
    _Result = edbg:fstart(ModAtoms, Opts3),

    {json, 200, #{status => <<"tracing_started">>,
                  modules => Modules,
                  trace_time => TraceTime,
                  max_msgs => MaxMsgs}}.

handle_trace_stop(_Ctx) ->
    edbg:fstop(),
    {json, 200, #{status => <<"tracing_stopped">>}}.

handle_trace_status(_Ctx) ->
    Running = case whereis(mytracer) of
                  undefined -> false;
                  Pid when is_pid(Pid) -> is_process_alive(Pid)
              end,
    {json, 200, #{running => Running}}.

handle_trace_result(#{query := Query}) ->
    Offset = binary_to_integer(maps:get(<<"offset">>, Query, <<"0">>)),
    Limit = binary_to_integer(maps:get(<<"limit">>, Query, <<"50">>)),
    Format = maps:get(<<"format">>, Query, <<"text">>),

    LogFile = get_log_file(),
    case file:read_file(LogFile) of
        {ok, Bin} ->
            AllEntries = lists:reverse(binary_to_term(Bin)),
            Total = length(AllEntries),
            Slice = lists:sublist(AllEntries, Offset + 1, Limit),
            Formatted = [format_entry(E, Format) || E <- Slice],
            {json, 200, #{total => Total,
                          offset => Offset,
                          limit => Limit,
                          entries => Formatted}};
        {error, enoent} ->
            {json, 200, #{total => 0, offset => Offset, limit => Limit,
                          entries => [],
                          message => <<"No trace result file found. Run a trace first.">>}};
        {error, Reason} ->
            {json, 500, #{error => list_to_binary(
                                     io_lib:format("~p", [Reason]))}}
    end.

handle_trace_summary(_Ctx) ->
    LogFile = get_log_file(),
    case file:read_file(LogFile) of
        {ok, Bin} ->
            AllEntries = binary_to_term(Bin),
            Summary = build_summary(AllEntries),
            {json, 200, Summary};
        {error, enoent} ->
            {json, 200, #{total_entries => 0,
                          message => <<"No trace result file found.">>}};
        {error, Reason} ->
            {json, 500, #{error => list_to_binary(
                                     io_lib:format("~p", [Reason]))}}
    end.

handle_trace_config_get(_Ctx) ->
    try
        Config = edbg_file_tracer:get_config(),
        {json, 200, format_config(Config)}
    catch
        exit:{noproc, _} ->
            {json, 200, #{message => <<"Tracer not started. No config available.">>}}
    end.

handle_trace_config_set(#{body := Body}) ->
    Decoded = decode_body(Body),
    try
        Config = edbg_file_tracer:get_config(),
        Funs = build_config_funs(Decoded),
        edbg_file_tracer:set_config(Funs, Config),
        NewConfig = edbg_file_tracer:get_config(),
        {json, 200, format_config(NewConfig)}
    catch
        exit:{noproc, _} ->
            {json, 500, #{error => <<"Tracer process not running.">>}}
    end.

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

ensure_inets() ->
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

decode_body(<<>>) -> #{};
decode_body(Body) when is_binary(Body) ->
    try json:decode(Body)
    catch _:_ -> #{}
    end.

get_log_file() ->
    try
        Config = edbg_file_tracer:get_config(),
        %% Config is a #state{} record; log_file is field 2
        element(2, Config)
    catch
        _:_ -> "./edbg.trace_result"
    end.

%%--------------------------------------------------------------------
%% Trace entry formatting
%%--------------------------------------------------------------------

format_entry({N, TraceMsg}, Format) ->
    Base = #{seq => N},
    format_trace_msg(TraceMsg, Base, Format).

format_trace_msg({trace, Pid, call, {M, F, Args}, _As}, Base, Format) ->
    Base#{type => <<"call">>,
          pid => list_to_binary(pid_to_list(Pid)),
          mfa => format_mfa(M, F, Args, Format),
          args => format_args(Args, Format)};
format_trace_msg({trace_ts, Pid, call, {M, F, Args}, Ts, _As}, Base, Format) ->
    Base#{type => <<"call">>,
          pid => list_to_binary(pid_to_list(Pid)),
          mfa => format_mfa(M, F, Args, Format),
          args => format_args(Args, Format),
          ts => Ts};
format_trace_msg({trace, Pid, return_from, {M, F, Arity}, Value, _As}, Base, Format) ->
    Base#{type => <<"return">>,
          pid => list_to_binary(pid_to_list(Pid)),
          mfa => format_mfa(M, F, Arity, Format),
          return_value => format_term(Value, Format)};
format_trace_msg({trace_ts, Pid, return_from, {M, F, Arity}, Value, Ts, _As}, Base, Format) ->
    Base#{type => <<"return">>,
          pid => list_to_binary(pid_to_list(Pid)),
          mfa => format_mfa(M, F, Arity, Format),
          return_value => format_term(Value, Format),
          ts => Ts};
format_trace_msg({trace, FromPid, send, Msg, ToPid, _As}, Base, Format) ->
    Base#{type => <<"send">>,
          pid => list_to_binary(pid_to_list(FromPid)),
          to_pid => list_to_binary(pid_to_list(ToPid)),
          message => format_term(Msg, Format)};
format_trace_msg({trace_ts, FromPid, send, Msg, ToPid, Ts, _As}, Base, Format) ->
    Base#{type => <<"send">>,
          pid => list_to_binary(pid_to_list(FromPid)),
          to_pid => list_to_binary(pid_to_list(ToPid)),
          message => format_term(Msg, Format),
          ts => Ts};
format_trace_msg({trace, ToPid, 'receive', Msg, _As}, Base, Format) ->
    Base#{type => <<"receive">>,
          pid => list_to_binary(pid_to_list(ToPid)),
          message => format_term(Msg, Format)};
format_trace_msg({trace_ts, ToPid, 'receive', Msg, Ts, _As}, Base, Format) ->
    Base#{type => <<"receive">>,
          pid => list_to_binary(pid_to_list(ToPid)),
          message => format_term(Msg, Format),
          ts => Ts};
format_trace_msg(Other, Base, _Format) ->
    Base#{type => <<"unknown">>,
          raw => list_to_binary(io_lib:format("~p", [Other]))}.

format_mfa(M, F, Args, <<"brief">>) when is_list(Args) ->
    list_to_binary(io_lib:format("~p:~p/~p", [M, F, length(Args)]));
format_mfa(M, F, Arity, <<"brief">>) when is_integer(Arity) ->
    list_to_binary(io_lib:format("~p:~p/~p", [M, F, Arity]));
format_mfa(M, F, Args, _) when is_list(Args) ->
    list_to_binary(io_lib:format("~p:~p/~p", [M, F, length(Args)]));
format_mfa(M, F, Arity, _) when is_integer(Arity) ->
    list_to_binary(io_lib:format("~p:~p/~p", [M, F, Arity])).

format_args(_Args, <<"brief">>) ->
    <<>>;
format_args(Args, _) when is_list(Args) ->
    list_to_binary(io_lib:format("~p", [Args]));
format_args(_, _) ->
    <<>>.

format_term(_Term, <<"brief">>) ->
    <<"...">>;
format_term(Term, _) ->
    list_to_binary(io_lib:format("~p", [Term])).

%%--------------------------------------------------------------------
%% Trace summary builder
%%--------------------------------------------------------------------

build_summary(Entries) ->
    Total = length(Entries),
    {Modules, Functions, Pids, HasTs} =
        lists:foldl(fun({_N, Msg}, {MAcc, FAcc, PAcc, TsAcc}) ->
                            {M2, F2, P2, Ts2} = extract_summary_info(Msg),
                            {inc_counter(M2, MAcc),
                             inc_counter(F2, FAcc),
                             case P2 of
                                 undefined -> PAcc;
                                 _ -> sets:add_element(P2, PAcc)
                             end,
                             TsAcc orelse Ts2}
                    end,
                    {#{}, #{}, sets:new(), false},
                    Entries),
    %% Top functions by count
    FunList = maps:to_list(Functions),
    TopFuns = lists:sublist(
                lists:reverse(lists:keysort(2, FunList)), 20),
    #{total_entries => Total,
      modules => Modules,
      top_functions => [#{mfa => K, count => V} || {K, V} <- TopFuns],
      unique_pids => [list_to_binary(pid_to_list(P))
                      || P <- sets:to_list(Pids)],
      has_timestamps => HasTs}.

extract_summary_info({trace, Pid, call, {M, F, A}, _As}) ->
    {atom_to_binary(M, utf8),
     list_to_binary(io_lib:format("~p:~p/~p", [M, F, arity(A)])),
     Pid, false};
extract_summary_info({trace_ts, Pid, call, {M, F, A}, _Ts, _As}) ->
    {atom_to_binary(M, utf8),
     list_to_binary(io_lib:format("~p:~p/~p", [M, F, arity(A)])),
     Pid, true};
extract_summary_info({trace, Pid, return_from, {M, F, Arity}, _Val, _As}) ->
    {atom_to_binary(M, utf8),
     list_to_binary(io_lib:format("~p:~p/~p", [M, F, Arity])),
     Pid, false};
extract_summary_info({trace_ts, Pid, return_from, {M, F, Arity}, _Val, _Ts, _As}) ->
    {atom_to_binary(M, utf8),
     list_to_binary(io_lib:format("~p:~p/~p", [M, F, Arity])),
     Pid, true};
extract_summary_info({trace, Pid, _, _, _As}) ->
    {undefined, undefined, Pid, false};
extract_summary_info({trace_ts, Pid, _, _, _Ts, _As}) ->
    {undefined, undefined, Pid, true};
extract_summary_info({trace, Pid, _, _, _, _As}) ->
    {undefined, undefined, Pid, false};
extract_summary_info({trace_ts, Pid, _, _, _, _Ts, _As}) ->
    {undefined, undefined, Pid, true};
extract_summary_info(_) ->
    {undefined, undefined, undefined, false}.

inc_counter(undefined, Acc) -> Acc;
inc_counter(Key, Acc) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Acc).

arity(A) when is_list(A) -> length(A);
arity(A) when is_integer(A) -> A.

%%--------------------------------------------------------------------
%% Config formatting
%%--------------------------------------------------------------------

format_config(Config) ->
    %% Config is #state{} record from edbg_file_tracer
    %% Fields: log_file, cfg_file, max_msgs, trace_time, trace_spec,
    %%         modules, which_pid, dump_output, tracer, srv_pid,
    %%         monotonic_ts, known_pids, send_receive, memory, set_on
    #{log_file => list_to_binary(element(2, Config)),
      cfg_file => format_cfg_file(element(3, Config)),
      max_msgs => element(4, Config),
      trace_time => element(5, Config),
      trace_spec => list_to_binary(io_lib:format("~p", [element(6, Config)])),
      modules => format_modules(element(7, Config)),
      monotonic_ts => element(12, Config),
      send_receive => element(14, Config),
      memory => element(15, Config)}.

format_cfg_file(false) -> <<"false">>;
format_cfg_file(F) when is_list(F) -> list_to_binary(F).

format_modules(Modules) ->
    [format_one_module(M) || M <- Modules].

format_one_module({m, Mname, '_'}) ->
    atom_to_binary(Mname, utf8);
format_one_module({m, Mname, Fname}) ->
    list_to_binary(io_lib:format("~p:~p", [Mname, Fname]));
format_one_module(Other) ->
    list_to_binary(io_lib:format("~p", [Other])).

%%--------------------------------------------------------------------
%% Config update helpers
%%--------------------------------------------------------------------

build_config_funs(Body) ->
    F1 = case maps:get(<<"max_msgs">>, Body, undefined) of
             undefined -> [];
             MaxMsgs -> [edbg_file_tracer:max_msgs_f(MaxMsgs)]
         end,
    F2 = case maps:get(<<"trace_time">>, Body, undefined) of
             undefined -> [];
             TraceTime -> [edbg_file_tracer:trace_time_f(TraceTime)]
         end,
    F3 = case maps:get(<<"log_file">>, Body, undefined) of
             undefined -> [];
             LogFile -> [edbg_file_tracer:log_file_f(binary_to_list(LogFile))]
         end,
    F4 = case maps:get(<<"monotonic_ts">>, Body, undefined) of
             true -> [edbg_file_tracer:monotonic_ts_f()];
             _ -> []
         end,
    F5 = case maps:get(<<"memory">>, Body, undefined) of
             true -> [edbg_file_tracer:memory_f()];
             _ -> []
         end,
    F6 = case maps:get(<<"send_receive">>, Body, undefined) of
             true -> [edbg_file_tracer:send_receive_f()];
             _ -> []
         end,
    F7 = case maps:get(<<"modules">>, Body, undefined) of
             undefined -> [];
             Mods ->
                 [edbg_file_tracer:add_mf_f(
                    edbg_file_tracer:mname(edbg_file_tracer:new_mf(),
                                           binary_to_atom(M, utf8)))
                  || M <- Mods]
         end,
    lists:flatten([F1, F2, F3, F4, F5, F6, F7]).
