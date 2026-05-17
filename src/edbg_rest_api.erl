%%%-------------------------------------------------------------------
%%% @doc edbg REST API server.
%%%
%%% A lightweight HTTP/JSON API exposing edbg tracing operations.
%%% Uses OTP's inets httpd with mod_esi for dynamic request handling.
%%% Binds to 127.0.0.1 only (no authentication needed).
%%%
%%% Start with: edbg:start_api() or edbg:start_api(Port)
%%% Default port: 4242
%%% @end
%%%-------------------------------------------------------------------
-module(edbg_rest_api).

-export([start/0, start/1, stop/0]).
-export([handle/3]).

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
    DocRoot = "/tmp/edbg_rest_api",
    ok = filelib:ensure_dir(DocRoot ++ "/dummy"),
    {ok, _Pid} = inets:start(httpd, [
        {port, Port},
        {server_name, "edbg_rest_api"},
        {server_root, "/tmp"},
        {document_root, DocRoot},
        {bind_address, {127,0,0,1}},
        {modules, [mod_alias, mod_esi]},
        {erl_script_alias, {"/api", [edbg_rest_api]}}
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
%% mod_esi callback — dispatches to handler functions.
%% URL format: /api/edbg_rest_api/<function>?params
%%--------------------------------------------------------------------
handle(SessionID, Env, Input) ->
    PathInfo0 = proplists:get_value(path_info, Env, ""),
    %% path_info may include query string — split it off
    {PathRaw, QueryFromPath} = case string:split(PathInfo0, "?") of
                                   [P, Q] -> {P, Q};
                                   [P] -> {P, ""}
                               end,
    %% Ensure path starts with /
    PathInfo = case PathRaw of
                   "/" ++ _ -> PathRaw;
                   "" -> "/";
                   _ -> "/" ++ PathRaw
               end,
    QueryStr0 = proplists:get_value(query_string, Env, ""),
    QueryStr = case {QueryStr0, QueryFromPath} of
                   {"", Q2} -> Q2;
                   {Q1, _} -> Q1
               end,
    Method = proplists:get_value(request_method, Env, "GET"),
    Query = parse_query(QueryStr),
    try
        dispatch(SessionID, Method, {PathInfo, Query}, Input)
    catch
        _:Error:Stack ->
            ErrorMsg = io_lib:format("~p", [Error]),
            StackMsg = io_lib:format("~p", [Stack]),
            send_json(SessionID, 500,
                      #{error => list_to_binary(lists:flatten(ErrorMsg)),
                        stack => list_to_binary(lists:flatten(StackMsg))})
    end.

%%--------------------------------------------------------------------
%% Internal: routing
%%--------------------------------------------------------------------

parse_query([]) -> #{};
parse_query(QueryStr) ->
    Pairs = string:split(QueryStr, "&", all),
    maps:from_list(
      lists:filtermap(
        fun(Pair) ->
                case string:split(Pair, "=") of
                    [Key, Val] ->
                        {true, {list_to_binary(Key), list_to_binary(Val)}};
                    _ -> false
                end
        end, Pairs)).

%%--------------------------------------------------------------------
%% Route: /trace/start
%%--------------------------------------------------------------------
dispatch(SessionID, _Method, {"/trace/start", Query}, Input) ->
    Body = get_body(Query, Input),
    Modules = maps:get(<<"modules">>, Body, []),
    TraceTime = maps:get(<<"trace_time">>, Body, 10),
    MaxMsgs = maps:get(<<"max_msgs">>, Body, 1000),
    MonotonicTs = maps:get(<<"monotonic_ts">>, Body, false),
    Memory = maps:get(<<"memory">>, Body, false),
    SendReceive = maps:get(<<"send_receive">>, Body, false),

    ModAtoms = [binary_to_atom(M, utf8) || M <- Modules],

    %% Build options list
    %% Disable cfg_file to prevent loading previous config from disk
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

    send_json(SessionID, 200,
              #{status => <<"tracing_started">>,
                modules => Modules,
                trace_time => TraceTime,
                max_msgs => MaxMsgs});

%%--------------------------------------------------------------------
%% Route: /trace/stop
%%--------------------------------------------------------------------
dispatch(SessionID, _Method, {"/trace/stop", _Query}, _Input) ->
    edbg:fstop(),
    send_json(SessionID, 200, #{status => <<"tracing_stopped">>});

%%--------------------------------------------------------------------
%% Route: /trace/status
%%--------------------------------------------------------------------
dispatch(SessionID, _Method, {"/trace/status", _Query}, _Input) ->
    %% Check if the tracer process is alive
    Running = case whereis(mytracer) of
                  undefined -> false;
                  Pid when is_pid(Pid) -> is_process_alive(Pid)
              end,
    send_json(SessionID, 200, #{running => Running});

%%--------------------------------------------------------------------
%% Route: /trace/result
%%--------------------------------------------------------------------
dispatch(SessionID, _Method, {"/trace/result", Query}, _Input) ->
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
            send_json(SessionID, 200,
                      #{total => Total,
                        offset => Offset,
                        limit => Limit,
                        entries => Formatted});
        {error, enoent} ->
            send_json(SessionID, 200,
                      #{total => 0, offset => Offset, limit => Limit,
                        entries => [],
                        message => <<"No trace result file found. Run a trace first.">>});
        {error, Reason} ->
            send_json(SessionID, 500,
                      #{error => list_to_binary(
                                   io_lib:format("~p", [Reason]))})
    end;

%%--------------------------------------------------------------------
%% Route: /trace/summary
%%--------------------------------------------------------------------
dispatch(SessionID, _Method, {"/trace/summary", _Query}, _Input) ->
    LogFile = get_log_file(),
    case file:read_file(LogFile) of
        {ok, Bin} ->
            AllEntries = binary_to_term(Bin),
            Summary = build_summary(AllEntries),
            send_json(SessionID, 200, Summary);
        {error, enoent} ->
            send_json(SessionID, 200,
                      #{total_entries => 0,
                        message => <<"No trace result file found.">>});
        {error, Reason} ->
            send_json(SessionID, 500,
                      #{error => list_to_binary(
                                   io_lib:format("~p", [Reason]))})
    end;

%%--------------------------------------------------------------------
%% Route: GET /trace/config
%%--------------------------------------------------------------------
dispatch(SessionID, _Method, {"/trace/config", Query}, Input) ->
    case maps:size(Query) > 0 orelse Input =/= [] of
        true ->
            %% Has body/params: update config
            Body = get_body(Query, Input),
            try
                Config = edbg_file_tracer:get_config(),
                Funs = build_config_funs(Body),
                edbg_file_tracer:set_config(Funs, Config),
                NewConfig = edbg_file_tracer:get_config(),
                send_json(SessionID, 200, format_config(NewConfig))
            catch
                exit:{noproc, _} ->
                    send_json(SessionID, 500,
                              #{error => <<"Tracer process not running.">>})
            end;
        false ->
            %% No body: read config
            try
                Config = edbg_file_tracer:get_config(),
                send_json(SessionID, 200, format_config(Config))
            catch
                exit:{noproc, _} ->
                    send_json(SessionID, 200,
                              #{message => <<"Tracer not started. No config available.">>})
            end
    end;

%%--------------------------------------------------------------------
%% Fallback: unknown route
%%--------------------------------------------------------------------
dispatch(SessionID, Method, {Path, _Query}, _Input) ->
    send_json(SessionID, 404,
              #{error => <<"Not found">>,
                method => list_to_binary(Method),
                path => list_to_binary(Path)}).

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

ensure_inets() ->
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

%% Get body from either the 'body' query parameter (URL-encoded JSON)
%% or from the request input (POST body)
get_body(Query, Input) ->
    case maps:get(<<"body">>, Query, undefined) of
        undefined ->
            parse_json_input(Input);
        BodyBin ->
            %% URL-decode and parse
            Decoded = uri_string:unquote(binary_to_list(BodyBin)),
            parse_json_input(Decoded)
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
%% JSON encoding (minimal, no external deps)
%%--------------------------------------------------------------------

send_json(SessionID, Code, Data) ->
    Json = encode_json(Data),
    StatusLine = case Code of
                     200 -> "200 OK";
                     404 -> "404 Not Found";
                     500 -> "500 Internal Server Error";
                     _ -> integer_to_list(Code)
                 end,
    mod_esi:deliver(SessionID,
                    io_lib:format(
                      "Status: ~s\r\n"
                      "Content-Type: application/json\r\n\r\n"
                      "~s", [StatusLine, Json])).

encode_json(Map) when is_map(Map) ->
    Pairs = maps:fold(
              fun(K, V, Acc) ->
                      Key = encode_json_key(K),
                      Val = encode_json(V),
                      [io_lib:format("~s:~s", [Key, Val]) | Acc]
              end, [], Map),
    ["{", lists:join(",", Pairs), "}"];
encode_json(List) when is_list(List) ->
    Items = [encode_json(I) || I <- List],
    ["[", lists:join(",", Items), "]"];
encode_json(Bin) when is_binary(Bin) ->
    ["\"", json_escape(Bin), "\""];
encode_json(Atom) when is_atom(Atom) ->
    case Atom of
        true -> "true";
        false -> "false";
        null -> "null";
        _ -> ["\"", atom_to_list(Atom), "\""]
    end;
encode_json(Int) when is_integer(Int) ->
    integer_to_list(Int);
encode_json(Float) when is_float(Float) ->
    io_lib:format("~g", [Float]);
encode_json(Pid) when is_pid(Pid) ->
    ["\"", pid_to_list(Pid), "\""];
encode_json(Other) ->
    ["\"", json_escape(list_to_binary(
                         io_lib:format("~p", [Other]))), "\""].

encode_json_key(K) when is_atom(K) ->
    ["\"", atom_to_list(K), "\""];
encode_json_key(K) when is_binary(K) ->
    ["\"", json_escape(K), "\""];
encode_json_key(K) ->
    ["\"", io_lib:format("~p", [K]), "\""].

json_escape(Bin) when is_binary(Bin) ->
    json_escape(binary_to_list(Bin));
json_escape([]) -> [];
json_escape([$" | T]) -> [$\\, $" | json_escape(T)];
json_escape([$\\ | T]) -> [$\\, $\\ | json_escape(T)];
json_escape([$\n | T]) -> [$\\, $n | json_escape(T)];
json_escape([$\r | T]) -> [$\\, $r | json_escape(T)];
json_escape([$\t | T]) -> [$\\, $t | json_escape(T)];
json_escape([C | T]) when C < 32 ->
    io_lib:format("\\u~4.16.0B", [C]) ++ json_escape(T);
json_escape([C | T]) -> [C | json_escape(T)].

%%--------------------------------------------------------------------
%% JSON input parsing (minimal)
%%--------------------------------------------------------------------

parse_json_input([]) -> #{};
parse_json_input(Input) when is_list(Input) ->
    try
        %% Input from mod_esi is the request body as a string
        Bin = list_to_binary(Input),
        decode_json(Bin)
    catch
        _:_ -> #{}
    end.

%% Minimal JSON decoder — handles objects, arrays, strings, numbers, booleans
decode_json(Bin) ->
    {Value, _Rest} = decode_value(skip_ws(Bin)),
    Value.

decode_value(<<${, Rest/binary>>) -> decode_object(skip_ws(Rest), #{});
decode_value(<<$[, Rest/binary>>) -> decode_array(skip_ws(Rest), []);
decode_value(<<$", Rest/binary>>) -> decode_string(Rest, []);
decode_value(<<"true", Rest/binary>>) -> {true, Rest};
decode_value(<<"false", Rest/binary>>) -> {false, Rest};
decode_value(<<"null", Rest/binary>>) -> {null, Rest};
decode_value(<<C, _/binary>> = Bin) when C >= $0, C =< $9; C == $- ->
    decode_number(Bin, []).

decode_object(<<$}, Rest/binary>>, Acc) -> {Acc, Rest};
decode_object(<<$", Rest0/binary>>, Acc) ->
    {Key, Rest1} = decode_string(Rest0, []),
    <<$:, Rest2/binary>> = skip_ws(Rest1),
    {Value, Rest3} = decode_value(skip_ws(Rest2)),
    Rest4 = skip_ws(Rest3),
    case Rest4 of
        <<$,, Rest5/binary>> -> decode_object(skip_ws(Rest5), Acc#{Key => Value});
        <<$}, Rest5/binary>> -> {Acc#{Key => Value}, Rest5}
    end.

decode_array(<<$], Rest/binary>>, Acc) -> {lists:reverse(Acc), Rest};
decode_array(Bin, Acc) ->
    {Value, Rest0} = decode_value(Bin),
    Rest1 = skip_ws(Rest0),
    case Rest1 of
        <<$,, Rest2/binary>> -> decode_array(skip_ws(Rest2), [Value | Acc]);
        <<$], Rest2/binary>> -> {lists:reverse([Value | Acc]), Rest2}
    end.

decode_string(<<$", Rest/binary>>, Acc) ->
    {list_to_binary(lists:reverse(Acc)), Rest};
decode_string(<<$\\, $", Rest/binary>>, Acc) ->
    decode_string(Rest, [$" | Acc]);
decode_string(<<$\\, $\\, Rest/binary>>, Acc) ->
    decode_string(Rest, [$\\ | Acc]);
decode_string(<<$\\, $n, Rest/binary>>, Acc) ->
    decode_string(Rest, [$\n | Acc]);
decode_string(<<$\\, $r, Rest/binary>>, Acc) ->
    decode_string(Rest, [$\r | Acc]);
decode_string(<<$\\, $t, Rest/binary>>, Acc) ->
    decode_string(Rest, [$\t | Acc]);
decode_string(<<$\\, $/, Rest/binary>>, Acc) ->
    decode_string(Rest, [$/ | Acc]);
decode_string(<<C, Rest/binary>>, Acc) ->
    decode_string(Rest, [C | Acc]).

decode_number(<<C, Rest/binary>>, Acc)
  when C >= $0, C =< $9; C == $-; C == $+; C == $.; C == $e; C == $E ->
    decode_number(Rest, [C | Acc]);
decode_number(Rest, Acc) ->
    Str = lists:reverse(Acc),
    Num = case lists:member($., Str) orelse lists:member($e, Str)
               orelse lists:member($E, Str) of
              true -> list_to_float(Str);
              false -> list_to_integer(Str)
          end,
    {Num, Rest}.

skip_ws(<<C, Rest/binary>>) when C == $\s; C == $\t; C == $\n; C == $\r ->
    skip_ws(Rest);
skip_ws(Bin) -> Bin.

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
