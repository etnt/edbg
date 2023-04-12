-module(edbg_tracer).

-export([file/0
         , file/1
         , fhelp/0
         , fstart/0
         , fstart/1
         , fstart/2
         , fstop/0
         , get_traced_pid/0
         , lts/0
         , send/2
         , start_my_tracer/0
         , tlist/0
         , tmax/1
         , tquit/0
         , traw/1
         , tstart/0
         , tstart/1
         , tstart/2
         , tstart/3
         , tstop/0
        ]).

-import(edbg_file_tracer,
        [add_mf_f/1
         , dump_output_eager_f/0
         , dump_output_lazy_f/0
         , fname/2
         , get_config/0
         , get_trace_spec/0
         , log_file_f/1
         , max_msgs_f/1
         , memory_f/0
         , mname/2
         , monotonic_ts_f/0
         , new_mf/0
         , send_receive_f/0
         , set_config/2
         , start_trace/0
         , stop_trace/0
         , trace_spec_f/1
         , trace_time_f/1
        ]).

%% Internal export
-export([tloop/3,
         ploop/1,
         rloop/2
        ]).

-include("edbg_trace.hrl").




-define(mytracer, mytracer).

-define(inside(At,Cur,Page), ((Cur >=At) andalso (Cur =< (At+Page)))).
-define(inside_vl(At,Cur,VL,Page), ((Cur >=At) andalso (VL =< Page))).

-record(tlist,
        {
         level = maps:new(),  % Key=<pid> , Val=<level>
         at = 1,
         current = 1,
         page = 20,
         send_receive = true, % do (not) show send/receive msgs
         memory = true,       % do (not) show memory info
         bs = erl_eval:new_bindings(),

         %% -- num of visible lines --
         %% If display of send/receive msgs is turned off
         %% then this counter keeps track of how many lines
         %% we are currently displaying, making it possible
         %% to fill the 'page'.
         vlines = 0
        }).


-record(t, {
          trace_max = 10000,
          tracer
         }).

start_my_tracer() ->
    case whereis(?mytracer) of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            Pid = spawn(fun() -> tinit(#t{}) end),
            register(?mytracer, Pid),
            Pid
    end.

fhelp() ->
    S = "\n"
    "edbg:fstart(ModFunList, Opts)\n"
    "edbg:fstop()\n"
    "edbg:file()\n"
    "edbg:file(FileName)\n"
    "edbg:fpid(Pid)\n"
    "edbg:fpid(Pid, Opts)\n"
    "\n"
    "ModFunList is a list of module names (atoms) or tuples {ModName, FunName}.\n"
    "\n"
    "Opts is a list of option tuples:\n"
    "\n"
    "{log_file, FileName}     : file where to store trace output\n"
    "                           default is: edbg.trace_result\n"
    "{max_msgs, MaxNumOfMsgs} : max number of trace messages\n"
    "                           default = 1000\n"
    "{trace_time, Seconds}    : max time to trace\n"
    "                           default = 10 seconds\n"
    "{trace_spec, Spec}       : see the erlang:trace/3 docs\n"
    "                           default = all\n"
    "dump_output_eager        : trace output goes to file often\n"
    "dump_output_lazy         : trace output goes to file when done (default)\n"
    "monotonic_ts             : show the elapsed monotonic nano seconds\n"
    "send_receive             : trace send/receive messages from 'known' pids\n"
    "memory                   : track the memory usage of the 'known' pids\n",
    io:format("~s~n",[S]).



%% dbg:tracer(process,{fun(Trace,N) ->
%%                        io:format("TRACE (#~p): ~p~n",[N,Trace]),
%%                        N+1
%%                       end, 0}).
%%dbg:p(all,clear).
%%dbg:p(all,[c]).

%% @doc Enter trace list mode based on given trace file.
file() ->
    file("./edbg.trace_result").

file(Fname) ->
    catch stop_trace(),
    catch edbg_file_tracer:stop(),
    try file:read_file(Fname) of
        {ok, Tdata} ->
            %% We expect Tdata to be a list of trace tuples as
            %% a binary in the external term form.
            call(start_my_tracer(), {load_trace_data,
                                     binary_to_term(Tdata)}),
            tlist();
        Error ->
            Error
    catch
        _:Err ->
            {error, Err}
    end.

%% @doc Start tracing to file.
fstart() ->
    edbg_file_tracer:start(),
    edbg_file_tracer:load_config(),
    start_trace().

fstart(ModFunList) ->
    fstart(ModFunList, []).

fstart(ModFunList, Options)
  when is_list(ModFunList) andalso
       is_list(Options) ->
    edbg_file_tracer:start(),
    MF  = new_mf(),
    MFs = lists:foldr(fun({Mname,Fname}, Acc) ->
                              [add_mf_f(fname(mname(MF, Mname), Fname))|Acc];
                         (Mname, Acc) when is_atom(Mname) ->
                              [add_mf_f(mname(MF, Mname))|Acc];
                         (X, Acc) ->
                              io:format("Ignoring ModFun: ~p~n",[X]),
                              Acc
                      end, [], ModFunList),

    Opts = lists:foldr(fun({log_file, Lname}, Acc) ->
                               [log_file_f(Lname)|Acc];
                          ({max_msgs, Max}, Acc) ->
                               [max_msgs_f(Max)|Acc];
                          ({trace_time, Time}, Acc) ->
                               [trace_time_f(Time)|Acc];
                          ({trace_spec, Spec}, Acc) ->
                               [trace_spec_f(Spec)|Acc];
                          (dump_output_lazy, Acc) ->
                               [dump_output_lazy_f()|Acc];
                          (dump_output_eager, Acc) ->
                               [dump_output_eager_f()|Acc];
                          (monotonic_ts, Acc) ->
                               [monotonic_ts_f()|Acc];
                          (send_receive, Acc) ->
                               [send_receive_f()|Acc];
                          (memory, Acc) ->
                               [memory_f()|Acc];
                          (X, Acc) ->
                               io:format("Ignoring Option: ~p~n",[X]),
                               Acc
                       end, [], Options),

    set_config(MFs++Opts, get_config()),
    start_trace().

fstop() ->
    edbg_file_tracer:stop_trace(),
    edbg_file_tracer:stop().


get_traced_pid() ->
    case get_trace_spec() of
        Pid when is_pid(Pid) -> Pid;
        _                    -> undefined
    end.


tstart() ->
    start_my_tracer().

tstart(Mod) when is_atom(Mod) ->
    tstart(Mod, []).

tstart(Mod, Mods) when is_atom(Mod) andalso is_list(Mods)  ->
    tstart(Mod, Mods, []).

tstart(Mod, Mods, Opts) when is_atom(Mod) andalso is_list(Mods)  ->
    trace_start({Mod, Mods, Opts}, Opts).

trace_start(FilterInput, Opts) ->
    call(start_my_tracer(), {start, FilterInput, Opts}).

call(MyTracer, Msg) ->
    MyTracer ! {self(), Msg},
    receive
        {MyTracer, Result} ->
            Result
    end.

lts() ->
    {ok,[X]} = file:consult("trace.edbg"),
    call(start_my_tracer(), X).


tlist() ->
    Self = self(),
    Prompt = spawn_link(fun() -> prompt(Self) end),
    print_help(),
    ?mytracer ! at,
    ploop(Prompt).

ploop(Prompt) ->
    receive
        {'EXIT', Prompt, _} ->
            true;

        quit ->
            true;

        _ ->
            ?MODULE:ploop(Prompt)
    end.


prompt(Pid) when is_pid(Pid) ->
    Prompt = "tlist> ",
    rloop(Pid, Prompt).

rloop(Pid, Prompt) ->
    case string:tokens(io:get_line(Prompt), "\n") of
        ["eval"++X] -> xeval(?mytracer, X);
        ["xnall"++X] -> xnall(?mytracer, X);
        ["xall"++X] -> xall(?mytracer, X);
        ["let"++X] -> xlet(?mytracer, X);
        ["set"++X] -> xset(?mytracer, X);
        ["off"++X]-> off(?mytracer, X);
        ["on"++X]-> on(?mytracer, X);
        ["pr"++X]-> show_record(?mytracer, X);
        ["d"++_] -> ?mytracer ! down;
        ["u"++_] -> ?mytracer ! up;
        ["t"++_] -> ?mytracer ! top;
        ["b"++_] -> ?mytracer ! bottom;
        ["a"++X] -> at(?mytracer, X);
        ["f"++X] -> find(?mytracer, X);
        ["s"++X] -> show(?mytracer, X);
        ["r"++X] -> show_return(?mytracer, X);
        ["w"++X] -> show_raw(?mytracer, X);
        ["p"++X] -> set_page(?mytracer, X);
        ["h"++_] -> print_help();
        ["q"++_] ->
            ?mytracer ! quit,
            Pid ! quit,
            exit(normal);

        _X ->
            ?info_msg("prompt got: ~p~n",[_X])
    end,
    ?MODULE:rloop(Pid, Prompt).

find(Pid, X) ->
    try
        Xstr = string:strip(X),
        case string:tokens(Xstr, ":") of
            [M,F] ->
                case string:tokens(F, " ") of
                    [F1] ->
                        Pid ! {find, {M,F1}};
                    [F1,An,Av] ->
                        Pid ! {find, {M,F1},{list_to_integer(An),Av}}
                end;
            [M|_] ->
                case string:chr(Xstr, $:) of
                    I when I > 0 ->
                        Pid ! {find, {M,""}};
                    _ ->
                        Pid ! {find_str, Xstr}
                end
        end
    catch
        _:_ -> false
    end.

on(Pid, X) ->
    case string:strip(X) of
        "send_receive" ->
            Pid ! {on, send_receive};
        "memory" ->
            Pid ! {on, memory};
        _ ->
            false
    end.

off(Pid, X) ->
    case string:strip(X) of
        "send_receive" ->
            Pid ! {off, send_receive};
        "memory" ->
            Pid ! {off, memory};
        _ ->
            false
    end.

%% Unload/Remove the current a module and re-load it from the code path!
xnall(_Pid, X) ->
    try
        {ModStr, _} = get_first_token(string:strip(X, left)),
        Module = list_to_atom(ModStr),
        reload_module(Module),
        c:m(Module),
        true
    catch
        _:_ -> false
    end.

reload_module(Module) ->
    case code:delete(Module) of
        false ->
            code:purge(Module),
            code:delete(Module);
        true ->
            true
    end,
    {module, Module} = code:load_file(Module).




%% Re-Compile and load a module with the export-all compiler option!
xall(_Pid, X) ->
    try
        {ModStr, _} = get_first_token(string:strip(X, left)),
        Module = list_to_atom(ModStr),
        recompile_as_export_all(Module),
        c:m(Module),
        true
    catch
        _:_ -> false
    end.

recompile_as_export_all(Module) ->
    Cs = Module:module_info(compile),
    {source, SrcFname} = lists:keyfind(source, 1, Cs),
    File = filename:rootname(SrcFname, ".erl"),
    {ok, Module, Code} = compile:file(File, [export_all,binary]),
    code:load_binary(Module, File, Code).

xeval(Pid, X) ->
    try
        Pid ! {xeval, X}
    catch
        _:_ -> false
    end.

xlet(Pid, X) ->
    try
        {Var, ExprStr} = get_first_token(string:strip(X, left)),
        Pid ! {xlet, Var, ExprStr}
    catch
        _:_ -> false
    end.

get_first_token(Str) ->
    string:take(Str, " ", true, leading).

xset(Pid, X) ->
    try
        case string:tokens(string:strip(X), " ") of
            [Var,A] ->
                %% Bind Var to Return-Value
                Pid ! {xset, Var, list_to_integer(A)};

            [Var,A,B] ->
                %% Bind Var to Argument-Value
                Pid ! {xset, Var, list_to_integer(A), list_to_integer(B)};

            [Var,A,B,R] ->
                %% Bind Var to a record field in the Argument-Value
                %% Example: string:tokens("#arg.client_ip_port","#.").
                case string:tokens(string:strip(R), "#.") of
                    [Record,Field] ->
                        Pid ! {xset, Var,
                               list_to_integer(A),list_to_integer(B),
                               Record, Field};
                    _ ->
                        ?info_msg("~ncould not parse record field: ~p~n",[R]),
                        Pid ! {xset, Var,
                               list_to_integer(A), list_to_integer(B)}
                end
        end
    catch
        _:_ -> false
    end.

show(Pid, X) ->
    parse_integers(Pid, X, show).

show_record(Pid, X) ->
    parse_integers(Pid, X, show_record).

set_page(Pid, X) ->
    parse_integers(Pid, X, set_page).

at(Pid, X) ->
    parse_integers(Pid, X, at).

show_return(Pid, X) ->
    parse_integers(Pid, X, show_return).

show_raw(Pid, X) ->
    parse_integers(Pid, X, show_raw).

parse_integers(Pid, X, Msg) ->
    try
        case string:tokens(string:strip(X), " ") of
            [] ->
                Pid ! Msg;
            [A] ->
                Pid ! {Msg, list_to_integer(A)};
            [A,B] ->
                Pid ! {Msg, list_to_integer(A), list_to_integer(B)}
        end
    catch
        _:_ -> false
    end.


print_help() ->
    S1 = " (h)elp (a)t [<N>] (d)own (u)p (t)op (b)ottom",
    S2 = " (s)how <N> [<ArgN>] (r)etval <N> ra(w) <N>",
    S3 = " (pr)etty print record <N> <ArgN>",
    S4 = " (f)ind <M>:<Fx> [<ArgN> <ArgVal>] | <RetVal>",
    S5 = " (on)/(off) send_receive | memory",
    S6 = " (p)agesize <N> (q)uit",
    S7 = " (set) <Var> <N> [<ArgN>]  (let) <Var> <Expr>",
    S8 = " (eval) <Expr>  (xall/xnall) <Mod>",
    S = io_lib:format("~n~s~n~s~n~s~n~s~n~s~n~s~n~s~n~s~n",
                      [S1,S2,S3,S4,S5,S6,S7,S8]),
    ?info_msg(?help_hi(S), []).


tstop() -> ?mytracer ! stop.
traw(N) when is_integer(N)  -> ?mytracer ! {raw,N}.
tquit() -> ?mytracer ! quit.
tmax(N) when is_integer(N) -> ?mytracer ! {max,N}.

tinit(X) ->
    process_flag(trap_exit, true),
    ?MODULE:tloop(X, #tlist{}, []).


tloop(#t{trace_max = MaxTrace} = X, Tlist, Buf) ->
    receive

        %% FROM THE TRACE FILTER

        %% Trace everything until Max is reached.
        {trace, From, {N,_Trace} = Msg} when N =< MaxTrace ->
            reply(From, ok),
            ?MODULE:tloop(X, Tlist ,[Msg|Buf]);

        %% Max is reached; stop tracing!
        {trace, From , {N,_Trace} = _Msg} when N > MaxTrace ->
            reply(From, stop),
            dbg:stop_clear(),
            ?MODULE:tloop(X#t{tracer = undefined}, Tlist ,Buf);


        %% FROM EDBG

        {max, N} ->
            ?MODULE:tloop(X#t{trace_max = N}, Tlist ,Buf);

        {set_page, Page} ->
            ?MODULE:tloop(X, Tlist#tlist{page = Page} ,Buf);

        {show_raw, N} ->
            dbg:stop_clear(),
            case lists:keyfind(N, 1, Buf) of
                {_, Msg} ->
                    ?info_msg("~n~p~n", [Msg]);
                _ ->
                    ?err_msg("not found~n",[])
            end,
            ?MODULE:tloop(X, Tlist ,Buf);

        {show_return, N} ->
            dbg:stop_clear(),
            case get_return_value(N, lists:reverse(Buf)) of
                {ok, {M,F,Alen}, RetVal} ->
                    Sep = pad(35, $-),
                    ?info_msg("~nCall: ~p:~p/~p , return value:~n~s~n~p~n",
                             [M,F,Alen,Sep,RetVal]);
                not_found ->
                    ?info_msg("~nNo return value found!~n",[])
            end,
            ?MODULE:tloop(X, Tlist ,Buf);

        {show, N} ->
            dbg:stop_clear(),
            mlist(N, Buf),
            ?MODULE:tloop(X, Tlist ,Buf);

        {show, N, ArgN} ->
            dbg:stop_clear(),
            try
                case lists:keyfind(N, 1, Buf) of
                    {_, ?CALL(_Pid, MFA, _As)} ->
                        show_arg(ArgN, MFA);

                    {_, ?CALL_TS(_Pid, MFA, _TS, _As)} ->
                        show_arg(ArgN, MFA);

                    _ ->
                        ?err_msg("not found~n",[])
                end
            catch
                _:_ ->  ?err_msg("not found~n",[])
            end,
            ?MODULE:tloop(X, Tlist ,Buf);

        {show_record, N, ArgN} ->
            dbg:stop_clear(),
            try
                case lists:keyfind(N, 1, Buf) of
                    {_, ?CALL(_Pid, MFA, _As)} ->
                        show_rec(ArgN, MFA);

                    {_, ?CALL_TS(_Pid, MFA, _TS, _As)} ->
                        show_rec(ArgN, MFA);

                    _ ->
                        ?err_msg("not found~n",[])
                end
            catch
                _:_ ->
                    ?err_msg("not found~n",[])
            end,
            ?MODULE:tloop(X, Tlist ,Buf);

        %% Set Variable to the specified Return-Value
        {xset, Var, N} ->
            dbg:stop_clear(),
            NewTlist =
                try
                    case get_return_value(N, lists:reverse(Buf)) of
                        {ok, {_M,_F,_Alen}, RetVal} ->
                            add_binding(Tlist, Var, RetVal);

                        not_found ->
                            ?info_msg("~nNo return value found!~n",[]),
                            Tlist
                    end
                catch
                    _:_ ->
                        ?err_msg("unexpected error~n",[]),
                        Tlist
                end,
            ?MODULE:tloop(X, NewTlist ,Buf);

        %% Set Variable to the specified Argument-Value
        {xset, Var, N, ArgN} ->
            dbg:stop_clear(),
            NewTlist =
                try
                    case lists:keyfind(N, 1, Buf) of
                        {_, ?CALL(_Pid, MFA, _As)} ->
                            Val = get_arg(ArgN, MFA),
                            add_binding(Tlist, Var, Val);

                        {_, ?CALL_TS(_Pid, MFA, _TS, _As)} ->
                            Val = get_arg(ArgN, MFA),
                            add_binding(Tlist, Var, Val);

                        _ ->
                            ?err_msg("not found~n",[]),
                            Tlist
                    end
                catch
                    _:_ ->
                        ?err_msg("unexpected error~n",[]),
                        Tlist
                end,
            ?MODULE:tloop(X, NewTlist ,Buf);

        %% Set Variable to the specified record field of Argument-Value
        {xset, Var, N, ArgN, RecordStr, FieldStr} ->
            dbg:stop_clear(),
            Record = list_to_atom(RecordStr),
            Field = list_to_atom(FieldStr),
            NewTlist =
                try
                    case lists:keyfind(N, 1, Buf) of
                        {_, ?CALL(_Pid, MFA, _As)} ->
                            Val = get_arg(ArgN, MFA),
                            FieldIndex = record_field_index(MFA, Record, Field),
                            add_binding(Tlist, Var, element(FieldIndex,Val));

                        {_, ?CALL_TS(_Pid, MFA, _TS, _As)} ->
                            Val = get_arg(ArgN, MFA),
                            FieldIndex = record_field_index(MFA, Record, Field),
                            add_binding(Tlist, Var, element(FieldIndex,Val));

                        _ ->
                            ?err_msg("not found~n",[]),
                            Tlist
                    end
                catch
                    _:_ ->
                        ?err_msg("unexpected error~n",[]),
                        Tlist
                end,
            ?MODULE:tloop(X, NewTlist ,Buf);

        {xlet, Var, ExprStr} ->
            dbg:stop_clear(),
            NewTlist =
                try
                    {ok, Tokens, _} = erl_scan:string(ExprStr),
                    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
                    {value, Result, _} = erl_eval:exprs(Exprs, Tlist#tlist.bs),
                    ?info_msg("~p~n",[Result]),
                    add_binding(Tlist, Var, Result)
                catch
                    _:_ ->
                        ?err_msg("unexpected error~n",[]),
                        Tlist
                end,
            ?MODULE:tloop(X, NewTlist ,Buf);

        {xeval, ExprStr} ->
            dbg:stop_clear(),
            try
                {ok, Tokens, _} = erl_scan:string(ExprStr),
                {ok, Exprs} = erl_parse:parse_exprs(Tokens),
                case catch erl_eval:exprs(Exprs, Tlist#tlist.bs) of
                    {value, Result, _} ->
                        ?info_msg("~p~n",[Result]);
                    Else ->
                        ?err_msg("~n~p~n",[Else])
                end
                catch
                    _:Err ->
                        ?err_msg("parse/eval error: ~n~p~n",[Err])
                end,
            ?MODULE:tloop(X, Tlist ,Buf);


        %% Find a matching function call
        {find, {Mstr,Fstr}} ->
            NewTlist = case find_mf(Tlist#tlist.at, Buf, Mstr, Fstr) of
                           not_found ->
                               ?info_msg("not found~n",[]),
                               Tlist;
                           NewAt ->
                               list_trace(Tlist#tlist{at = NewAt}, Buf)
                       end,
            ?MODULE:tloop(X, NewTlist ,Buf);

        %% Find a matching function call where ArgN contains Value
        {find, {Mstr,Fstr},{An,Av}} ->
            NewTlist = case find_mf_av(Tlist#tlist.at,Buf,Mstr,Fstr,An,Av) of
                           not_found ->
                               ?info_msg("not found~n",[]),
                               Tlist;
                           NewAt ->
                               list_trace(Tlist#tlist{at = NewAt}, Buf)
                       end,
            ?MODULE:tloop(X, NewTlist ,Buf);

        %% Find a match among the return values
        {find_str, Str} ->
            NewTlist = case find_retval(Tlist#tlist.at, Buf, Str) of
                           not_found ->
                               ?info_msg("not found~n",[]),
                               Tlist;
                           NewAt ->
                               list_trace(Tlist#tlist{at = NewAt}, Buf)
                       end,
            ?MODULE:tloop(X, NewTlist ,Buf);

        top ->
            NewTlist = list_trace(Tlist#tlist{at = 0}, Buf),
            ?MODULE:tloop(X, NewTlist, Buf);

        bottom ->
            {N,_} = hd(Buf),
            NewTlist = list_trace(Tlist#tlist{at = N}, Buf),
            ?MODULE:tloop(X, NewTlist, Buf);

        {on, send_receive} ->
            ?info_msg("turning on display of send/receive messages~n",[]),
            ?MODULE:tloop(X, Tlist#tlist{send_receive = true}, Buf);

        {on, memory} ->
            ?info_msg("turning on display of memory usage~n",[]),
            ?MODULE:tloop(X, Tlist#tlist{memory = true}, Buf);

        {off, send_receive} ->
            ?info_msg("turning off display of send/receive messages~n",[]),
            ?MODULE:tloop(X, Tlist#tlist{send_receive = false}, Buf);

        {off, memory} ->
            ?info_msg("turning off display of memory usage~n",[]),
            ?MODULE:tloop(X, Tlist#tlist{memory = false}, Buf);

        at ->
            NewAt = erlang:max(0, Tlist#tlist.at - Tlist#tlist.page - 1),
            NewTlist = list_trace(Tlist#tlist{at = NewAt}, Buf),
            ?MODULE:tloop(X, NewTlist, Buf);

        {at, At} ->
            NewTlist = list_trace(Tlist#tlist{at = At}, Buf),
            ?MODULE:tloop(X, NewTlist, Buf);

        up ->
            NewAt = erlang:max(0, Tlist#tlist.at - (2*Tlist#tlist.page)),
            NewTlist = list_trace(Tlist#tlist{at = NewAt}, Buf),
            ?MODULE:tloop(X, NewTlist, Buf);

        down ->
            dbg:stop_clear(),
            NewTlist = list_trace(Tlist, Buf),
            ?MODULE:tloop(X, NewTlist, Buf);

        {raw, N} ->
            case lists:keyfind(N, 1, Buf) of
                {_,V} -> ?info_msg("~p~n",[V]);
                _     -> ?info_msg("nothing found!~n",[])
            end,
            ?MODULE:tloop(X, Tlist ,Buf);

        stop ->
            dbg:stop_clear(),
            ?MODULE:tloop(X#t{tracer = undefined}, Tlist ,Buf);

        quit ->
            dbg:stop_clear(),
            exit(quit);

        {From, {load_trace_data, TraceData}} ->
            From ! {self(), ok},
            ?MODULE:tloop(X, Tlist ,TraceData);

        {From, {start, Start, Opts}} ->
            TraceSetupMod = get_trace_setup_mod(Opts),
            TraceMax = get_trace_max(Opts),
            case TraceSetupMod:start_tracer(Start) of
                {ok, NewTracer} ->
                    From ! {self(), started},
                    save_start_trace({start, Start, Opts}),
                    ?MODULE:tloop(X#t{trace_max = TraceMax,
                                      tracer = NewTracer}, Tlist ,[]);
                {error, _} = Error ->
                    From ! {self(), Error},
                    ?MODULE:tloop(X, Tlist ,Buf)
            end;

        _X ->
            %%?info_msg("mytracer got: ~p~n",[_X]),
            ?MODULE:tloop(X, Tlist ,Buf)
    end.

add_binding(#tlist{bs = Bs} = T, Var, Val) ->
    T#tlist{bs = erl_eval:add_binding(list_to_atom(Var), Val, Bs)}.

get_arg(ArgN, {_M,_F,A}) ->
    lists:nth(ArgN, A).

show_arg(ArgN, {M,F,A}) ->
    Sep = pad(35, $-),
    ArgStr = "argument "++integer_to_list(ArgN)++":",
    ?info_msg("~nCall: ~p:~p/~p , ~s~n~s~n~p~n",
              [M,F,length(A),ArgStr,Sep,lists:nth(ArgN,A)]).

show_rec(ArgN, {M,F,A}) ->
    Sep = pad(35, $-),
    Fname = edbg:find_source(M),
    {ok, Defs} = pp_record:read(Fname),
    ArgStr = "argument "++integer_to_list(ArgN)++":",
    ?info_msg("~nCall: ~p:~p/~p , ~s~n~s~n~s~n",
              [M,F,length(A),ArgStr,Sep,
               pp_record:print(lists:nth(ArgN,A), Defs)]).


find_mf(At, Buf, Mstr, Fstr) ->
    Mod = list_to_atom(Mstr),
    %% First get the set of trace messages to investigate
    L = lists:takewhile(
          fun({N,_}) when N>=At -> true;
             (_)                -> false
          end, Buf),
    %% Discard non-matching calls
    R = lists:dropwhile(
          fun({_N, ?CALL(_Pid, {M,_,_}, _As)}) when M == Mod andalso
                                                        Fstr == "" ->
                  false;
             ({_N, ?CALL_TS(_Pid, {M,_,_}, _TS, _As)}) when M == Mod andalso
                                                            Fstr == "" ->
                  false;
             ({_N, ?CALL(_Pid, {M,F,_}, _As)}) when M == Mod ->
                  not(lists:prefix(Fstr, atom_to_list(F)));
             ({_N, ?CALL_TS(_Pid, {M,F,_}, _TS, _As)}) when M == Mod ->
                  not(lists:prefix(Fstr, atom_to_list(F)));
             (_) ->
                  true
          end, lists:reverse(L)),
    case R of
        [{N,_}|_] -> N;
        _         -> not_found
    end.

find_mf_av(At, Buf, Mstr, Fstr, An, Av) ->
    Mod = list_to_atom(Mstr),
    %% First get the set of trace messages to investigate
    L = lists:takewhile(
          fun({N,_}) when N>=At -> true;
             (_)                -> false
          end, Buf),
    %% Discard non-matching calls
    R = lists:dropwhile(
          fun({_N, ?CALL(_Pid, {M,F,A}, _As)}) when M == Mod andalso
                                                    length(A) >= An ->
                  do_find_mf_av(Fstr, An, Av, F, A);
             ({_N, ?CALL_TS(_Pid, {M,F,A}, _TS, _As)}) when M == Mod andalso
                                                            length(A) >= An ->
                  do_find_mf_av(Fstr, An, Av, F, A);
             (_) ->
                  true
          end, lists:reverse(L)),
    case R of
        [{N,_}|_] -> N;
        _         -> not_found
    end.

do_find_mf_av(Fstr, An, Av, F, A) ->
    case lists:prefix(Fstr, atom_to_list(F)) of
        true ->
            ArgStr = lists:flatten(io_lib:format("~p",[lists:nth(An,A)])),
            try re:run(ArgStr,Av) of
                nomatch -> true;
                _       -> false
            catch
                _:_ -> true
            end;
        _ ->
            true
    end.

get_buf_at(At, Buf) ->
    lists:takewhile(
      fun({N,_}) when N>=At -> true;
         (_)                -> false
      end, Buf).

get_buf_before_at(At, Buf) ->
    lists:dropwhile(
      fun({N,_}) when N>=At -> true;
         (_)                -> false
      end, Buf).


find_retval(At, Buf, Str) ->
    %% First get the set of trace messages to investigate
    L = get_buf_at(At, Buf),
    %% Discard non-matching return values
    try
        lists:foldl(
          fun({N, ?RETURN_FROM(_Pid, MFA, Value, _As)}=X,_Acc) ->
                  do_find_retval(N, Str, Value, X, MFA, Buf);
             ({N, ?RETURN_FROM_TS(_Pid, MFA, Value, _TS, _As)}=X,_Acc) ->
                  do_find_retval(N, Str, Value, X, MFA, Buf);
             (X, Acc) ->
                  [X|Acc]
          end, [], lists:reverse(L)),
        not_found
    catch
        throw:{matching_call,{N,_}} -> N;
        _:_                         -> not_found
    end.

do_find_retval(At, Str, Value, X, MFA, Buf) ->
    L = get_buf_before_at(At, Buf),
    ValStr = lists:flatten(io_lib:format("~p",[Value])),
    try re:run(ValStr, Str) of
        nomatch -> [X|Buf];
        _       -> find_matching_call(MFA, L, 0)
    catch
        _:_ -> [X|Buf]
    end.

%% X = {Trace(_ts), Pid, CallOrReturnFrom, MFA, ...}
-define(m(X), element(1,element(4,X))).
-define(f(X), element(2,element(4,X))).
-define(a(X), element(3,element(4,X))).
-define(l(X), length(element(3,element(4,X)))).

%% Will throw exception at success; crash if nothing is found!
find_matching_call({M,F,A}, [{_N,Trace}=X|_], 0)
  when ?is_trace_call(Trace) andalso
       ?m(Trace) == M andalso
       ?f(Trace) == F andalso
       ?l(Trace) == A ->
    throw({matching_call,X});
find_matching_call({M,F,A}=MFA, [{_N,Trace}|L], N)
  when ?is_trace_call(Trace) andalso
       ?m(Trace) == M andalso
       ?f(Trace) == F andalso
       ?l(Trace) == A ->
    find_matching_call(MFA, L, N-1);
find_matching_call({M,F,A}=MFA, [{_N,Trace}|L], N)
  when ?is_trace_return_from(Trace) andalso
       ?m(Trace) == M andalso
       ?f(Trace) == F andalso
       ?a(Trace) == A ->
    find_matching_call(MFA, L, N+1);
find_matching_call(MFA, [{_N,_Trace}|L], N) ->
    find_matching_call(MFA, L, N).


get_trace_setup_mod(Opts) ->
    get_opts(Opts, setup_mod, edbg_trace_filter).

get_trace_max(Opts) ->
    get_opts(Opts, trace_max, 10000).

get_opts(Opts, Key, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {Key, Mod} -> Mod;
        _          -> Default
    end.


save_start_trace(X) ->
    {ok,Fd} = file:open("trace.edbg",[write]),
    try
        io:format(Fd, "~p.~n", [X])
    after
        file:close(Fd)
    end.

field_size([{N,_}|_]) ->
    integer_to_list(length(integer_to_list(N)));
field_size(_) ->
    "1". % shouldn't happen...

list_trace(Tlist, Buf) ->
    maybe_put_first_timestamp(Buf),
    Fs = field_size(Buf),
    Zlist =
        lists:foldr(

          %% C A L L
          fun({N, ?CALL(Pid, {M,F,A}, As)},
              #tlist{level = LevelMap,
                     memory = MemoryP,
                     at = At,
                     vlines = VL,
                     page = Page} = Z)
                when ?inside_vl(At,N,VL,Page) ->
                  Level = maps:get(Pid, LevelMap, 0),
                  MPid = mpid(MemoryP, Pid, As),
                  ?info_msg("~"++Fs++".s:~s ~s ~p:~p/~p~n",
                           [integer_to_list(N),pad(Level),MPid,M,F,length(A)]),
                  Z#tlist{vlines = VL + 1,
                          level = maps:put(Pid, Level+1, LevelMap)};

             ({N, ?CALL_TS(Pid, {M,F,A}, TS, As)},
              #tlist{level = LevelMap,
                     memory = MemoryP,
                     at = At,
                     vlines = VL,
                     page = Page} = Z)
                when ?inside_vl(At,N,VL,Page) ->
                  Level = maps:get(Pid, LevelMap, 0),
                  MPid = mpid(MemoryP, Pid, As),
                  ?info_msg("~"++Fs++".s:~s ~s ~p:~p/~p - ~p~n",
                           [integer_to_list(N),pad(Level),MPid,M,F,length(A),
                            xts(TS)]),
                  Z#tlist{vlines = VL + 1,
                          level = maps:put(Pid, Level+1, LevelMap)};

             ({_N, ?CALL(Pid, {_M,_F,_A}, _As}),
              #tlist{level = LevelMap} = Z) ->
                  Level = maps:get(Pid, LevelMap, 0),
                  Z#tlist{level = maps:put(Pid, Level+1, LevelMap)};

             ({_N, ?CALL_TS(Pid, {_M,_F,_A}, _TS, _As)},
              #tlist{level = LevelMap} = Z) ->
                  Level = maps:get(Pid, LevelMap, 0),
                  Z#tlist{level = maps:put(Pid, Level+1, LevelMap)};

             %% R E T U R N _ F R O M
             ({_N, ?RETURN_FROM(Pid, _MFA, _Value, _As)},
              #tlist{level = LevelMap} = Z) ->
                  Level = maps:get(Pid, LevelMap, 0),
                  Z#tlist{level = maps:put(Pid,erlang:max(Level-1,0),LevelMap)};

             ({_N, ?RETURN_FROM_TS(Pid, _MFA, _Value, _TS, _As)},
              #tlist{level = LevelMap} = Z) ->
                  Level = maps:get(Pid, LevelMap, 0),
                  Z#tlist{level = maps:put(Pid,erlang:max(Level-1,0),LevelMap)};

             %% S E N D
             ({N, ?SEND(FromPid, Msg, ToPid, _As)},
              #tlist{send_receive = true,
                     level = LevelMap,
                     at = At,
                     vlines = VL,
                     page = Page} = Z)
                when ?inside_vl(At,N,VL,Page) ->
                  Level = maps:get(FromPid, LevelMap, 0),
                  ?info_msg("~"++Fs++".s:~s >>> Send(~p) -> To(~p)  ~s~n",
                            [integer_to_list(N),pad(Level),
                             FromPid,ToPid,truncate(Msg)]),
                  Z#tlist{vlines = VL + 1};
             ({_N, ?SEND(_FromPid, _Msg, _ToPid, _As)}, Z) ->
                  Z;

             ({N, ?SEND_TS(FromPid, Msg, ToPid, TS, _As)},
               #tlist{send_receive = true,
                     level = LevelMap,
                     at = At,
                     vlines = VL,
                     page = Page} = Z)
                when ?inside_vl(At,N,VL,Page) ->
                 Level = maps:get(FromPid, LevelMap, 0),
                  ?info_msg("~"++Fs++".s:~s >>> Send(~p) -> To(~p)  ~s - ~p~n",
                            [integer_to_list(N),pad(Level),
                             FromPid,ToPid,truncate(Msg),xts(TS)]),
                  Z#tlist{vlines = VL + 1};
             ({_N, ?SEND_TS(_FromPid, _Msg, _ToPid, _TS, _As)}, Z) ->
                  Z;

             %% R E C E I V E
             ({N, ?RECEIVE(ToPid, Msg, _As)},
              #tlist{send_receive = true,
                     level = LevelMap,
                     at = At,
                     vlines = VL,
                     page = Page} = Z)
                when ?inside_vl(At,N,VL,Page) ->
                  Level = maps:get(ToPid, LevelMap, 0),
                  ?info_msg("~"++Fs++".s:~s <<< Receive(~p)  ~s~n",
                            [integer_to_list(N),pad(Level),
                             ToPid,truncate(Msg)]),
                  Z#tlist{vlines = VL + 1};
             ({_N, ?RECEIVE(_ToPid, _Msg, _As)}, Z) ->
                  Z;

             ({N, ?RECEIVE_TS(ToPid, Msg, TS, _As)},
              #tlist{send_receive = true,
                     level = LevelMap,
                     at = At,
                     vlines = VL,
                     page = Page} = Z)
                when ?inside_vl(At,N,VL,Page) ->
                  Level = maps:get(ToPid, LevelMap, 0),
                  ?info_msg("~"++Fs++".s:~s <<< Receive(~p)  ~s - ~p~n",
                            [integer_to_list(N),pad(Level),
                             ToPid,truncate(Msg),xts(TS)]),
                  Z#tlist{vlines = VL + 1};
             ({_N, ?RECEIVE_TS(_ToPid, _Msg, _TS, _As)}, Z) ->
                  Z

          end, Tlist#tlist{vlines = 0,
                           level = maps:new()}, Buf),

    NewAt = Tlist#tlist.at + Tlist#tlist.page + 1,
    Zlist#tlist{at = NewAt}.

mpid(true = _MemoryP, Pid, As) ->
    case lists:keyfind(memory, 1, As) of
        {_, Mem} when is_integer(Mem) ->
            pid_to_list(Pid)++"("++integer_to_list(Mem)++")";
        _ ->
            pid_to_list(Pid)
    end;
mpid(_MemoryP , Pid, _As) ->
    pid_to_list(Pid).


truncate(Term) ->
    truncate(Term, 20).

truncate(Term, Length) ->
    string:slice(io_lib:format("~p",[Term]), 0, Length)++"...".

%% Elapsed monotonic time since first trace message
xts(TS) ->
    case get(first_monotonic_timestamp) of
        undefined ->
            0;
        XTS ->
            TS - XTS
    end.


maybe_put_first_timestamp(Buf) ->
    case get(first_monotonic_timestamp) of
        undefined ->
            case Buf of
                [{_N, ?CALL_TS(_Pid, _MFA, _TS, _S)}|_] ->
                    put(first_monotonic_timestamp,
                        get_first_monotonic_timestamp(Buf));
                [{_N, ?RETURN_FROM_TS(_Pid, _MFA, _Value, _TS, _As)}|_] ->
                    put(first_monotonic_timestamp,
                        get_first_monotonic_timestamp(Buf));
                [{_N, ?SEND_TS(_FromPid, _Msg, _ToPid, _TS, _As)}|_] ->
                    put(first_monotonic_timestamp,
                        get_first_monotonic_timestamp(Buf));
                [{_N, ?RECEIVE_TS(_FromPid, _Msg, _TS, _As)}|_] ->
                    put(first_monotonic_timestamp,
                        get_first_monotonic_timestamp(Buf));
                _ ->
                    undefined
            end;
        TS ->
            TS
    end.

get_first_monotonic_timestamp(Buf) ->
    case lists:reverse(Buf) of
        [{_N, ?CALL_TS(_Pid, _MFA, TS, _S)}|_] ->
            TS;
        [{_N, ?RETURN_FROM_TS(_Pid, _MFA, _Value, TS, _As)}|_] ->
            TS;
        [{_N, ?SEND_TS(_FromPid, _Msg, _ToPid, TS, _As)}|_] ->
            TS;
        [{_N, ?RECEIVE_TS(_FromPid, _Msg, TS, _As)}|_] ->
            TS
    end.


get_return_value(N, [{I,_}|T]) when I < N ->
    get_return_value(N, T);
get_return_value(N, [{N, ?CALL(_Pid, {M,F,A}, _As)}|T]) ->
    find_return_value({M,F,length(A)}, T);
get_return_value(N, [{N, ?CALL_TS(_Pid, {M,F,A}, _TS, _As)}|T]) ->
    find_return_value({M,F,length(A)}, T);
get_return_value(N, [{I,_}|_]) when I > N ->
    not_found;
get_return_value(_, []) ->
    not_found.

find_return_value(MFA, T) ->
    find_return_value(MFA, T, 0).

find_return_value(MFA,[{_, ?RETURN_FROM(_Pid, MFA, Val, _As)}|_],0 = _Depth)->
    {ok, MFA, Val};
find_return_value(MFA, [{_, ?RETURN_FROM_TS(_Pid, MFA, Val, _TS, _As)}|_],
                  0 = _Depth) ->
    {ok, MFA, Val};
find_return_value(MFA, [{_, ?RETURN_FROM(_Pid, MFA, _Val, _As)}|T], Depth)
  when Depth > 0 ->
    find_return_value(MFA, T, Depth-1);
find_return_value(MFA,[{_, ?RETURN_FROM_TS(_Pid, MFA, _MFA, _TS, _As)}|T],Depth)
  when Depth > 0 ->
    find_return_value(MFA, T, Depth-1);
find_return_value(MFA, [{_, ?CALL(_Pid, MFA, _As)}|T], Depth) ->
    find_return_value(MFA, T, Depth+1);
find_return_value(MFA, [{_, ?CALL_TS(_Pid, MFA, _TS, _As)}|T], Depth) ->
    find_return_value(MFA, T, Depth+1);
find_return_value(MFA, [_|T], Depth) ->
    find_return_value(MFA, T, Depth);
find_return_value(_MFA, [], _Depth) ->
    not_found.


mlist(N, Buf) ->
    try
        case lists:keyfind(N, 1, Buf) of
            {_, ?CALL(_Pid, MFA, _As)} ->
                do_mlist(MFA);

            {_, ?CALL_TS(_Pid, MFA, _TS, _As)} ->
                do_mlist(MFA);

            {_, ?SEND(SendPid, Msg, ToPid, _As)} ->
                show_send_msg(SendPid, ToPid, Msg);

            {_, ?SEND_TS(SendPid, Msg, ToPid, _TS, _As)} ->
                show_send_msg(SendPid, ToPid, Msg);

            {_, ?RECEIVE(RecvPid, Msg, _As)} ->
                show_recv_msg(RecvPid, Msg);

            {_, ?RECEIVE_TS(RecvPid, Msg, _TS, _As)} ->
                show_recv_msg(RecvPid, Msg);

            _ ->
                ?info_msg("not found~n",[])
        end
    catch
        _:Err ->
            ?info_msg(?c_err("CRASH: ~p~n"), [Err])
    end.

show_send_msg(SendPid, ToPid, Msg) ->
    ?info_msg("~nMessage sent by: ~p  to: ~p~n~p~n",
                      [SendPid,ToPid,Msg]).

show_recv_msg(RecvPid, Msg) ->
    ?info_msg("~nMessage received by: ~p~n~p~n",
                      [RecvPid,Msg]).


do_mlist({M,F,A}) ->
    Fname = edbg:find_source(M),
    {ok, SrcBin, Fname} = erl_prim_loader:get_file(Fname),
    LF = atom_to_list(F),
    Src = binary_to_list(SrcBin),
    %% '.*?' ::= ungreedy match!
    RegExp = "\\n"++LF++"\\(.*?->",
    %% 'dotall' ::= allow multiline function headers
    case re:run(Src, RegExp, [global,dotall,report_errors]) of
        {match, MatchList} ->
            {FmtStr, Args} = mk_print_match(SrcBin, MatchList),
            Sep = pad(35, $-),
            ?info_msg("~nCall: ~p:~p/~p~n~s~n"++FmtStr++"~n~s~n",
                      [M,F,length(A),Sep|Args]++[Sep]);
        Else ->
            ?info_msg("nomatch: ~p~n",[Else])
    end.


mk_print_match(SrcBin, MatchList) ->
    F = fun([{Start,Length}], {FmtStrAcc, ArgsAcc}) ->
                <<_:Start/binary,Match:Length/binary,_/binary>> = SrcBin,
                Str = binary_to_list(Match),
                {"~s~n"++FmtStrAcc, [Str|ArgsAcc]}
        end,
    lists:foldr(F, {"",[]}, MatchList).



pad(0) -> [];
pad(N) ->
    pad(N, $\s).

pad(N,C) ->
    lists:duplicate(N,C).

send(Pid, Msg) ->
    Pid ! {trace, self(), Msg},
    receive
        {Pid, ok}   -> ok;
        {Pid, stop} -> exit(stop)
    end.

reply(Pid, Msg) ->
    Pid ! {self(), Msg}.



record_field_index({M,_,_}, Record, Field) ->
    %% we cache the Record Field info for a Module
    case get({M,Record}) of
        undefined ->
            %% [{Field,Index}]
            Fields = get_record_fields(M, Record),
            put({M,Record}, Fields),
            {Field,Index} = lists:keyfind(Field, 1, Fields),
            Index;
        Fields ->
            {Field,Index} = lists:keyfind(Field, 1, Fields),
            Index
    end.



get_record_fields(Mod, Record) ->
    %% Get the Record definitions for the Module
    Fname = edbg:find_source(Mod),
    {ok, Defs} = pp_record:read(Fname),

    %% Get the Record Fields
    {Record,{attribute,_,record,{Record,Fs}}} = lists:keyfind(Record, 1, Defs),

    %% Extract the Record Field names; keep the order
    F = fun({typed_record_field,{record_field,_,{atom,_,Name},_},_},Acc) ->
                [Name|Acc];
           ({typed_record_field,{record_field,_,{atom,_,Name}},_},Acc) ->
                [Name|Acc];
           ({record_field,_,{atom,_,Name}},Acc) ->
                [Name|Acc];
           ({record_field,_,{atom,_,Name},_},Acc) ->
                [Name|Acc]
        end,
    Fields = lists:foldr(F, [], Fs),

    %% Create a Key-Value list of the fields and their index into the Record
    lists:zip(Fields, lists:seq(2, erlang:length(Fields)+1)).
