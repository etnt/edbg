%%%-------------------------------------------------------------------
%%% @doc Explore all supervision trees
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(edbg_sup_trees).

-export([all_sup_trees/0
        , loop/3
        , start/0
        , supervisors/0
        , supervisors/1
        ]).


-define(SERVER, ?MODULE).

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

-record(state,
        {
         sup_trees = [],
         trace_started = false,
         traced_pid
        }).

%% Regarding the 'modules' entry:
%% It is used by the release handler during code replacement to determine
%% which processes are using a certain module. As a rule of thumb,
%% if the child process is a supervisor, gen_server or, gen_statem,
%% this is to be a list with one element [Module], where Module is
%% the callback module. If the child process is an event manager
%% (gen_event) with a dynamic set of callback modules,
%% value dynamic must be used.
-record(sup_tree,
        {n = 0,  % enumeration number
         id,
         pid,
         modules = [],
         children = [],
         expand = false
        }).

-record(worker,
        {n = 0,  % enumeration number
         id,
         pid,
         modules = [],
         is_gen_server = false,
         gen_module
        }).


start() ->
    Self = self(),
    Prompt = spawn_link(fun() -> prompt(Self) end),
    Prompt ! show,
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
    process_flag(trap_exit, true),
    State0 = refresh(),
    State = help(show(State0)),
    loop(Pid, prompt(), State).


loop(Pid, Prompt, State0) ->
    io:format("~n",[]),
    State =
        case string:tokens(b2l(io:get_line(Prompt)), "\n") of
            ["h"++X]  -> help(X, State0);
            ["x"++X]  -> help(expand(X, State0));
            ["s"++X]  -> shrink_or_show(X, State0);
            ["p"++X]  -> pinfo(X, State0);
            ["g"++X]  -> gen_state(X, State0);
            ["b"++X]  -> backtrace(X, State0);
            ["m"++X]  -> setup_monitor(X, State0);
            ["r"++_]  -> help(show(refresh()));
            ["ts"++X] -> start_trace(X, State0);
            ["te"++_] -> stop_trace(State0);
            ["tf"++_] -> show(show_trace(State0));
            ["q"++_]  -> Pid ! quit, exit(normal);

            _X ->
                ?info_msg("prompt got: ~p~n",[_X]),
                State0
        end,
    ?MODULE:loop(Pid, Prompt, State).


b2l(B) when is_binary(B) ->
     erlang:binary_to_list(B);
b2l(L) when is_list(L) ->
    L.


refresh() ->
    #state{sup_trees = enumerate(all_sup_trees())}.

prompt() ->
    "suptrees> ".


show(State) ->
    show("", State).

show(Chars, State) ->
    display(Chars, State#state.sup_trees, 0),
    State.

display(Chars,
        [#sup_tree{n = N,
                   id = Id,
                   pid = Pid,
                   modules = Modules,
                   expand = false}|Tail],
        Indent) ->
    Pad = indent(Indent),
    io:format("~p:~s ~s~p ~p ~p~n", [N,  ?c_err("S"), Pad, Id, Pid, Modules]),
    display(Chars, Tail, Indent);
display(Chars,
        [#sup_tree{n = N,
                   id = Id,
                   pid = Pid,
                   modules = Modules,
                   children = Children,
                   expand = true}|Tail],
        Indent) ->
    Pad = indent(Indent),
    io:format("~p:~s ~s~p ~p ~p~n", [N,  ?c_err("S"), Pad, Id, Pid, Modules]),
    display(Chars, Children, Indent+2),
    display(Chars, Tail, Indent);
display(Chars, [#worker{n = N,
                        id = Id,
                        pid = Pid,
                        is_gen_server = IsGenSrv,
                        modules = Modules}|Tail], Indent) ->
    Pad = indent(Indent),
    io:format("~p:~s ~s~p ~p ~p~n", [N, ?c_warn(wg(IsGenSrv)), Pad, Id, Pid, Modules]),
    display(Chars, Tail, Indent);
display(_, [], _) ->
    ok.

wg(true = _IsGenSrv) -> "G";
wg(_)                -> "W".


indent(N) ->
    string:copies(" ", N).

help(State) ->
    print_help(),
    State.

help(_, State) ->
    print_help(),
    State.


get_pid(#sup_tree{pid = Pid}) -> Pid;
get_pid(#worker{pid = Pid})   -> Pid;
get_pid(Pid) when is_pid(Pid) -> Pid.


gen_state(Chars, #state{sup_trees = SupTrees} = State) ->
    try
        case parse_ints(Chars) of
            [I] ->
                do(SupTrees, I, fun(X) -> do_pinfo(X, fun p_gen_state/1) end),
                State;
            [I | Ints] ->
                do(SupTrees, I, fun(X) -> do_linfo(Ints, X,  fun p_gen_state/1) end),
                State
        end
    catch
        _:_ ->
            show(State)
    end.

%% Print the State reord of the callback module that
%% the gen_server is holding. Try to pretty-print it
%% if possible.
p_gen_state(Pid) when is_pid(Pid) ->
    try
        State = sys:get_state(Pid, 100),
        UsePpRecord = is_pp_record_available(),
        print_gen_state(Pid, State, UsePpRecord)
    catch
        _:_ ->
            ok
    end;
p_gen_state(_) ->
    ok.

print_gen_state(Pid, State, false = _UsePpRecord) ->
    io:format("~n~s ~p~n~p~n", [?c_warn("Process State:"),Pid, State]);
print_gen_state(Pid, State, true = _UsePpRecord) ->
    try
        {ok, {Mod,_,_}} = get_initial_call(Pid),
        Fname = edbg:find_source(Mod),
        {ok, Defs} = pp_record:read(Fname),
        io:format("~n~s~n", [pp_record:print(State, Defs)])
    catch
        _:_ ->
            print_gen_state(Pid, State, false)
    end.

is_pp_record_available() ->
    case code:ensure_loaded(pp_record) of
        {module, pp_record} ->
            true;
        _ ->
            false
    end.


start_trace(Chars, #state{trace_started = false, sup_trees = SupTrees} = State) ->
    try
        case parse_ints(Chars) of
            [I] ->
                do(SupTrees, I, fun(X) -> do_pinfo(X, fun do_trace/1) end),
                store_traced_pid(State);
            [I | Ints] ->
                do(SupTrees, I, fun(X) -> do_linfo(Ints, X,  fun do_trace/1) end),
                store_traced_pid(State)
        end
    catch
        _:_ ->
            State
    end;
start_trace(_Chars, #state{trace_started = true, traced_pid = Pid} = State) ->
    io:format("~n~s ~s ~p~n",[?c_err("<ERROR>"),"Already tracing on Pid:",Pid]),
    State.

store_traced_pid(State) ->
    case get(traced_pid) of
        Pid when is_pid(Pid) ->
            State#state{trace_started = true, traced_pid = Pid};
        _ ->
            State
    end.

do_trace(Pid) when is_pid(Pid) ->
    edbg:fstart([], [{trace_spec,Pid},
                     dump_output_eager,
                     send_receive,
                     {max_msgs, 1000000}]),
    put(traced_pid, Pid),
    io:format("~n~s ~s ~p~n",[?c_warn("<INFO>"),"Tracing on Pid:",Pid]).


stop_trace(#state{trace_started = true, traced_pid = Pid} = State) ->
    edbg:fstop(),
    io:format("~n~s ~s ~p~n",[?c_warn("<INFO>"),"Stopped tracing on Pid:",Pid]),
    State#state{trace_started = false, traced_pid = undefined};
stop_trace(State) ->
    %% In case suptree was stopped/started and
    %% perhaps even the shells proc.dict was erased;
    Pid = case erase(traced_pid) of
              Pid0 when is_pid(Pid0) -> Pid0;
              _ ->
                  edbg_tracer:get_traced_pid()
          end,
    edbg:fstop(),
    io:format("~n~s ~s ~p~n",[?c_warn("<INFO>"),"Stopped tracing on Pid:",Pid]),
    State#state{trace_started = false, traced_pid = undefined}.

show_trace(State) ->
    edbg:file(),
    State.


setup_monitor(Chars, #state{sup_trees = SupTrees} = State) ->
    try
        case parse_ints(Chars) of
            [I] ->
                do(SupTrees, I, fun(X) -> do_pinfo(X, fun create_monitor/1) end),
                State;
            [I | Ints] ->
                do(SupTrees, I, fun(X) -> do_linfo(Ints, X,  fun create_monitor/1) end),
                State
        end
    catch
        _:_ ->
            show(State)
    end.

create_monitor(Pid) when is_pid(Pid) ->
    F = fun() ->
                ReqId = erlang:monitor(process, Pid),
                io:format("~n~s ~s ~p~n",[?c_warn("<INFO>"),"Monitoring:",Pid]),
                receive
                    {'DOWN', ReqId, process, Pid, ExitReason} ->
                        io:format("~n~s ~s ~p , Reason: ~p~n",
                                  [?c_warn("<INFO>"),"Monitor got DOWN from:",
                                   Pid, ExitReason])
                end
        end,
    erlang:spawn(F);
create_monitor(_) ->
    ok.



pinfo(Chars, #state{sup_trees = SupTrees} = State) ->
    try
        case parse_ints(Chars) of
            [I] ->
                do(SupTrees, I, fun(X) -> do_pinfo(X, fun out_pinfo/1) end),
                State;
            [I | Ints] ->
                do(SupTrees, I, fun(X) -> do_linfo(Ints, X,  fun out_pinfo/1) end),
                State
        end
    catch
        _:_ ->
            show(State)
    end.

backtrace(Chars, #state{sup_trees = SupTrees} = State) ->
    try
        case parse_ints(Chars) of
            [I] ->
                do(SupTrees, I, fun(X) -> do_pinfo(X, fun out_btrace/1) end),
                State;
            [I | Ints] ->
                do(SupTrees, I, fun(X) -> do_linfo(Ints, X,  fun out_btrace/1) end),
                State
        end
    catch
        _:_ ->
            show(State)
    end.




do_pinfo(X, OutFun) ->
    Pid = get_pid(X),
    OutFun(Pid),
    X.

do_linfo([I], X, OutFun) ->
    try
        Pid = get_pid(X),
        {links, Pids} = erlang:process_info(Pid, links),
        LPid = lists:nth(I, Pids),
        OutFun(LPid),
        X
    catch
        _:_ -> X
    end;
do_linfo([H|T], X, OutFun) ->
    try
        Pid = get_pid(X),
        {links, Pids} = erlang:process_info(Pid, links),
        LPid = lists:nth(H, Pids),
        do_linfo(T, LPid, OutFun)
    catch
        _:_ -> X
    end;
do_linfo([], X, _OutFun) ->
    X.

out_pinfo(Pid) when is_pid(Pid) ->
    io:format("~n=== Process Info: ~p~n~p~n", [Pid,erlang:process_info(Pid)]).

out_btrace(Pid) when is_pid(Pid) ->
    io:format("~n=== Process Backtrace: ~p~n", [Pid]),
    c:bt(Pid).

expand(Chars, #state{sup_trees = SupTrees0} = State) ->
    try
        I = parse_int(Chars),
        SupTrees = set_expand(SupTrees0, I, true),
        show(State#state{sup_trees = SupTrees})
    catch
        _:_ ->
            help(show(State))
    end.

shrink_or_show(Chars, #state{sup_trees = SupTrees0} = State) ->
    try
        I = parse_int(Chars),
        SupTrees = set_expand(SupTrees0, I, false),
        help(show(State#state{sup_trees = SupTrees}))
    catch
        _:_ ->
            help(show(State))
    end.


enumerate(SupTrees0) ->
    {SupTrees, _MaxN} = enumerate(SupTrees0, 1),
    SupTrees.

enumerate([#sup_tree{children = Children0} = H | Tail0], N) ->
    {Children, N1} = enumerate(Children0, N+1),
    {Tail, N2} = enumerate(Tail0, N1),
    {[H#sup_tree{n = N, children = Children} | Tail], N2};
enumerate([#worker{} = H | Tail0], N) ->
    {Tail, N1} = enumerate(Tail0, N+1),
    {[H#worker{n = N} | Tail], N1};
enumerate([], N) ->
    {[],N}.





set_expand([#sup_tree{n = N} = H | T], N, Bool) ->
    [H#sup_tree{expand = Bool} | T];
set_expand([#sup_tree{n = N1, children = Children} = H1,
            #sup_tree{n = N2} = H2 | T], I, Bool)
  when N1<I andalso N2>I->
    [H1#sup_tree{children = set_expand(Children, I, Bool)},H2|T];
set_expand([#sup_tree{n = N, children = Children} = H], I, Bool) when N<I ->
    [H#sup_tree{children = set_expand(Children, I, Bool)}];
set_expand([H|T], N, Bool) ->
    [H | set_expand(T, N, Bool)];
set_expand([], _, _) ->
    [].


do([#sup_tree{n = N} = H | T], N, Fun) ->
    [Fun(H) | T];
do([#sup_tree{n = N1, children = Children} = H1,
            #sup_tree{n = N2} = H2 | T], I, Fun)
  when N1<I andalso N2>I->
    [H1#sup_tree{children = do(Children, I, Fun)},H2|T];
do([#sup_tree{n = N, children = Children} = H], I, Fun) when N<I ->
    [H#sup_tree{children = do(Children, I, Fun)}];
do([#worker{n = N} = H | T], N, Fun) ->
    [Fun(H) | T];
do([H|T], N, Fun) ->
    [H | do(T, N, Fun)];
do([], _, _) ->
    [].


parse_int(Chars) ->
    try string:tokens(string:strip(Chars), " ") of
        [X]   -> list_to_integer(X);
        _     -> throw(parse_int)
    catch
        _:_ ->
            throw(parse_int)
    end.

parse_ints(Chars) ->
    try
        [list_to_integer(X) ||
            X <- string:tokens(string:strip(Chars), " ")]
    catch
        _:_ ->
            throw(parse_int)
    end.



print_help() ->
    S1 = " (h)elp e(x)pand [<N>] (s)hrink [<N>] (s)how",
    S2 = " (p)rocess info [<N> ]+ (b)acktrace [<N> ]+",
    S3 = " (m)onitor [<N> ]+ (g)en-state [<N> ]+",
    S4 = " (ts)start-trace [<N> ]+ (te)end-trace (tf)show-trace",
    S5 = " (r)efresh (q)uit",
    S = io_lib:format("~n~s~n~s~n~s~n~s~n~s~n",[S1,S2,S3,S4,S5]),
    ?info_msg(?help_hi(S), []).


supervisors() ->
    supervisors(kernel_sup).

supervisors(Supervisor) when is_atom(Supervisor) ->
    supervisors(Supervisor, new_sup_tree(Supervisor)).

%% supervisors(user = Supervisor, SupTree) -> % eternal loop?
%%     SupTree;
%% supervisors(standard_error = Supervisor, SupTree) -> % eternal loop?
%%     SupTree;
supervisors(_Supervisor, SupTree) ->
    case supervisor:which_children(SupTree#sup_tree.pid) of
        Children when is_list(Children) ->
            lists:foldl(fun({Id, Pid, Type, Modules}, AccTree)
                              when (Type == supervisor) andalso is_pid(Pid) ->
                                Cs = AccTree#sup_tree.children,
                                AccTree#sup_tree{modules = Modules,
                                                 children =
                                                     Cs ++
                                                     [supervisors(Id,  new_sup_tree(Id, Pid))]};
                           ({Id, Pid, Type, Modules}, AccTree) when Type == worker ->
                                {IsGenSrv, Gmod} = is_gen_server(Pid),
                                Entry = #worker{id = Id, pid = Pid,
                                                is_gen_server = IsGenSrv,
                                                gen_module = Gmod,
                                                modules = Modules},
                                Cs = AccTree#sup_tree.children,
                                AccTree#sup_tree{children = Cs ++ [Entry]};
                           (Entry, AccTree) ->
                                ?warn_msg("Ignoring: ~p~n",[Entry]),
                                AccTree
                        end, SupTree, Children);
        _ ->
            SupTree
    end.

%% Is the given process of type: gen_server, gen_statem or gen_event?
%% We will use this info to enable extration of the State using
%% the sys:get_state/2 function
is_gen_server(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, current_function) of
        {current_function, {M , _Fun, _NoOfArgs}}
        when M == gen_server orelse
             M == gen_statem orelse
             M == gen_event ->
            {true, M};
        _ ->
            {false, undefined}
    end;
is_gen_server(_) ->
    {false, undefined}.


%% Try to find all Supervisors in the system.
%% Idea: Go through all registered processes and
%% try to figure out if they are a supervisor.
%% Is there a better way?
%% Can a supervisor not be registered and hence not be included here?
all_sup_trees() ->
    Sups = lists:foldr(
             fun(Id,Acc) ->
                     case get_initial_call(whereis(Id)) of
                         {ok, {supervisor,_,_}} ->
                             [Id|Acc];
                         _ ->
                             Acc
                     end
             end, [], lists:sort(erlang:registered())),
    [supervisors(Id) || Id <- Sups].


get_initial_call(Pid) when is_pid(Pid) ->
    case process_info(Pid, dictionary) of
        {dictionary,L} ->
            case lists:keyfind('$initial_call', 1, L) of
                {'$initial_call', Icall} ->
                    {ok, Icall};
                _ ->
                    false
            end;
        _ ->
            false
    end.


new_sup_tree(SupervisorId) ->
    new_sup_tree(SupervisorId, erlang:whereis(SupervisorId)).

new_sup_tree(SupervisorId, Pid) ->
    #sup_tree{id = SupervisorId,
              pid = Pid}.
