-module(edbg).
-on_load(init_edbg/0).

-export([a/0,
         a/1,
         a/3,
         attach/1,
         attach/3,
         b/2,
         br/0,
         br/1,
         break/2,
         breaks/0,
         breaks/1,
         break_in/3,
         c/1,
         c/3,
         continue/1,
         continue/3,
         delete_break/2,
         bdel/2,
         disable_break/2,
         boff/2,
         enable_break/2,
         bon/2,
         f/1,
         f/3,
         finish/1,
         finish/3,
         i/1,
         i/2,
         id/0,
         it/3,
         lab/0,
         load_all_breakpoints/0,
         ml/1,
         ml/2,
         ml/3,
         mlist/1,
         mlist/2,
         mlist/3,
         pl/0,
         plist/0,
         n/1,
         n/3,
         next/1,
         next/3,
         save_all_breakpoints/0,
         s/1,
         s/3,
         step/1,
         step/3,
         t/2,
         t/3,
         tlist/0,
         lts/0,
         tstart/2,
         tstart/3,
         tquit/0
        ]).

%% Internal export
-export([aloop/1,
         ploop/3,
         it_loop/1,
         itest_at_break/1,
         find_source/1
        ]).

%% Import color functions
-import(edbg_color_srv,
        [info_msg/2,
         att_msg/2,
         warn_msg/2,
         err_msg/2,
         cur_line_msg/2,
         c_hi/1,
         c_warn/1,
         c_err/1,
         help_hi/1]).

init_edbg() ->
    ok = edbg_color_srv:init().

lts() ->
    edbg_tracer:lts().

tquit() ->
    edbg_tracer:tquit().

tstart(Mod, Mods) ->
    edbg_tracer:tstart(Mod,Mods).

tstart(Mod, Mods, Opts) when is_atom(Mod) andalso
                             is_list(Mods) andalso
                             is_list(Opts) ->
    edbg_tracer:tstart(Mod,Mods,Opts).


tlist() ->
    edbg_tracer:tlist().

pl() -> plist().

ml(X) -> mlist(X).
ml(X,Y) -> mlist(X,Y).
ml(X,Y,Z) -> mlist(X,Y,Z).


br() -> breaks().
breaks() ->
    print_all_breakpoints(int:all_breaks()).

br(Mod) -> breaks(Mod).
breaks(Mod) ->
    print_all_breakpoints(int:all_breaks(Mod)).

b(Mod, Line) ->
    break(Mod,Line).

break(Mod, Line) ->
    ok = int:break(Mod, Line),
    save_all_breakpoints(),
    ok.

bdel(_Mod, _Line) -> delete_break.
delete_break(Mod, Line) ->
    ok = int:delete_break(Mod, Line),
    save_all_breakpoints(),
    ok.

boff(Mod, Line) -> disable_break(Mod, Line).
disable_break(Mod, Line) ->
    ok = int:disable_break(Mod, Line),
    save_all_breakpoints(),
    ok.

bon(Mod, Line) -> enable_break(Mod, Line).
enable_break(Mod, Line) ->
    ok = int:enable_break(Mod, Line),
    save_all_breakpoints(),
    ok.


c(Pid) -> continue(Pid).

c(P0, P1, P2) -> continue(c:pid(P0, P1, P2)).

continue(P0, P1, P2) -> continue(c:pid(P0, P1, P2)).

continue(Pid) when is_pid(Pid) ->
    int:continue(Pid).

%% FIXME doesn't work ?
break_in(Mod, Line, Arity) ->
    int:break_in(Mod, Line, Arity).

i(Mod) ->
    Fname = find_source(Mod),
    int:i(Fname).

id() ->
    int:interpreted().

i(Mod,Line) ->
    Fname = find_source(Mod),
    int:i(Fname),
    int:delete_break(Mod, Line),
    break(Mod, Line).

it(Mod,Line,Fun) when is_function(Fun)  ->
    Fname = find_source(Mod),
    int:i(Fname),
    t(Mod,Line,Fun).

%% Reuse the existing trigger function!
t(Mod,Line) ->
    int:delete_break(Mod, Line),
    ok = int:break(Mod, Line),
    ok = int:test_at_break(Mod, Line, {edbg,itest_at_break}),
    save_all_breakpoints(),
    ok.

t(Mod,Line,Fun) when is_function(Fun)  ->
    int:delete_break(Mod, Line),
    ok = itest_at_break(Fun),
    ok = int:break(Mod, Line),
    ok = int:test_at_break(Mod, Line, {edbg,itest_at_break}),
    save_all_breakpoints(),
    ok.



-define(itest_at_break, itest_at_break).

itest_at_break(Fun) when is_function(Fun) ->
    case whereis(?itest_at_break) of
        Pid when is_pid(Pid) ->
            Pid ! {self(), cond_fun, Fun},
            receive
                {Pid, ok} -> ok
            after 3000 ->
                {error, timeout}
            end;
        _ ->
            Pid = spawn(fun() -> ?MODULE:it_loop(Fun) end),
            register(?itest_at_break, Pid),
            ok
    end;
%%
itest_at_break(Bindings) ->
    try
        case whereis(?itest_at_break) of
            Pid when is_pid(Pid) ->
                Pid ! {self(), eval, Bindings},
                receive
                    {Pid, result, Bool} ->
                        Bool
                after 3000 ->
                    false
                end
        end
    catch
        _:_ ->
            false
    end.


it_loop(Fun) ->
    receive
        {From, cond_fun, NewFun} ->
            From ! {self(), ok},
            ?MODULE:it_loop(NewFun);

        {From, eval, Bindings} ->
            try
                true = Fun(Bindings),
                From ! {self(), result, true}
            catch
                _:_ ->
                    From ! {self(), result, false}
            end,
            ?MODULE:it_loop(Fun);

        %% Used when saving breakpoints...
        {From, get_fun} ->
            From ! {self(), ok, Fun},
            ?MODULE:it_loop(Fun);

        _ ->
            ?MODULE:it_loop(Fun)
    end.

get_it_break_fun() ->
    try
        case whereis(?itest_at_break) of
            Pid when is_pid(Pid) ->
                Pid ! {self(), get_fun},
                receive
                    {Pid, ok, Fun} ->
                        Fun
                after 3000 ->
                    null
                end
        end
    catch
        _:_ ->
            null
    end.


s(Pid) -> step(Pid).

s(P0, P1, P2) -> step(c:pid(P0, P1, P2)).

step(P0, P1, P2) -> step(c:pid(P0, P1, P2)).

step(Pid) when is_pid(Pid) ->
    int:step(Pid),
    code_list(Pid).


n(Pid) -> next(Pid).

n(P0, P1, P2) -> next(c:pid(P0, P1, P2)).

next(P0, P1, P2) -> next(c:pid(P0, P1, P2)).

next(Pid) when is_pid(Pid) ->
    int:next(Pid),
    code_list(Pid).


f(Pid) -> finish(Pid).

f(P0, P1, P2) -> finish(c:pid(P0, P1, P2)).

finish(P0, P1, P2) -> finish(c:pid(P0, P1, P2)).

finish(Pid) when is_pid(Pid) ->
    int:finish(Pid),
    code_list(Pid).


code_list(Pid) ->
    case get_break_point(Pid) of
        {Pid, {_Mod,_Name,_Args}, break, {Mod,Line}} ->
            mlist(Mod,Line),
            ok;
        Else ->
            Else
    end.


-define(DEFAULT_CONTEXT_SIZE, 5).

-record(s, {
          pid,
          meta,
          prompt,
          break_at,
          break_points = [],
          context = ?DEFAULT_CONTEXT_SIZE,
          stack,
          trace = false,  % toggle
          mytracer
         }).


toggle(true)  -> false;
toggle(false) -> true.

%% Attach to the first found process sitting on a break point!
a() ->
    Self = self(),
    case [Pid || {Pid, _Func, Status, _Info} <- int:snapshot(),
               Status == break,
               Pid =/= Self] of
        [X|_] ->
            attach(X);
        _ ->
            "No process found to be at a break point!"
    end.

a(Pid) -> attach(Pid).

a(P0, P1, P2) -> attach(c:pid(P0, P1, P2)).

attach(P0, P1, P2) -> attach(c:pid(P0, P1, P2)).

attach(Pid) when is_pid(Pid) ->
    Self = self(),
    case int:attached(Pid) of
        {ok, Meta} ->
            Prompt = spawn_link(fun() -> prompt(Pid,Self) end),
            print_help(),
            aloop(#s{pid=Pid,
                     meta=Meta,
                     break_points = int:all_breaks(),
                     prompt=Prompt});
        Else ->
            err_msg("Failed to attach to process: ~p~n",[Pid]),
            Else
    end.


aloop(#s{meta   = Meta,
         prompt = Prompt} = S) ->
    receive

        {'EXIT', Meta, Reason} ->
            {meta_exit, Reason};

        {'EXIT', Prompt, Reason} ->
            {prompt_exit, Reason};

        %% FROM THE INTERPRETER (?)
        {int,{new_break,{{Mod,Line},_L} = Bp}} ->
            Bps = S#s.break_points,
            info_msg([c_hi("Break point set at"), " ~p:~p~n"], [Mod,Line]),
            ?MODULE:aloop(S#s{break_points = [Bp|Bps]});

        {int,{interpret,_Mod}} ->
            ?MODULE:aloop(S);

        {int,{break_options,{{_Mod,_Line},_Opts}}} ->
            ?MODULE:aloop(S);

        {int,{delete_break,{_Mod,_Line}}} ->
            ?MODULE:aloop(S);

        %% FROM THE META PROCESS
        {Meta, {trace,true}} ->
            ?MODULE:aloop(S);

        {Meta, running} ->
            ?MODULE:aloop(S);

        {Meta, {attached, _Mod, _Line, _Bool}} ->
            ?MODULE:aloop(S);

        {Meta, {break_at, Mod, Line, Cur}} ->
            Bs = int:meta(Meta, bindings, nostack),
            mlist(Mod,Line,S#s.context),
            ?MODULE:aloop(S#s{break_at = {Mod,Line},
                              stack = {Cur,Cur,Bs,{Mod,Line}}});

        {Meta,{exit_at, {Mod, Line}, Reason, Cur}} ->
            Bs = int:meta(Meta, bindings, nostack),
            mlist(Mod,Line,S#s.context),
            err_msg("ERROR REASON: ~p~n",[Reason]),
            ?MODULE:aloop(S#s{break_at = {Mod,Line},
                              stack = {Cur+1,Cur+1,Bs,{Mod,Line}}});

        {Meta, {trace_output, StrFun}} ->
            info_msg("~s~n",[StrFun("~tp")]),
            ?MODULE:aloop(S);

        %% FROM THE PROMPTER
        {Prompt, eval_expr, Str} ->
            Bs = int:meta(Meta, bindings, nostack),
            case evaluate(Str, Bs) of
                {ok, Value} ->
                    info_msg([c_hi("EVALUATED VALUE"), ":~n~p~n"], [Value]);
                {error, ErrStr} ->
                    err_msg("~s~n",[ErrStr])
            end,
            ?MODULE:aloop(S);

        {Prompt, up} ->
            {Cur, Max, _, _} = S#s.stack,
            case int:meta(Meta, stack_frame, {up, Cur}) of
                {New, {undefined,-1}, _Bs} -> % call from non-interpreted code
                    Stack = {New, Max, undefined, undefined};

                {New, {Mod,Line}, Bs} ->
                    Stack = {New, Max, Bs, {Mod,Line}},
                    mlist(Mod,Line,S#s.context);

                top ->
                    Stack = S#s.stack,
                    att_msg("Top of stack frames!~n",[])
            end,
            ?MODULE:aloop(S#s{stack = Stack});

        {Prompt, down} ->
            {Cur, Max, _, _} = S#s.stack,
            case int:meta(Meta, stack_frame, {down, Cur}) of
                {New, {undefined,-1}, _Bs} -> % call from non-interpreted code
                    Stack = {New, Max, undefined, undefined};

                {New, {Mod,Line}, Bs} ->
                    Stack = {New, Max, Bs, {Mod,Line}},
                    mlist(Mod,Line,S#s.context);

                bottom ->
                    Bs = int:meta(Meta, bindings, nostack),
                    {Mod,Line} = ModLine = S#s.break_at,
                    Stack = {Max,Max,Bs,ModLine},
                    mlist(Mod,Line,S#s.context)
            end,
            ?MODULE:aloop(S#s{stack = Stack});

        {Prompt, at} ->
            case S#s.stack of
                {_,_,_,{Mod,Line}} ->
                    mlist(Mod,Line,S#s.context);
                _ ->
                    att_msg("No info available...~n",[])
            end,
            ?MODULE:aloop(S);

        {Prompt, at, Ctx} ->
            case S#s.stack of
                {_,_,_,{Mod,Line}} ->
                    mlist(Mod,Line,Ctx);
                _ ->
                    att_msg("No info available...~n",[])
            end,
            ?MODULE:aloop(S);

        {Prompt, list_module, {Mod,Line,Ctx}} ->
            mlist(Mod,Line,Ctx),
            ?MODULE:aloop(S);

        {Prompt, list_module, {Mod,Line}} ->
            mlist(Mod,Line),
            ?MODULE:aloop(S);

        {Prompt, step = X} ->
            int:meta(Meta, X),
            ?MODULE:aloop(S);

        {Prompt, next = X} ->
            int:meta(Meta, X),
            ?MODULE:aloop(S);

        {Prompt, finish = X} ->
            int:meta(Meta, X),
            ?MODULE:aloop(S);

        {Prompt, processes} ->
            plist(),
            ?MODULE:aloop(S);

        {Prompt, messages = X} ->
            Ms = int:meta(Meta, X),
            info_msg([c_hi("MSGS"), ": ~p~n"] ,[Ms]),
            ?MODULE:aloop(S);

        {Prompt, continue = X} ->
            int:meta(Meta, X),
            ?MODULE:aloop(S);

        {Prompt, skip = X} ->
            int:meta(Meta, X),
            ?MODULE:aloop(S);

        {Prompt, context, Ctx} ->
            ?MODULE:aloop(S#s{context = Ctx});

        {Prompt, breakpoints} ->
            breaks(),
            ?MODULE:aloop(S);

        {Prompt, backtrace = X} ->
            Bt = int:meta(Meta, X, 1),
            info_msg("BT ~p~n",[Bt]),
            ?MODULE:aloop(S);

        {Prompt, interpret, Mod} ->
            int:i(Mod),
            info_msg([c_hi("Interpreted modules"), ": ~p~n"],
                     [int:interpreted()]),
            ?MODULE:aloop(S);

        {Prompt, var, Var} ->
            {_Cur,_Max,Bs,_} = S#s.stack,
            case find_var(Var, Bs) of
                [{VarName, Val}] ->
                    info_msg([c_hi("~p"), "= ~p~n"], [VarName, Val]);
                [{_Var1, _Val1}|_] = Candidates ->
                    info_msg([c_hi("~p"), c_warn(" ambigious prefix"),
                              ": ~p~n"], [Var, [N || {N,_} <- Candidates]]);
                []      -> info_msg([c_hi("~p"), c_err(" not found"), "~n"],
                                    [Var])
            end,
            ?MODULE:aloop(S);

        {Prompt, pr_var, Var} ->
            {_Cur,_Max,Bs,_} = S#s.stack,
            try
                case find_var(Var, Bs) of
                    [{VarName, Val}] ->
                        case S#s.stack of
                            {_,_,_,{Mod,_Line}} ->
                                Fname = edbg:find_source(Mod),
                                {ok, Defs} = pp_record:read(Fname),
                                info_msg("~p =~n~s~n",
                                          [VarName,
                                           pp_record:print(Val, Defs)]);
                            _ ->
                                err_msg("No module info available...~n",[])
                        end;
                    [{_Var1, _Val1}|_] = Candidates ->
                        err_msg("~p ambigious prefix: ~p~n",
                                [Var, [N || {N,_} <- Candidates]]);
                    []      ->
                        att_msg("~p not found~n",[Var])
                end
            catch
                _:_ -> err_msg("Operation failed...~n",[])
            end,
            ?MODULE:aloop(S);

        {Prompt, set_break, Line} when is_integer(Line) ->
            case S#s.break_at of
                {Mod,_} ->
                    int:break(Mod, Line);
                _ ->
                    err_msg("Unknown Module; no break point set~n",[])
            end,
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, test_break, Line} when is_integer(Line) ->
            case S#s.break_at of
                {Mod,_} ->
                    t(Mod, Line);
                _ ->
                    err_msg("Unknown Module; no break point set~n",[])
            end,
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, delete_break, Line} when is_integer(Line) ->
            case S#s.break_at of
                {Mod,_} ->
                    int:delete_break(Mod, Line);
                _ ->
                    err_msg("Unknown Module; no break point deleted~n",[])
            end,
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, disable_break, Line} when is_integer(Line) ->
            case S#s.break_at of
                {Mod,_} ->
                    int:disable_break(Mod, Line);
                _ ->
                    err_msg("Unknown Module; no break point disabled~n", [])
            end,
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, enable_break, Line} when is_integer(Line) ->
            case S#s.break_at of
                {Mod,_} ->
                    int:enable_break(Mod, Line);
                _ ->
                     err_msg("Unknown Module; no break point enabled~n", [])
            end,
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, set_break, {Mod,Line}} when is_integer(Line) ->
            int:break(Mod, Line),
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, test_break, {Mod,Line}} when is_integer(Line) ->
            t(Mod,Line),
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, delete_break, {Mod,Line}} when is_integer(Line) ->
            int:delete_break(Mod, Line),
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, disable_break, {Mod,Line}} when is_integer(Line) ->
            int:disable_break(Mod, Line),
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, enable_break, {Mod,Line}} when is_integer(Line) ->
            int:enable_break(Mod, Line),
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, trace = X} ->
            Trace = S#s.trace,
            NewTrace = toggle(Trace),
            int:meta(Meta, X, NewTrace),
            ?MODULE:aloop(S#s{trace = NewTrace});

        {Prompt, quit} ->
            exit(normal);

        _X ->
            info_msg("aloop got: ~p~n",[_X]),
            ?MODULE:aloop(S)

    end.

%%
%% {ok, Tokens, _} = erl_scan:string("lists:map(fun(X) -> X+1 end, L).").
%% {ok, Exprs} = erl_parse:parse_exprs(Tokens).
%% erl_eval:add_bindings('L',[1,2,3],erl_eval:new_bindings()).
%% erl_eval:exprs(Exprs,Bs).
%%  {value,[2,3,4],[{'L',[1,2,3]}]}
%%
evaluate(ExprStr, Bindings) ->
    try
        {ok, Tokens, _} = erl_scan:string(ExprStr),
        {ok, Exprs} = erl_parse:parse_exprs(Tokens),
        {value, Value, _Bs} = erl_eval:exprs(Exprs, Bindings),
        {ok, Value}
    catch
        _:_ ->
            {error,"Failed to parse/evaluate expression"}
    end.



print_all_breakpoints(L) ->
    att_msg("~nBREAKPOINTS~n",[]),
    F = fun({{Mod,Line},[Status,Trigger,_,Cond]}) ->
                info_msg(" ~p:~p  Status=~p Trigger=~p Cond=~p~n",
                          [Mod,Line,Status,Trigger,Cond])
        end,
    [F(X) || X <- L].

save_all_breakpoints() ->
    L = int:all_breaks(),
    {ok,Fd} = file:open("breakpoints.edbg",[write]),
    try
        F = fun({{Mod,Line},[Status,Trigger,X,Cond]}) ->
                    case Cond of
                        {edbg,itest_at_break} ->
                            Cfun = get_it_break_fun(),
                            io:format(Fd,"{{~p,~p},{~p,~p,~p,~p}}.~n",
                                      [Mod,Line,Status,Trigger,X,
                                       term_to_binary(Cfun)]);
                        _ ->
                            io:format(Fd,"{{~p,~p},{~p,~p,~p,~p}}.~n",
                                      [Mod,Line,Status,Trigger,X,Cond])
                    end
            end,
        [F(Z) || Z <- L]
    after
        file:close(Fd)
    end.

lab() -> load_all_breakpoints().

load_all_breakpoints() ->
    {ok,L} = file:consult("breakpoints.edbg"),
    F = fun({{Mod,Line},{Status,Trigger,_X,Cond}})->
                case Cond of
                    Cbin when is_binary(Cbin) ->
                        Cfun = binary_to_term(Cbin),
                        it(Mod,Line,Cfun);

                    {Cmod,_} when Cmod =/= ?MODULE ->
                        i(Mod,Line),
                        ok = int:test_at_break(Mod,Line,Cond);

                    _ ->
                        i(Mod,Line)
                end,
                case {Status,Trigger} of
                    {inactive,_} -> int:disable_break(Mod,Line);
                    {_,disable}  -> int:disable_break(Mod,Line); % disable?
                    _       -> false
                end
        end,
    [F(Z) || Z <- L].

%% First try for exact match, then for a prefix match
find_var(Var, Bindings) ->
    case int:get_binding(Var, Bindings) of
        {value, Val} ->
            [{Var,Val}];
        _ ->
            VarStr = erlang:atom_to_list(Var),
            PrefixFun = fun({K,_V}) ->
                                lists:prefix(VarStr, erlang:atom_to_list(K))
                        end,
            lists:filtermap(PrefixFun, Bindings)
    end.

prompt(Pid, Apid) when is_pid(Pid), is_pid(Apid) ->
    Prompt = "("++pid_to_list(Pid)++")> ",
    ploop(Apid, Prompt, _PrevCmd = []).

ploop(Apid, Prompt, PrevCmd) ->
    %% Empty prompt repeats previous command
    Cmd = case string:tokens(io:get_line(Prompt), "\n") of
                []   -> PrevCmd;
                Cmd0 -> Cmd0
          end,

    case Cmd of
        ["a"++X] -> at(Apid, X);
        ["l"++X] -> send_list_module(Apid, X);
        ["n"++_] -> Apid ! {self(), next};
        ["s"++_] -> Apid ! {self(), step};
        ["f"++_] -> Apid ! {self(), finish};
        ["c"++_] -> Apid ! {self(), continue};
        ["m"++_] -> Apid ! {self(), messages};
        ["pr"++X] -> Apid ! {self(), pr_var, list_to_atom(string:strip(X))};
        ["p"++_] -> Apid ! {self(), processes};
        ["k"++_] -> Apid ! {self(), skip};
        ["u"++_] -> Apid ! {self(), up};
        ["q"++_] -> Apid ! {self(), quit}, exit(normal);
        ["i"++X] -> Apid ! {self(), interpret, list_to_atom(string:strip(X))};
        ["v"++X] -> Apid ! {self(), var, list_to_atom(string:strip(X))};
        ["x"++X] -> set_context(Apid, X);
        ["h"++_] -> print_help();

        ["ba"++_] -> Apid ! {self(), backtrace};
        ["br"++_] -> Apid ! {self(), breakpoints};
        ["b"++X]  -> set_break(Apid, X);

        ["te"++X] -> test_break(Apid, X);
        ["t"++_]  -> Apid ! {self(), trace};

        ["en"++X] -> ena_break(Apid, X);
        ["e"++X]  -> Apid ! {self(), eval_expr, string:strip(X)};

        ["di"++X] -> dis_break(Apid, X);
        ["de"++X] -> del_break(Apid, X);
        ["d"++_]  -> Apid ! {self(), down};

        _X ->
            info_msg("prompt got: ~p~n",[_X])
    end,
    ?MODULE:ploop(Apid, Prompt, Cmd).

at(Apid, X) ->
    try
        [Ctx] = string:tokens(string:strip(X), " "),
        Apid ! {self(), at, list_to_integer(Ctx)}
    catch
        _:_ -> Apid ! {self(), at}
    end.

print_help() ->
    S1 = " (h)elp (a)t <Ctx> (n)ext (s)tep (f)inish (c)ontinue s(k)ip",
    S2 = " (m)essages (t)oggle trace (q)uit (br)eakpoints (p)rocesses",
    S3 = " (de)lete/(di)sable/(en)able/(te)st/(b)reak <Line | Mod Line>",
    S4 = " (v)ar <variable> (e)val <expr> (i)nterpret <Mod>",
    S5 = " (pr)etty print record <variable> conte(x)t <Ctx>",
    S6 = " (u)p (d)own (l)ist module <Mod Line <Ctx>>",
    S = io_lib:format("~n~s~n~s~n~s~n~s~n~s~n~s~n", [S1,S2,S3,S4,S5,S6]),
    info_msg(help_hi(S), []).

set_context(Apid, X) ->
    case string:strip(X) of
        [] ->
            %% Empty argument resets to default context size of five rows
            Apid ! {self(), context, ?DEFAULT_CONTEXT_SIZE};

        Arg ->
            %% Try setting context size, notify user if bad input
            try
                Apid ! {self(), context, list_to_integer(Arg)}
            catch
                error:badarg ->
                    info_msg("context only takes numbers~n", [])
            end
    end.

set_break(Apid, X) ->
    send_mod_line(Apid, X, set_break).

test_break(Apid, X) ->
    send_mod_line(Apid, X, test_break).

del_break(Apid, X) ->
    send_mod_line(Apid, X, delete_break).

dis_break(Apid, X) ->
    send_mod_line(Apid, X, disable_break).

ena_break(Apid, X) ->
    send_mod_line(Apid, X, enable_break).

send_mod_line(Apid, X, Cmd) ->
    try
        case string:tokens(string:strip(X), " ") of
            [Mod,Line] ->
                Apid ! {self(), Cmd, {list_to_atom(Mod),list_to_integer(Line)}};
            [Line] ->
                Apid ! {self(), Cmd, list_to_integer(Line)}
        end,
        true
    catch
        _:_ -> false
    end.


send_list_module(Apid, X) ->
    try
        case string:tokens(string:strip(X), " ") of
            [Mod,Line,Ctx] ->
                Apid ! {self(), list_module, {list_to_atom(Mod),
                                              list_to_integer(Line),
                                              list_to_integer(Ctx)}};
            [Mod,Line] ->
                Apid ! {self(), list_module, {list_to_atom(Mod),
                                              list_to_integer(Line)}}
        end,
        true
    catch
        _:_ -> false
    end.




plist() ->
    Self = self(),
    L = [X || X = {Pid, _Func, _Status, _Info} <- int:snapshot(),
                Pid =/= Self],
    %%[{<0.33.0>,{sloop,start,[]},idle,{}},
    %% {<0.42.0>,{sloop,init,[]},break,{sloop,19}}]

    lists:map(fun({Pid, {Mod,Name,Args}, break = Status, {Mod2,Line}}) ->
                      info_msg("~10.p  ~-25.s  ~-10.s  ~p:~p~n",
                               [Pid,q(Mod,Name,length(Args)),
                                q(Status),Mod2,Line]);

                 ({Pid, {Mod,Name,Args}, exit = Status, Reason}) ->
                      if (Reason =/= normal) ->
                              info_msg("~10.p  ~-25.s  ~-10.s  Reason: ~p~n",
                                       [Pid,
                                        q(Mod,Name,length(Args)),
                                        q(Status),Reason]);
                         true ->
                              false
                      end,
                      ok;

                 ({Pid, {Mod,Name,Args}, running = Status, _}) ->
                      info_msg("~10.p  ~-25.s  ~-10.s~n",
                               [Pid,q(Mod,Name,length(Args)),q(Status)]);

                 ({Pid, {Mod,Name,Args}, idle = Status, _}) ->
                      info_msg("~10.p  ~-25.s  ~-10.s~n",
                               [Pid,q(Mod,Name,length(Args)),q(Status)]);

                 ({Pid, {Mod,Name,Args}, waiting = Status, _}) ->
                      info_msg("~10.p  ~-25.s  ~-10.s~n",
                               [Pid,q(Mod,Name,length(Args)),q(Status)])

              end, L),
    ok.

q(M,F,A) ->
    q(M)++":"++q(F)++"/"++q(A).

q(A) when is_atom(A) -> atom_to_list(A);
q(I) when is_integer(I) -> integer_to_list(I).

mlist(Mod) when is_atom(Mod) ->
    mod_list(Mod, {1,-1,-1});
%%
mlist(Pid) when is_pid(Pid) ->
    case get_break_point(Pid) of
        {_Pid, {_Mod,_Name,_Args}, break, {Mod,Line}} ->
            mlist(Mod, Line);
        Else ->
            Else
    end.

mlist(Mod, Row) when is_atom(Mod) ->
    mod_list(Mod, {1,Row,5}).

mlist(Mod, Row, Ctx) when is_atom(Mod) ->
    mod_list(Mod, {1,Row,Ctx});
mlist(P0, P1, P2) when P0 >= 0, P1 >= 0, P2 >= 0 ->
    mlist(c:pid(P0, P1, P2)).

mod_list(Mod, Rinfo)->

    %% The interpreter requires both the source code and the object code.
    %% The object code must include debug information

    Fname = find_source(Mod),
    {ok, SrcBin, Fname} = erl_prim_loader:get_file(Fname),

    put(mod_bps, int:all_breaks(Mod)),
    info_msg([c_hi("MODULE"), ": ~p.erl~n"], [Mod]),
    lists:foldl(fun(Line,{Cur,Row,Ctx}) when Cur == Row ->
                        Fs = field_size(Row,Ctx),
                        cur_line_msg("~"++Fs++".s> ~s~n",[i2l(Cur),b2l(Line)]),
                        {Cur+1,Row,Ctx};

                   (Line,{Cur,Row,Ctx}) when Row == -1 orelse
                                             (Cur >= (Row-Ctx) andalso
                                              Cur =< (Row+Ctx)) ->
                        Fs = field_size(Row,Ctx),
                        info_msg("~"++Fs++".s~s ~s~n",[i2l(Cur),sep(Cur),
                                                       b2l(Line)]),
                        {Cur+1,Row,Ctx};

                   (_Line,{Cur,Row,Ctx}) ->
                        {Cur+1,Row,Ctx}
                end,
                Rinfo,
                re:split(SrcBin,"\n")),
    ok.

field_size(Row,Ctx) ->
    integer_to_list(length(integer_to_list(Row+Ctx))).

sep(Row) ->
    case get(mod_bps) of
        [] ->
            ":";
        Bs ->
            case [X || {{_,L},_} = X <- Bs,
                       L == Row] of
                [] ->
                    ":";
                _ ->
                    c_err("*")
            end
    end.


find_source(Mod) ->
    [Fname] = [Z || {source,Z} <-
                        hd([X || {compile,X} <-
                                     apply(Mod,module_info,[])])],
    Fname.


i2l(I) when is_integer(I) -> integer_to_list(I).
b2l(B) when is_binary(B)  -> binary_to_list(B).

get_break_point(MyPid) ->
    case [X || X = {Pid, _Func, _Status, _Info} <- int:snapshot(),
               Pid == MyPid] of
        [] ->
            pid_not_found;

        [{_Pid, {_Mod,_Name,_Args}, break, {_Mod,_Line}} = Bx] ->
            Bx;

        _ ->
            no_break_found
    end.
