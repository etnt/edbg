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
         sup_trees = []
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
         modules = []
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
    State = show(State0),
    print_help(),
    loop(Pid, prompt(), State).


loop(Pid, Prompt, State0) ->
    io:format("~n",[]),
    State =
        case string:tokens(io:get_line(Prompt), "\n") of
            ["h"++X] -> help(X, State0);
            ["d"++X] -> show(X, State0);
            ["x"++X] -> expand(X, State0);
            ["s"++X] -> shrink(X, State0);
            ["p"++X] -> pinfo(X, State0);
            ["b"++X] -> backtrace(X, State0);
            ["m"++X] -> setup_monitor(X, State0);
            ["r"++_] -> refresh();
            ["q"++_] -> Pid ! quit, exit(normal);

            _X ->
                ?info_msg("prompt got: ~p~n",[_X]),
                State0
        end,
    ?MODULE:loop(Pid, Prompt, State).

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
                        modules = Modules}|Tail], Indent) ->
    Pad = indent(Indent),
    io:format("~p:~s ~s~p ~p ~p~n", [N, ?c_warn("W"), Pad, Id, Pid, Modules]),
    display(Chars, Tail, Indent);
display(_, [], _) ->
    ok.

indent(N) ->
    string:copies(" ", N).

help(_, State) ->
    print_help(),
    State.


get_pid(#sup_tree{pid = Pid}) -> Pid;
get_pid(#worker{pid = Pid})   -> Pid;
get_pid(Pid) when is_pid(Pid) -> Pid.


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
                io:format("~n~s ~p~n", [?c_warn("Monitoring:"),Pid]),
                receive
                    {'DOWN', ReqId, process, Pid, ExitReason} ->
                        io:format("~n~s ~p , Reason: ~p~n",
                                  [?c_err("Monitor got DOWN from:"),
                                   Pid, ExitReason])
                end
        end,
    erlang:spawn(F).



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
            show(State)
    end.

shrink(Chars, #state{sup_trees = SupTrees0} = State) ->
    try
        I = parse_int(Chars),
        SupTrees = set_expand(SupTrees0, I, false),
        show(State#state{sup_trees = SupTrees})
    catch
        _:_ ->
            show(State)
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
    S1 = " (h)elp e(x)pand [<N>] (s)hrink [<N>]",
    S2 = " (p)rocess info [<N> [<M>]] (b)acktrace [<N> [<M>]]",
    S3 = " (m)onitor [<N> [<M>]]",
    S4 = " (r)efresh (q)uit",
    S = io_lib:format("~n~s~n~s~n~s~n~s~n",[S1,S2,S3,S4]),
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
                                Entry = #worker{id = Id, pid = Pid, modules = Modules},
                                Cs = AccTree#sup_tree.children,
                                AccTree#sup_tree{children = Cs ++ [Entry]};
                           (Entry, AccTree) ->
                                ?warn_msg("Ignoring: ~p~n",[Entry]),
                                AccTree
                        end, SupTree, Children);
        _ ->
            SupTree
    end.

%% Try to find all Supervisors in the system.
%% Idea: Go through all registered processes and
%% try to figure out if they are a supervisor.
%% Is there a better way?
%% Can a supervisor not be registered and hence not be included here?
all_sup_trees() ->
    Sups = lists:foldr(
             fun(Id,Acc) ->
                     case process_info(whereis(Id), dictionary) of
                         {dictionary,L} ->
                             case lists:keyfind('$initial_call', 1, L) of
                                 {'$initial_call',{supervisor,_,_}} ->
                                     [Id|Acc];
                                 _ ->
                                     Acc
                             end;
                         _ ->
                             Acc
                     end
             end, [], lists:sort(erlang:registered())),
    [supervisors(Id) || Id <- Sups].

    %% SupTree#sup_tree{children =
    %%                      [supervisors(Id,  new_sup_tree(Id, Pid)) ||
    %%                          {Id, Pid, Type, _Modules}
    %%                              <- supervisor:which_children(SupTree#sup_tree.pid),
    %%                          Type == supervisor,
    %%                          is_pid(Pid) == true]}.


new_sup_tree(SupervisorId) ->
    new_sup_tree(SupervisorId, erlang:whereis(SupervisorId)).

new_sup_tree(SupervisorId, Pid) ->
    #sup_tree{id = SupervisorId,
              pid = Pid}.
