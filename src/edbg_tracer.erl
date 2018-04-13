-module(edbg_tracer).

-export([file/1,
         start_my_tracer/0,
         send/2,
         tlist/0,
         tmax/1,
         traw/1,
         tquit/0,
         lts/0,
         tstart/0,
         tstart/1,
         tstart/2,
         tstart/3,
         tstop/0
        ]).

%% Internal export
-export([tloop/3,
         ploop/1,
         rloop/2
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

-define(mytracer, mytracer).

-define(inside(At,Cur,Page), ((Cur >=At) andalso (Cur =< (At+Page)))).

-record(tlist, {
          level = 0,
          at = 1,
          current = 1,
          page = 20
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

%% dbg:tracer(process,{fun(Trace,N) ->
%%                        io:format("TRACE (#~p): ~p~n",[N,Trace]),
%%                        N+1
%%                       end, 0}).
%%dbg:p(all,clear).
%%dbg:p(all,[c]).

file(Fname) ->
    case file:read_file(Fname) of
        {ok, Tdata} ->
            %% We expect Tdata to be a list of trace tuples as
            %% a binary in the external term form.
            call(start_my_tracer(), {load_trace_data, binary_to_term(Tdata)});
        Error ->
            Error
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
        ["d"++_] -> ?mytracer ! down;
        ["u"++_] -> ?mytracer ! up;
        ["t"++_] -> ?mytracer ! top;
        ["b"++_] -> ?mytracer ! bottom;
        ["a"++X] -> at(?mytracer, X);
        ["f"++X] -> find(?mytracer, X);
        ["s"++X] -> show(?mytracer, X);
        ["r"++X] -> show_return(?mytracer, X);
        ["w"++X] -> show_raw(?mytracer, X);
        ["pr"++X]-> show_record(?mytracer, X);
        ["p"++X] -> set_page(?mytracer, X);
        ["h"++_] -> print_help();
        ["q"++_] -> Pid ! quit, exit(normal);

        _X ->
            info_msg("prompt got: ~p~n",[_X])
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
    S5 = " (p)agesize <N> (q)uit",
    S = io_lib:format("~n~s~n~s~n~s~n~s~n~s~n",[S1,S2,S3,S4,S5]),
    info_msg(help_hi(S), []).


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
                    info_msg("~n~p~n", [Msg]);
                _ ->
                    err_msg("not found~n",[])
            end,
            ?MODULE:tloop(X, Tlist ,Buf);

        {show_return, N} ->
            dbg:stop_clear(),
            case get_return_value(N, lists:reverse(Buf)) of
                {ok, {M,F,Alen}, RetVal} ->
                    Sep = pad(35, $-),
                    info_msg("~nCall: ~p:~p/~p , return value:~n~s~n~p~n",
                             [M,F,Alen,Sep,RetVal]);
                not_found ->
                    info_msg("~nNo return value found!~n",[])
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
                    {_,{trace, _Pid, call, {M,F,A}}} ->
                        Sep = pad(35, $-),
                        ArgStr = "argument "++integer_to_list(ArgN)++":",
                        info_msg("~nCall: ~p:~p/~p , ~s~n~s~n~p~n",
                                 [M,F,length(A),ArgStr,Sep,lists:nth(ArgN,A)]);
                    _ ->
                        err_msg("not found~n",[])
                end
            catch
                _:_ ->  err_msg("not found~n",[])
            end,
            ?MODULE:tloop(X, Tlist ,Buf);

        {show_record, N, ArgN} ->
            dbg:stop_clear(),
            try
                case lists:keyfind(N, 1, Buf) of
                    {_,{trace, _Pid, call, {M,F,A}}} ->
                        Sep = pad(35, $-),
                        Fname = edbg:find_source(M),
                        {ok, Defs} = pp_record:read(Fname),
                        ArgStr = "argument "++integer_to_list(ArgN)++":",
                        info_msg("~nCall: ~p:~p/~p , ~s~n~s~n~s~n",
                                 [M,F,length(A),ArgStr,Sep,
                                  pp_record:print(lists:nth(ArgN,A), Defs)]);
                    _ ->
                        err_msg("not found~n",[])
                end
            catch
                _:_ ->  err_msg("not found~n",[])
            end,
            ?MODULE:tloop(X, Tlist ,Buf);

        %% Find a matching function call
        {find, {Mstr,Fstr}} ->
            NewTlist = case find_mf(Tlist#tlist.at, Buf, Mstr, Fstr) of
                           not_found ->
                               info_msg("not found~n",[]),
                               Tlist;
                           NewAt ->
                               list_trace(Tlist#tlist{at = NewAt}, Buf)
                       end,
            ?MODULE:tloop(X, NewTlist ,Buf);

        %% Find a matching function call where ArgN contains Value
        {find, {Mstr,Fstr},{An,Av}} ->
            NewTlist = case find_mf_av(Tlist#tlist.at,Buf,Mstr,Fstr,An,Av) of
                           not_found ->
                               info_msg("not found~n",[]),
                               Tlist;
                           NewAt ->
                               list_trace(Tlist#tlist{at = NewAt}, Buf)
                       end,
            ?MODULE:tloop(X, NewTlist ,Buf);

        %% Find a match among the return values
        {find_str, Str} ->
            NewTlist = case find_retval(Tlist#tlist.at, Buf, Str) of
                           not_found ->
                               info_msg("not found~n",[]),
                               Tlist;
                           NewAt ->
                               list_trace(Tlist#tlist{at = NewAt}, Buf)
                       end,
            ?MODULE:tloop(X, NewTlist ,Buf);

        top ->
            NewTlist = list_trace(Tlist#tlist{at = 1}, Buf),
            ?MODULE:tloop(X, NewTlist, Buf);

        bottom ->
            {N,_} = hd(Buf),
            NewTlist = list_trace(Tlist#tlist{at = N}, Buf),
            ?MODULE:tloop(X, NewTlist, Buf);

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
                {_,V} -> info_msg("~p~n",[V]);
                _     -> info_msg("nothing found!~n",[])
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
            %%info_msg("mytracer got: ~p~n",[_X]),
            ?MODULE:tloop(X, Tlist ,Buf)
    end.


find_mf(At, Buf, Mstr, Fstr) ->
    Mod = list_to_atom(Mstr),
    %% First get the set of trace messages to investigate
    L = lists:takewhile(
          fun({N,_}) when N>=At -> true;
             (_)                -> false
          end, Buf),
    %% Discard non-matching calls
    R = lists:dropwhile(
          fun({_N,{trace,_Pid,call,{M,_,_}}}) when M == Mod andalso
                                                   Fstr == "" ->
                  false;
             ({_N,{trace,_Pid,call,{M,F,_}}}) when M == Mod ->
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
          fun({_N,{trace,_Pid,call,{M,F,A}}}) when M == Mod andalso
                                                   length(A) >= An ->
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
                  end;
             (_) ->
                  true
          end, lists:reverse(L)),
    case R of
        [{N,_}|_] -> N;
        _         -> not_found
    end.

find_retval(At, Buf, Str) ->
    %% First get the set of trace messages to investigate
    L = lists:takewhile(
          fun({N,_}) when N>=At -> true;
             (_)                -> false
          end, Buf),
    %% Discard non-matching return values
    try
        lists:foldl(
          fun({_N,{trace,_Pid,return_from, _MFA, Value}}=X,Acc) ->
                  ValStr = lists:flatten(io_lib:format("~p",[Value])),
                  try re:run(ValStr, Str) of
                      nomatch -> [X|Acc];
                      _       -> find_matching_call(Acc, 0)
                  catch
                      _:_ -> [X|Acc]
                  end;
             (X, Acc) ->
                  [X|Acc]
          end, [], lists:reverse(L)),
        not_found
    catch
        throw:{matching_call,{N,_}} -> N;
        _:_                         -> not_found
    end.

%% Will throw exception at success; crash if nothing is found!
find_matching_call([{_N,Trace}=X|_], 0) when element(3, Trace) == call ->
    throw({matching_call,X});
find_matching_call([{_N,Trace}|L], N) when element(3, Trace) == call ->
    find_matching_call(L, N-1);
find_matching_call([{_N,Trace}|L], N) when element(3, Trace) == return_from ->
    find_matching_call(L, N+1).


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
    Fs = field_size(Buf),
    Zlist =
        lists:foldr(

          %% C A L L
          fun({N,{trace, Pid, call, {M,F,A}}},
              #tlist{level = Level,
                     at = At,
                     page = Page} = Z)
                when ?inside(At,N,Page) ->
                  info_msg("~"++Fs++".s:~s ~p ~p:~p/~p~n",
                           [integer_to_list(N),pad(Level),Pid,M,F,length(A)]),
                  Z#tlist{level = Level+1};

             ({_N,{trace, _Pid, call, {_M,_F,_A}}},
              #tlist{level = Level} = Z) ->
                  Z#tlist{level = Level+1};

             %% R E T U R N _ F R O M
             ({_N,{trace, _Pid, return_from, _MFA, _Value}},
              #tlist{level = Level} = Z) ->
                  Z#tlist{level = erlang:max(Level-1,0)}

          end, Tlist#tlist{level = 0}, Buf),

    NewAt = Tlist#tlist.at + Tlist#tlist.page + 1,
    Zlist#tlist{at = NewAt}.


get_return_value(N, [{I,_}|T]) when I < N ->
    get_return_value(N, T);
get_return_value(N, [{N,{trace, _Pid, call, {M,F,A}}}|T]) ->
    find_return_value({M,F,length(A)}, T);
get_return_value(N, [{I,_}|_]) when I > N ->
    not_found;
get_return_value(_, []) ->
    not_found.

find_return_value(MFA, T) ->
    find_return_value(MFA, T, 0).

find_return_value(MFA, [{_,{trace,_Pid,return_from,MFA,Val}}|_], 0 = _Depth) ->
    {ok, MFA, Val};
find_return_value(MFA, [{_,{trace,_Pid,return_from,MFA,_}}|T], Depth)
  when Depth > 0 ->
    find_return_value(MFA, T, Depth-1);
find_return_value(MFA, [{_,{trace, _Pid, call, MFA}}|T], Depth) ->
    find_return_value(MFA, T, Depth+1);
find_return_value(MFA, [_|T], Depth) ->
    find_return_value(MFA, T, Depth);
find_return_value(_MFA, [], _Depth) ->
    not_found.


mlist(N, Buf) ->
    try
        case lists:keyfind(N, 1, Buf) of
            {_,{trace, _Pid, call, {M,F,A}}} ->
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
                        info_msg("~nCall: ~p:~p/~p~n~s~n"++FmtStr++"~n~s~n",
                                 [M,F,length(A),Sep|Args]++[Sep]);
                    Else ->
                        info_msg("nomatch: ~p~n",[Else])
                end;
            _ ->
                info_msg("not found~n",[])
        end
    catch
        _:Err ->
            info_msg([c_err("CRASH: ~p"), " ~p~n"],
                     [Err,erlang:get_stacktrace()])
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
