%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <kruskakli@gmail.com>
%%% @copyright (C) 2017, Torbjorn Tornkvist
%%% @doc edbg - Erlang/Elixir trace and debug tool
%%%
%%% `edbg' is a tty based interface to the Erlang debugger/tracer
%%% and the supervisor trees.
%%%
%%% This module is the main interface to `edbg'.
%%%
%%% Note that most of the functions here relates to the debugger
%%% functionality.
%%%
%%% For tracing you only need the functions: {@link fstart/2} ,
%%% {@link fstop/0}, {@link file/0}, {@link xfile/0}
%%% (and possibly some variants of the argument number).
%%%
%%% For the Supervisor Tree Browser, you only need:
%%% {@link suptrees/0}.
%%%
%%% @end
%%% Created : 4 Sep 2017 by Torbjorn Tornkvist <kruskakli@gmail.com>
%%%-------------------------------------------------------------------
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
         delete_breaks/0,
         delete_break/2,
         bdel/0,
         bdel/2,
         disable_breaks/0,
         disable_break/2,
         boff/0,
         boff/2,
         enable_breaks/0,
         enable_break/2,
         file/0,
         file/1,
         fhelp/0,
         fpid/1,
         fpid/2,
         fstart/0,
         fstart/1,
         fstart/2,
         fstop/0,
         bon/0,
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
         rmi/0,
         rmi/1,
         sab/0,
         save_all_breakpoints/0,
         s/1,
         s/3,
         step/1,
         step/3,
         suptrees/0,
         t/2,
         t/3,
         lts/0,
         xfile/0,
         xfile/1
        ]).

%% Internal export
-export([aloop/1,
         ploop/3,
         it_loop/1,
         itest_at_break/1,
         find_source/1
        ]).

-ifndef(ELIXIR).
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
-define(info_msg(Fmt,Args), io:format(lists:flatten(Fmt),Args)).
-define(att_msg(Fmt,Args), io:format(lists:flatten(Fmt),Args)).
-define(warn_msg(Fmt,Args), io:format(lists:flatten(Fmt),Args)).
-define(err_msg(Fmt,Args), io:format(lists:flatten(Fmt),Args)).
-define(cur_line_msg(Fmt,Args), io:format(lists:flatten(Fmt),Args)).
-define(c_hi(Str), Str).
-define(c_warn(Str), Str).
-define(c_err(Str), Str).
-define(help_hi(Str), Str).
-define(edbg_color_srv_init(), ok).
-endif.
-else.
-define(info_msg(Fmt,Args), io:format(lists:flatten(Fmt),Args)).
-define(att_msg(Fmt,Args), io:format(lists:flatten(Fmt),Args)).
-define(warn_msg(Fmt,Args), io:format(lists:flatten(Fmt),Args)).
-define(err_msg(Fmt,Args), io:format(lists:flatten(Fmt),Args)).
-define(cur_line_msg(Fmt,Args), io:format(lists:flatten(Fmt),Args)).
-define(c_hi(Str), Str).
-define(c_warn(Str), Str).
-define(c_err(Str), Str).
-define(help_hi(Str), Str).
-define(edbg_color_srv_init(), ok).
-endif.


%% @private
init_edbg() ->
    ok = ?edbg_color_srv_init().

%% @private
lts() ->
    edbg_tracer:lts().

%% @doc As 'file/1' but uses the default trace output filename.
file() ->
    edbg_tracer:file().

%% @doc Load the trace output from the file 'Fname'.
%%
%% When the file is loaded, enter the trace list mode.
%% @end
file(Fname) ->
    edbg_tracer:file(Fname).

%% @doc As 'file/0' but hint that Elixir code is traced.
xfile() ->
    edbg_tracer:xfile().

%% @doc As 'file/1' but hint that Elixir code is traced.
xfile(Fname) ->
    edbg_tracer:xfile(Fname).

%% @doc Start tracing making use of a previously stored configuration.
%%
%% A previous call to 'fstart/2' will store the configuration
%% on disk so that it can be resued when calling this function.
%%
%% @end
fstart() ->
    edbg_tracer:fstart().

%% @doc Start tracing making use of a previously stored configuration.
%%
%% A previous call to 'fstart/2' will store the configuration
%% on disk so that it can be resued when calling this function.
%%
%% @end
fstart(ModFunList) ->
    edbg_tracer:fstart(ModFunList).

%% @doc Start tracing to file.
%%
%% 'ModFunList' is a list of module names (atoms)
%% or tuples {ModuleName, FunctionName}. This makes it possible to trace
%% on all functions within a Module, or just a few functions within a Module.
%%
%% 'Opts' is a list of option tuples:
%%
%% <ul>
%%   <li>{log_file, FileName} : file where to store trace output; default: 'edbg.trace_result'</li>
%%   <li>{cfg_file, FileName} : file where to store the config; default: 'edbg_trace.config'</li>
%%   <li>{max_msgs, MaxNumOfMsgs} : max number of trace messages; default = 1000</li>
%%   <li>{trace_time, Seconds} : max time to trace; default = 10 seconds</li>
%%   <li>{trace_spec, Spec} : see the erlang:trace/3 docs; default = all</li>
%%   <li>dump_output_eager : trace output goes to file often</li>
%%   <li>dump_output_lazy : trace output goes to file not so often (default)</li>
%%   <li>monotonic_ts : show the elapsed monotonic nano seconds</li>
%%   <li>send_receive : trace send/receive messages from 'known' pids</li>
%%   <li>memory : track the memory usage of the 'known' pids</li>
%%   <li>set_on_spawn : any process created by a traced process inherit its trace flags</li>
%%   <li>set_on_first_spawn : the first process created by a traced process inherit its trace flags</li>
%%   <li>set_on_link : any process linked by a traced process inherit its trace flags</li>
%%   <li>set_on_first_link : the first process linked by a traced process inherit its trace flags</li>
%% </ul>
%%
%% Tracing in an Erlang node is setup by the 'erlang:trace/3' and
%% 'erlang:trace_pattern/3' BIF's. The generated trace output in
%% a production system can quickly produce a staggering amount of
%% data, which easily can swamp the whole system, so that it becomes
%% unusable.
%%
%% It is therefore important to restrict what to trace, the amount of
%% generated trace messages and the maximum time we allow tracing to go on.
%% 'edbg' helps you with this but you can still brake
%% your system if you are careless setting the trace parameters.
%%
%% With the `log_file` you can override the default name of the file
%% where the trace output should be stored. This can be necessary if
%% you want to specify a certain location (e.g a r/w directory/file of
%% an Elixir/Nerves device). For the same reason you can specify what
%% file the 'cfg_file' should point to, or simply turn off the config
%% file completely by setting it to `false'.
%%
%% With the `max_msgs' and `trace_time' parameters you can
%% restrict the amount of generated trace messages and running time
%% before stopping the tracing.
%%
%% The `trace_spec' is also a way of restricting what to trace on.
%% Default is `all', but for later OTP versions (> 18.3): `processes'
%% is available and would be more specific.
%% For more info about this, see the 'erlang:trace/3' documentation.
%%
%% With the `dump_output_lazy' switch set, trace output goes to file not
%% until the tracer is stopped (e.g by calling the 'file/1' function), or
%% that a limiting filter such as `max_msg' or `trace_time' is reached.
%% This is the default.
%%
%% With the `dump_output_eager' switch set, trace output goes to file often
%% which may be necessary if you run edbg tracing and the system unexpectedly
%% shuts down.
%%
%% With the `monotonic_ts' switch set, each trace message will have a
%% monotonic timestamp, in nanoseconds, attached to it. This will be displayed
%% in the call graph as the elapsed time counted from the first received
%% trace message.
%%
%% With the `send_receive' switch set, we will also trace messages sent and
%% received by 'known' pids. By 'known' pids we mean processes that we have
%% seen earlier in a traced call. The reason for this is to avoid beig swamped
%% by all the messages that the trace BIF is sending us. Note that we still
%% may get a lots of messages that will cause the resulting trace file to be
%% large and make the handling of it slower. The display of sent and received
%% messages can be toggled on/off from the trace command prompt, see also the
%% trace examples.
%%
%% With the `memory' switch set, we will also track the memory usage of
%% the processes that we get trace messages for. The memory size shown
%% is the size in bytes of the process. This includes call stack, heap,
%% and internal structures, as what we get from the process_info(Pid, memory)
%% BIF.
%%
%% NOTE: we run the process_info/2 BIF when we receive the
%% trace message from the BEAM engine so the memory size we present does
%% not exactly represent the state of the process at the creation of the
%% trace message.
%%
%% The `set_on_XXX' options probably works best together with the {@link fpid/2}
%% function.
%%
%%
%%```
%%    % Example, trace calls to the foo module, no more than 1000 trace msgs
%%    edbg:fstart([foo], [{max_msgs, 1000}]).
%%'''
%%
%%
%%```
%%    % Example, trace all modules in a particular process,
%%    % dump the traced output to file often,
%%    % no more than 1000 trace msgs.
%%    edbg:fstart([], [{trace_spec,Pid}, dump_output_eager, {max_msgs, 1000}]).
%% '''
%%
%% @end
fstart(ModFunList, Options) ->
    edbg_tracer:fstart(ModFunList, Options).

%% @doc Stop tracing and dump the trace output to file.
fstop() ->
    edbg_tracer:fstop().

%% @doc Start tracing the process: 'Pid'.
%%
%% A qick way of start tracing a process.
%%
%% The Options are set to be:
%%
%% ```
%%   [dump_output_eager,
%%    send_receive,
%%    {max_msgs, 1000000}]
%% '''
%%
%% @end
fpid(Pid) when is_pid(Pid) ->
    fpid(Pid, [dump_output_eager,
               send_receive,
               {max_msgs, 1000000}]).

%% @doc Start tracing the process: 'Pid'.
%%
%% Start tracing the process. The Options are the same as
%% for the 'fstart/2' function.
%%
%% NOTE: The `set_on_XXXX' options can be very useful here.
%% See also {@link fstart/2}
%%
%% @end
fpid(Pid, Options) when is_pid(Pid) andalso is_list(Options) ->
    edbg:fstart([], [{trace_spec,Pid} | Options]),
    io:format("~n~s ~s ~p~n",[?c_warn("<INFO>"),"Tracing on Pid:",Pid]).

%% @doc Display a short help text.
fhelp() ->
    edbg_tracer:fhelp().

%% @doc Enter the Supervisor Tree Browser.
%% @see edbg_sup_trees
suptrees() ->
    edbg_sup_trees:start().


%% @doc Show all interpreted processes.
pl() ->
 plist().

%% @doc As 'mlist/1'.
ml(X) -> mlist(X).
%% @doc As 'mlist/2'.
ml(X,Y) -> mlist(X,Y).
%% @doc As 'mlist/3'.
ml(X,Y,Z) -> mlist(X,Y,Z).

%% @doc Display all break points.
br() ->
    breaks().
%% @doc Display all break points.
breaks() ->
    print_all_breakpoints(int:all_breaks()).

%% @doc Display all break points for module 'Mod'.
br(Mod) ->
    breaks(Mod).
%% @doc Display all break points for module 'Mod'.
breaks(Mod) ->
    print_all_breakpoints(int:all_breaks(Mod)).

%% @doc Set a break point in 'Mod' at line 'Line'.
b(Mod, Line) ->
    break(Mod,Line).

%% @doc Set a break point in 'Mod' at line 'Line'.
break(Mod, Line) ->
    ok = int:break(Mod, Line),
    save_all_breakpoints(),
    ok.

%% @doc Delete all break points.
bdel() ->
    delete_breaks().

%% @doc Delete all break points.
delete_breaks() ->
    [delete_break(Mod, Line) || {{Mod, Line},_} <- int:all_breaks()].

%% @doc Delete the break point in 'Mod' on line 'Line'.
bdel(Mod, Line) ->
    delete_break(Mod, Line).
%% @doc Delete the break point in 'Mod' on line 'Line'.
delete_break(Mod, Line) ->
    ok = int:delete_break(Mod, Line),
    save_all_breakpoints(),
    ok.

%% @doc Disable all break points.
boff() ->
    disable_breaks().
%% @doc Disable all break points.
disable_breaks() ->
    [disable_break(Mod, Line) || {{Mod, Line},_} <- int:all_breaks()].

%% @doc Disable the break point in 'Mod' on line 'Line'.
boff(Mod, Line) ->
    disable_break(Mod, Line).
%% @doc Disable the break point in 'Mod' on line 'Line'.
disable_break(Mod, Line) ->
    ok = int:disable_break(Mod, Line),
    save_all_breakpoints(),
    ok.


%% @doc Disable all break points.
bon() ->
    enable_breaks().
%% @doc Disable all break points.
enable_breaks() ->
    [enable_break(Mod, Line) || {{Mod, Line},_} <- int:all_breaks()].

%% @doc Enable the break point in 'Mod' on line 'Line'.
bon(Mod, Line) ->
    enable_break(Mod, Line).
%% @doc Enable the break point in 'Mod' on line 'Line'.
enable_break(Mod, Line) ->
    ok = int:enable_break(Mod, Line),
    save_all_breakpoints(),
    ok.


%% @doc Continue the execution of the given process 'Pid'.
c(Pid) ->
    continue(Pid).

%% @doc Continue the execution of the given process &lt;P0.P1.P2&gt;.
c(P0, P1, P2) ->
    continue(c:pid(P0, P1, P2)).

%% @doc Continue the execution of the given process &lt;P0.P1.P2&gt;.
continue(P0, P1, P2) ->
    continue(c:pid(P0, P1, P2)).

%% @doc Continue the execution of the given process 'Pid'.
continue(Pid) when is_pid(Pid) ->
    int:continue(Pid).

%% FIXME doesn't work ?
%% @private
break_in(Mod, Line, Arity) ->
    int:break_in(Mod, Line, Arity).

%% @doc Start interpret the module 'Mod'.
%%
%% With only one argument, you don't set an explicit break point,
%% but you will be able to step into the module while debugging.
%% @end
i(Mod) ->
    Fname = find_source(Mod),
    int:i(Fname).

%% @doc Show all interpreted modules.
id() ->
    int:interpreted().

%% @doc Start interpret module 'Mod' and set a break point at line 'Line'.
i(Mod,Line) ->
    Fname = find_source(Mod),
    int:i(Fname),
    int:delete_break(Mod, Line),
    break(Mod, Line).

%% @doc Start interpret module 'Mod' and set a conditional break point.
%%
%% Start interpret module 'Mod' and set a conditional break point
%% in 'Mod' at line 'Line'.
%% The 'Fun/1' as an anonymous function of arity 1 that gets executed
%% each time the break point is passed. When the 'Fun/1' returns
%% 'true' the break point will trigger and the execution will stop,
%% else the execution will continue.
%%
%% The 'Fun/1' takes an argument which will be a list of current Variable
%% bindings; typically it makes use of the function
%% 'int:get_binding(Var, Bindings)' (where 'Var' is an atom denoting a
%% particular variable) to decide if the break point should trigger
%% or not. See the example further below for how to use it.
%%
%% Note that only one interactive trigger function can be used at a time.
%% @end
it(Mod,Line,Fun) when is_function(Fun)  ->
    Fname = find_source(Mod),
    int:i(Fname),
    t(Mod,Line,Fun).

%% @doc As 't/3' but will reuse an existing trigger function.
t(Mod,Line) ->
    int:delete_break(Mod, Line),
    ok = int:break(Mod, Line),
    ok = int:test_at_break(Mod, Line, {edbg,itest_at_break}),
    save_all_breakpoints(),
    ok.

%% @doc As 'it/3' but assumes 'Mod' already is interpreted.
t(Mod,Line,Fun) when is_function(Fun)  ->
    int:delete_break(Mod, Line),
    ok = itest_at_break(Fun),
    ok = int:break(Mod, Line),
    ok = int:test_at_break(Mod, Line, {edbg,itest_at_break}),
    save_all_breakpoints(),
    ok.



-define(itest_at_break, itest_at_break).

%% @private
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

%% @private
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


%% @doc Same as 'step/1'.
s(Pid) ->
    step(Pid).

%% @doc Same as 'step/3'.
s(P0, P1, P2) -> step(c:pid(P0, P1, P2)).

%% @doc Do a 'Step' debug operation of a stopped process: &gt;P0.P1.P2&lt;.
step(P0, P1, P2) -> step(c:pid(P0, P1, P2)).

%% @doc Do a 'Step' debug operation of a stopped process: 'Pid'.
step(Pid) when is_pid(Pid) ->
    int:step(Pid),
    code_list(Pid).


%% @doc Same as 'next/1'.
n(Pid) ->
    next(Pid).

%% @doc Same as 'next/3'.
n(P0, P1, P2) ->
    next(c:pid(P0, P1, P2)).

%% @doc Do a 'Next' debug operation of a stopped process: &gt;P0.P1.P2&lt;.
next(P0, P1, P2) ->
    next(c:pid(P0, P1, P2)).

%% @doc Do a 'Next' debug operation of a stopped process: 'Pid'.
next(Pid) when is_pid(Pid) ->
    int:next(Pid),
    code_list(Pid).

%% @doc Remove all interpreted modules.
rmi() ->
    lists:foreach(fun remove_interpreted_module/1, id()).

%% @doc Remove a specific interpreted modules.
rmi(Mod) ->
    remove_interpreted_module(Mod).

%% @doc Doesn't support distributed mode. See int:del_mod/2 for more.
remove_interpreted_module(Mod) ->
    dbg_iserver:safe_cast({delete, Mod}),
    erts_debug:breakpoint({Mod,'_','_'}, false),
    erlang:yield().

%% @doc Finish execution of a debugged function in process: 'Pid'.
f(Pid) ->
    finish(Pid).

%% @doc Finish execution of a debugged function in process: &lt;P0.P1.P2&gt;.
f(P0, P1, P2) ->
    finish(c:pid(P0, P1, P2)).

%% @doc Finish execution of a debugged function in process: 'Pid'.
finish(Pid) when is_pid(Pid) ->
    int:finish(Pid),
    code_list(Pid).

%% @doc Finish execution of a debugged function in process: &lt;P0.P1.P2&gt;.
finish(P0, P1, P2) ->
    finish(c:pid(P0, P1, P2)).


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

%% @doc Attach to the first process found stopped on a break point.
%%
%% Attach to an interpreted process in order to manually control
%% the further execution, inspect variables, etc. When called, you
%% will enter a sort of mini-shell where you can issue a number of
%% commands.
%% @end
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

%% @doc Attach to the given Pid.
a(Pid) ->
    attach(Pid).

%% @doc Attach to the given process: &lt;P0.P1.P2&gt; .
a(P0, P1, P2) ->
    attach(c:pid(P0, P1, P2)).

%% @doc Attach to the given process: &lt;P0.P1.P2&gt; .
attach(P0, P1, P2) ->
    attach(c:pid(P0, P1, P2)).

%% @doc Attach to the given Pid.
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
            ?err_msg("Failed to attach to process: ~p~n",[Pid]),
            Else
    end.


%% @private
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
            ?info_msg([?c_hi("Break point set at"), " ~p:~p~n"], [Mod,Line]),
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

        {Meta, {func_at, Mod, Line, Cur}} ->
            Bs = int:meta(Meta, bindings, nostack),
            mlist(Mod,Line,S#s.context),
            ?MODULE:aloop(S#s{break_at = {Mod,Line},
                              stack = {Cur,Cur,Bs,{Mod,Line}}});

        {Meta,{exit_at, {Mod, Line}, Reason, Cur}} ->
            Bs = int:meta(Meta, bindings, nostack),
            mlist(Mod,Line,S#s.context),
            ?err_msg("ERROR REASON: ~p~n",[Reason]),
            ?MODULE:aloop(S#s{break_at = {Mod,Line},
                              stack = {Cur+1,Cur+1,Bs,{Mod,Line}}});

        {Meta, {trace_output, StrFun}} ->
            ?info_msg("~s~n",[StrFun("~tp")]),
            ?MODULE:aloop(S);

        %% FROM THE PROMPTER
        {Prompt, eval_expr, Str} ->
            Bs = int:meta(Meta, bindings, nostack),
            case evaluate(Str, Bs) of
                {ok, Value} ->
                    ?info_msg([?c_hi("EVALUATED VALUE"), ":~n~p~n"], [Value]);
                {error, ErrStr} ->
                    ?err_msg("~s~n",[ErrStr])
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
                    ?att_msg("Top of stack frames!~n",[])
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
                    ?att_msg("No info available...~n",[])
            end,
            ?MODULE:aloop(S);

        {Prompt, at, Ctx} ->
            case S#s.stack of
                {_,_,_,{Mod,Line}} ->
                    mlist(Mod,Line,Ctx);
                _ ->
                    ?att_msg("No info available...~n",[])
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
            ?info_msg([?c_hi("MSGS"), ": ~p~n"] ,[Ms]),
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
            ?info_msg("BT ~p~n",[Bt]),
            ?MODULE:aloop(S);

        {Prompt, interpret, Mod} ->
            int:i(Mod),
            ?info_msg([?c_hi("Interpreted modules"), ": ~p~n"],
                     [int:interpreted()]),
            ?MODULE:aloop(S);

        {Prompt, var, Var} ->
            {_Cur,_Max,Bs,_} = S#s.stack,
            case find_var(Var, Bs) of
                [{VarName, Val}] ->
                    ?info_msg([?c_hi("~p"), "= ~p~n"], [VarName, Val]);
                [{_Var1, _Val1}|_] = Candidates ->
                    ?info_msg([?c_hi("~p"), ?c_warn(" ambigious prefix"),
                              ": ~p~n"], [Var, [N || {N,_} <- Candidates]]);
                []      -> ?info_msg([?c_hi("~p"), ?c_err(" not found"), "~n"],
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
                                ?info_msg("~p =~n~s~n",
                                          [VarName,
                                           pp_record:print(Val, Defs)]);
                            _ ->
                                ?err_msg("No module info available...~n",[])
                        end;
                    [{_Var1, _Val1}|_] = Candidates ->
                        ?err_msg("~p ambigious prefix: ~p~n",
                                [Var, [N || {N,_} <- Candidates]]);
                    []      ->
                        ?att_msg("~p not found~n",[Var])
                end
            catch
                _:_ -> ?err_msg("Operation failed...~n",[])
            end,
            ?MODULE:aloop(S);

        {Prompt, set_break, Line} when is_integer(Line) ->
            case S#s.break_at of
                {Mod,_} ->
                    int:break(Mod, Line);
                _ ->
                    ?err_msg("Unknown Module; no break point set~n",[])
            end,
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, test_break, Line} when is_integer(Line) ->
            case S#s.break_at of
                {Mod,_} ->
                    t(Mod, Line);
                _ ->
                    ?err_msg("Unknown Module; no break point set~n",[])
            end,
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, delete_break, Line} when is_integer(Line) ->
            case S#s.break_at of
                {Mod,_} ->
                    int:delete_break(Mod, Line);
                _ ->
                    ?err_msg("Unknown Module; no break point deleted~n",[])
            end,
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, disable_break, Line} when is_integer(Line) ->
            case S#s.break_at of
                {Mod,_} ->
                    int:disable_break(Mod, Line);
                _ ->
                    ?err_msg("Unknown Module; no break point disabled~n", [])
            end,
            save_all_breakpoints(),
            ?MODULE:aloop(S);

        {Prompt, enable_break, Line} when is_integer(Line) ->
            case S#s.break_at of
                {Mod,_} ->
                    int:enable_break(Mod, Line);
                _ ->
                     ?err_msg("Unknown Module; no break point enabled~n", [])
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
            ?info_msg("aloop got: ~p~n",[_X]),
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
    ?att_msg("~nBREAKPOINTS~n",[]),
    F = fun({{Mod,Line},[Status,Trigger,_,Cond]}) ->
                ?info_msg(" ~p:~p  Status=~p Trigger=~p Cond=~p~n",
                          [Mod,Line,Status,Trigger,Cond])
        end,
    [F(X) || X <- L].

%% @doc Save all current break points.
%%
%% Identical to 'save_all_breakpoints/0'.
%%
%% @end
sab() ->
    save_all_breakpoints().

%% @doc Save all current break points.
%%
%% Whenever a break point is set or modified, information is
%% stored on disk in the file 'breakpoints.edbg' .
%%
%% @end
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

%% @doc Load (previously) stored break points.
%%
%% Identical to 'load_all_breakpoints/0'.
%%
%% @end
lab() ->
    load_all_breakpoints().

%% @doc Load (previously) stored break points.
%%
%% Whenever a break point is set or modified, information is
%% stored on disk in the file 'breakpoints.edbg' . This function
%% will load and set those breakpoints found in this file.
%%
%% @end
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

%% @private
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
            ?info_msg("prompt got: ~p~n",[_X])
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
    ?info_msg(?help_hi(S), []).

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
                    ?info_msg("context only takes numbers~n", [])
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




%% @doc Show all interpreted processes.
%%
%% Show all interpreted processes and what state there are in.
%% In particular this is useful to see if a process has stopped
%% at a break point. The process identifier ('Pid') displayed in the
%% leftmost column can be used with the 'attach/1' function.
%%
%% @end
plist() ->
    Self = self(),
    L = [X || X = {Pid, _Func, _Status, _Info} <- int:snapshot(),
                Pid =/= Self],
    %%[{<0.33.0>,{sloop,start,[]},idle,{}},
    %% {<0.42.0>,{sloop,init,[]},break,{sloop,19}}]

    lists:map(fun({Pid, {Mod,Name,Args}, break = Status, {Mod2,Line}}) ->
                      ?info_msg("~10.p  ~-25.s  ~-10.s  ~p:~p~n",
                               [Pid,q(Mod,Name,length(Args)),
                                q(Status),Mod2,Line]);

                 ({Pid, {Mod,Name,Args}, exit = Status, Reason}) ->
                      if (Reason =/= normal) ->
                              ?info_msg("~10.p  ~-25.s  ~-10.s  Reason: ~p~n",
                                       [Pid,
                                        q(Mod,Name,length(Args)),
                                        q(Status),Reason]);
                         true ->
                              false
                      end,
                      ok;

                 ({Pid, {Mod,Name,Args}, running = Status, _}) ->
                      ?info_msg("~10.p  ~-25.s  ~-10.s~n",
                               [Pid,q(Mod,Name,length(Args)),q(Status)]);

                 ({Pid, {Mod,Name,Args}, idle = Status, _}) ->
                      ?info_msg("~10.p  ~-25.s  ~-10.s~n",
                               [Pid,q(Mod,Name,length(Args)),q(Status)]);

                 ({Pid, {Mod,Name,Args}, waiting = Status, _}) ->
                      ?info_msg("~10.p  ~-25.s  ~-10.s~n",
                               [Pid,q(Mod,Name,length(Args)),q(Status)])

              end, L),
    ok.

q(M,F,A) ->
    q(M)++":"++q(F)++"/"++q(A).

q(A) when is_atom(A) -> atom_to_list(A);
q(I) when is_integer(I) -> integer_to_list(I).

%% @doc List the source code, centered around a break point.
%%
%% The 'mlist/1' can also take a Module as an argument to
%% list the source ode starting from Row 1.
%% @end
mlist(Pid) when is_pid(Pid) ->
    case get_break_point(Pid) of
        {_Pid, {_Mod,_Name,_Args}, break, {Mod,Line}} ->
            mlist(Mod, Line);
        Else ->
            Else
    end;
mlist(Mod) when is_atom(Mod) ->
    mod_list(Mod, {1,-1,-1}).



%% @doc List the source code of a 'Module' centered around 'Row'.
mlist(Mod, Row) when is_atom(Mod) ->
    mod_list(Mod, {1,Row,5}).

%% @doc List the source code of a Module.
%%
%% List the source code of a 'Module', either centered around a triggered
%% break point, or around a given 'Line'. The amount of lines being display
%% around the line is controlled by the 'Contexft' value, which per default
%% is set to '5' (i.e display 5 lines above and below the line).
%%
%% Note that the listing will display the line numbers at the left border
%% of the output where breakpoints are high lighted by a '*' character
%% and the given line as '>'. However, if no line is given, the '>'
%% character will be used to denote where we currently are stopped.
%%
%% The 'mlist/3' can also take a sequence of integers for supplying a 'Pid'
%% when we should center around a break point.
%% @end
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
    ?info_msg([?c_hi("MODULE"), ": ~p.erl~n"], [Mod]),
    lists:foldl(fun(Line,{Cur,Row,Ctx}) when Cur == Row ->
                        Fs = field_size(Row,Ctx),
                        ?cur_line_msg("~"++Fs++".s> ~s~n",[i2l(Cur),b2l(Line)]),
                        {Cur+1,Row,Ctx};

                   (Line,{Cur,Row,Ctx}) when Row == -1 orelse
                                             (Cur >= (Row-Ctx) andalso
                                              Cur =< (Row+Ctx)) ->
                        Fs = field_size(Row,Ctx),
                        ?info_msg("~"++Fs++".s~s ~s~n",[i2l(Cur),sep(Cur),
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
                    ?c_err("*")
            end
    end.

%% @private
find_source(Mod) ->
    case filelib:find_source(code:which(Mod)) of
        {ok, Fname} ->
            Fname;
        {error, _} ->
            find_source_from_modinfo(Mod)
    end.
    %% [Fname] = [Z || {source,Z} <-
    %%                     hd([X || {compile,X} <-
    %%                                  apply(Mod,module_info,[])])],

%% @private
find_source_from_modinfo(Mod) ->
    Cs = Mod:module_info(compile),
    {source, Fname} = lists:keyfind(source, 1, Cs),
    Fname.

i2l(I) when is_integer(I) -> integer_to_list(I).
b2l(B) when is_binary(B)  -> binary_to_list(B).

get_break_point(MyPid) ->
    case [X || X = {Pid, _Func, _Status, _Info} <- int:snapshot(),
               Pid == MyPid] of
        [] ->
            pid_not_found;

        [{_Pid, {Mod,_Name,_Args}, break, {Mod,_Line}} = Bx] ->
            Bx;

        _ ->
            no_break_found
    end.
