# E D B G

A tty based interface to the Erlang debugger/tracer and supervisor tree browser.

If you don't like GUI's, `edbg` may be your cup of tea.

Or, for example, you work from home but still want to debug your code
at your work desktop without the hassle of forwarding a GUI,
then `edbg` is perfect.

The purpose of `edbg` is to provide a simple and intuitive interface to
the Erlang debugger and the builtin tracing functionality.

`edbg` consists of three main parts:

* [Tracing](#trace-usage)
* [Supervision Tree Browser](#supervisor-usage)
* [Debugger](#dbg-usage)

You can see examples of how to use `edbg` in
the [wiki](https://github.com/etnt/edbg/wiki) pages.

<a name="install"></a>
## INSTALL
If you don't fancy rebar and favour plain Make + Erlc then:
```
   Run: make old
   Add: code:add_path("YOUR-PATH-HERE/edbg/ebin").
   Add: code:add_path("YOUR-PATH-HERE/edbg/deps/pp_record/ebin").
     to your ~/.erlang file.
```


```
   Run: make
   Add: code:add_path("YOUR-PATH-HERE/edbg/_build/default/lib/edbg/ebin").
   Add: code:add_path("YOUR-PATH-HERE/edbg/_build/default/lib/pp_record/ebin").
     to your ~/.erlang file.
```

NOTE: The coloring code makes use of 'maps', so in case of an
older Erlang system where 'maps' isn't supported, you must compile
the code as:
```
   env USE_COLORS=false make
```

<a name="trace-usage"></a>
## TRACE USAGE
The `fstart` functions is using the Erlang trace BIFs.
They store the trace output on file so that it survives
the initial session where the tracing took place (which can
be good if called from within a Common-Test test case or similar).

### edbg:fstart(ModFunList, Opts)
Start tracing to file. `ModFunList` is a list of module names (atoms)
or tuples {ModuleName, FunctionName}. This makes it possible to trace
on all functions within a Module, or just a few functions within a Module.

`Opts` is a list of option tuples:

* {log_file, FileName} : file where to store trace output; default: `edbg.trace_result`
* {max_msgs, MaxNumOfMsgs} : max number of trace messages; default = 1000
* {trace_time, Seconds} : max time to trace; default = 10 seconds
* {trace_spec, Spec} : see the erlang:trace/3 docs; default = all
* dump_output_eager : trace output goes to file often
* dump_output_lazy : trace output goes to file not so often (default)
* monotonic_ts : show the elapsed monotonic nano seconds
* send_receive : trace send/receive messages from 'known' pids
* memory : track the memory usage of the 'known' pids

Tracing in an Erlang node is setup by the `erlang:trace/3` and
`erlang:trace_pattern/3` BIFs`. The generated trace output in
a production system can quickly produce a staggering amount of
data, which easily can swamp the whole system, so that it becomes
unusable.

It is therefore important to restrict what to trace, the amount of
generated trace messages and the maximum time we allow tracing to go on.
`edbg` helps you with this but you can still brake
your system if you are careless setting the trace parameters.

With the `max_msgs` and `trace_time` parameters you can
restrict the amount of generated trace messages and running time
before stopping the tracing.

The `trace_spec` is also a way of restricting what to trace on.
Default is `all`, but for later OTP versions (> 18.3): `processes`
is available and would be more specific.
For more info about this, see the erlang:trace/3 documentation.

With the `dump_output_lazy` switch set, trace output goes to file not
until the tracer is stopped (e.g by calling the file/1 function), or
that a limiting filter such as max_msg or trace_time is reached.
This is the default.

With the `dump_output_eager` switch set, trace output goes to file often
which may be necessary if you run edbg tracing and the system goes down.

With the `monotonic_ts` switch set, each trace message will have a
monotonic timestamp, in nanoseconds, attached to it. This will be displayed
in the call graph as the elapsed time counted from the first received
trace message.

With the `send_receive` switch set, we will also trace messages sent and
received by 'known' pids. By 'known' pids we mean processes that we have
seen earlier in a traced call. The reason for this is to avoid beig swamped
by all the messages that the trace BIF is sending us. Note that we still
may get a lots of messages that will cause the resulting trace file to be
large and make the handling of it slower. The display of sent and received
messages can be toggled on/off from the trace command prompt, see also the
trace examples.

With the `memory` switch set, we will also track the memory usage of
the processes that we get trace messages for. The memory size shown
is the size in bytes of the process. This includes call stack, heap,
and internal structures, as what we get from the process_info(Pid, memory)
BIF. NOTE: we run the process_info/2 BIF when we receive the
trace message from the BEAM engine so the memory size we present does
not exactly represent the state of the process at the creation of the
trace message.


```erlang
   % Example, trace calls to the foo module, no more than 1000 trace msgs
   edbg:fstart([foo], [{max_msgs, 1000}]).
```

```erlang
   % Example, trace all modules in a particular process,
   % dump the traced output to file often,
   % no more than 1000 trace msgs.
   edbg:fstart([], [{trace_spec,Pid}, dump_output_eager, {max_msgs, 1000}]).
```

### edbg:fstart(ModFunList)
As edbg:fstart/2 but using no Options.

### edbg:fstart()
Resuse the last setup from edbg:fstart/[1,2].

### edbg:file(LogFileName)
Load the trace output from the file `LogFileName` and
enter the trace list mode.

### edbg:file()
As edbg:file/1 but use the default trace output filename.

### edbg:fstop()
Stop tracing and dump the trace output to file.

### edbg:fhelp()
Prints the available options.


<a name="supervisor-usage"></a>
## SUPERVISION USAGE
By invoking the `edbg:suptrees()` function from the Erlang shell,
you will enter the supervision tree browser; a way to quickly
get an overview of your system by listing the running supervisors.

The browser makes it possible to browse through the tree of supervisors
as well as any linked processes. At any point, a process can have its
(default) process-info data printed as well as its backtrace.

If the worker process is of type gen_server, gen_event or gen_statem,
it is possible to pretty-print the State of the callback module that
the worker process is maintaining.

It is also possible to setup a process monitor on any process in order
to get a notification printed if the process should terminate.

Note that the (edbg) tracing is built-in; i.e you can start tracing 
on any process you can access from the supervision browser.
This will trace all modules that are executing within the process
as well as any messages sent/received from/to the process.


<a name="dbg-usage"></a>
## DEBUGGER USAGE

### edbg:i(Mod, Line) | edbg:i(Mod)
Start interpret module `Mod` and set a break point at line `Line`.
With only one argument, you don't set an explicit break point,
but you will be able to step into the module while debugging.

### edbg:b(Mod, Line) | edbg:break(Mod, Line)
Set a break point in `Mod` at line `Line`.

### edbg:it(Mod, Line, Fun)
Start interpret module `Mod` and set a conditional break point
 in `Mod` at line `Line`.
The `Fun/1` as an anonymous function of arity 1 that gets executed
each time the break point is passed. When the `Fun/1` returns
`true` the break point will trigger and the execution will stop,
else the execution will continue.

The `Fun/1` takes an argument which will be a list of current Variable
bindings; typically it makes use of the function
`int:get_binding(Var, Bindings)` (where `Var` is an atom denoting a
particular variable) to decide if the break point should trigger
or not. See the example further below for how to use it.

Note that only one interactive trigger function can be used at a time.

### edbg:t(Mod, Line) | edbg:t(Mod, Line, Fun)
Works at `edbg:it/3` but assumes that the module alread is being
interpreted.

The `t/2` function will reuse an already existing trigger function.

### edbg:pl() | edbg:plist()
Show all interpreted processes and what state there are in.
In particular this is useful to see if a process is has stopped
in a break point. The process identifier (Pid) displayed in the
leftmost column can be used with the `edbg:attach/1` function.

### edbg:a(Pid) | edbg:attach(Pid)
### edbg:a(P0, P1, P2) | edbg:attach(P0, P1, P2)
### edbg:a()
Attach to an interpreted process in order to manually control
the further execution, inspect variables, etc. When called, you
will enter a sort of mini-shell where you can issue a number of
commands; see the examples below.

The `a/0` function is a shortcut to attach to the first process found
that has stopped at a break point.

The `a/3` and `attach/3` functions are shorthands for supplying a pid as a
sequence of integers.

### edbg:mlist(Mod) | edbg:mlist(Pid) | edbg:mlist(Mod, Line) | edbg:mlist(Mod, Line, Context) | edbg:mlist(P0, P1, P2)
### edbg:ml(Mod) | edbg:ml(Pid) | edbg:ml(Mod, Line) | edbg:ml(Mod, Line, Context)
List the source code of a Module, either centered around a triggered
break point, or around a given `Line`. The amount of lines being display
around the line is controled by the `Context` value, which per default
is set to `5` (i.e display 5 lines above and below the line).

Note that the listing will display the line numbers at the left border
of the output where breakpoints are high lighted by a `*` character
and the given line as `>`. However, if no line is given, the `>`
character will be used to denote where we currently are stopped.

The `mlist/3` can take a sequence of integers for supplying a pid.

### edbg:br() | edbg:breaks()
Display all break points.

### edbg:br(Mod) | edbg:breaks(Mod)
Display the break points in module `Mod`.

### edbg:s(Pid) | edbg:step(Pid)
### edbg:s(P0, P1, P2) | edbg:step(P0, P1, P2)
Step the execution of a stopped process.

### edbg:n(Pid) | edbg:next(Pid)
### edbg:n(P0, P1, P2) | edbg:next(P0, P1, P2)
Next the execution of a stopped process.

### edbg:c(Pid) | edbg:continue(Pid)
### edbg:c(P0, P1, P2) | edbg:continue(P0, P1, P2)
Continue the execution of a stopped process.

### edbg:id()
Show all interpreted modules.

### edbg:delete_break(Mod,Line) | edbg:disable_break(Mod,Line) | edbg:enable_break(Mod,Line)
### edbg:bdel(Mod,Line) | edbg:boff(Mod,Line) | edbg:bon(Mod,Line)
Delete/Disable/Enable a break point.

### edbg:load_all_breakpoints() | edbg:lab()
Whenever a break point is set or modified, information is
stored on disk in the file `breakpoints.edbg`. This function
will load and set those breakpoints found in this file.


