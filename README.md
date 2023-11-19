# E D B G

[![hex.pm version](https://img.shields.io/hexpm/v/edbg.svg)](https://hex.pm/packages/edbg)
[![Hex.pm Downloads](https://img.shields.io/hexpm/dt/edbg.svg?style=flat-square)](https://hex.pm/packages/edbg)
[![License: Apache 2.0](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)

> A tty based interface to the Erlang debugger/tracer and a supervisor tree browser.

The purpose of `edbg` is to provide a simple and intuitive
interface to the Erlang debugger and the builtin tracing functionality.

Especially when tracing in the BEAM machine, it is easy to be swamped
by all the trace messages that are generated. `edbg` will avoid that
by presenting a clean function call graph, indented according to the call
depth. `edbg` provide means to search for a particular function call,
or an arbitrary string among the function arguments or return values;
you may then inspect the content of only those arguments you are
interested in.

`edbg` now also support [Elixir](https://github.com/etnt/edbg/wiki/Tracing#elixir-support)!

Example: The call graph

```erlang
  10:   <0.258.0> yaws_server:gserv_loop/4
  11:    <0.281.0> yaws_server:acceptor0/2
  12:     <0.281.0> yaws_server:do_accept/1
  13:      <0.259.0> yaws_server:aloop/4

```

Example: Display content of a function call argument

```erlang
  tlist> pr 177 2
  Call: yaws_server:handle_request/3 , argument 2:
  -----------------------------------
  #arg{clisock = #Port<0.6886>,
       client_ip_port = {{127,0,0,1},35871},
       headers = #headers{connection = undefined,accept = "*/*",
                          host = "localhost:8008",if_modified_since = undefined,
                          if_match = undefined,if_none_match = undefined,
  ...snip...

```
`edbg` consists of three main parts:

* [Tracing](#tracing)
* [Supervision Tree Browser](#supervisor-tree)
* [Debugger](#debugger)

If you don't like GUI's, `edbg` may be your cup of tea.

Or, for example, you work from home but still want to debug your code
at your work desktop without the hassle of forwarding a GUI,
then `edbg` is perfect.

You can see examples of how to use `edbg` in
the [wiki](https://github.com/etnt/edbg/wiki) pages.

<a name="install"></a>
## INSTALL
```
   Run: make
   Add: code:add_path("YOUR-PATH-HERE/edbg/_build/default/lib/edbg/ebin").
   Add: code:add_path("YOUR-PATH-HERE/edbg/_build/default/lib/pp_record/ebin").
     to your ~/.erlang file.
```

If you don't fancy rebar and favour plain Make + Erlc then:
```
   Run: make old
   Add: code:add_path("YOUR-PATH-HERE/edbg/ebin").
   Add: code:add_path("YOUR-PATH-HERE/edbg/deps/pp_record/ebin").
     to your ~/.erlang file.
```

A Hex package exist so to use as an Elixir Mix dependency,
add this to your list of deps:

   {:edbg, "~> 0.9.5"}

NOTE: The coloring code makes use of 'maps', so in case of an
older Erlang system where 'maps' isn't supported, you must compile
the code as:
```
   env USE_COLORS=false make
```



<a name="tracing"></a>
## Tracing

The built in tracing functionality of the BEAM is great
and there are a couple of tools that builds upon it.

What differentiates `edbg` is two things; first it runs in
the terminal with all the pros (and cons) that brings;
second it try to avoid drowning you in trace output.

Trace output can be massive and in fact sink your whole
BEAM node. `edbg` protects you from that by letting you
restrict the amount of trace messages generated and/or
the amount of time the tracing will run for. The output
will then be presented in structured way to let you
focus on what you are looking for, without having to
wade through mountains of non relevant data.

<a name="supervisor-tree"></a>
## Supervisor Tree Browser
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


<a name="debugger"></a>
## Debugger

With the `edbg` debugger interface you can make use of the Erlang
debugger in a similar way as with the GUI version.

You can set break points and then attach to a process that has
stopped at a break point. From there you can single step and do the
usual debugger operations.

What differentiates `edbg` is the fact that you are running in a 
terminal.

