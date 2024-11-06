# Home

EDBG is a tty based interface to the Erlang debugger/tracer and the supervisor trees.

If you (like me) don't like GUI's, `edbg` may be your cup of tea.

Or, for example, you work from home but still want to debug your code at your work desktop with out the hassle of forwarding a GUI, then `edbg` is perfect.

The purpose of `edbg` is to provide a simple and intuitive interface to the Erlang debugger and the builtin tracing functionality.

### Tracing

The `edbg` tracing functionality is what I myself is using all the time. The idea here is to avoid getting drowned in the (often) massive amount of trace output that the BEAM can generate. So what you will see is a list of function calls, forming a call chain, indented according to its depth in the call chain. This makes it easy to identify function calls that may be of interest for a closer study and it is then possible to display the contents of the various arguments to that particular function.

### Supervisor Tree Browser

By invoking the Supervisor Browser from the Erlang shell, you can quickly get an overview of your system by listing the running supervisors and digging out various info from them, or any linked process, as well as initiate tracing.

### The Debugger

A lot of the debugger functionality is already supported by the standard OTP int.erl module. What the edbg debugger brings is basically the tty based attached mode which makes it possible to single-step through the code as it is running.

Running the Debugger is very nice of course but in a real life system your will often run into problems with various timers expiring before you are done single-stepping. Therefore, tracing is often the better solution, where you can run your tests and when done, analyse the trace output.

### History

The idea of `edbg` runs back to the early -90's when my old friend Magnus wrote the very first Erlang debugger (interpreter). As a proof of concept I then wrote something similar to the `edbg` debugger just to verify that the Erlang debugger had a proper lower API that the GUI was built upon. That particular code of mine is since then long gone...

Fast-forward 30 years and I mentioned this to a collegue of mine and the idea of creating `edbg` was formed.
