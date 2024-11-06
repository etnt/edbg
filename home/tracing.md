# Tracing

The function `edbg:fstart/2` takes one argument containing the list of modules we want to trace, and a second argument containing various options. The trace output will be stored on file.

So in the example below we want to trace on three modules (yaws\_server,yaws,yaws\_config) from the Yaws webserver. With the `max_msgs` option we restrict the allowed number of trace messages to 10000.

```
1> edbg:fstart([yaws_server,yaws,yaws_config],[{max_msgs,10000}]).
ok
```

Now run some traffic toward yaws and when done, stop the tracing:

```
2> edbg:fstop().
ok
```

Here we rely on using the default filename for storing the trace output, hence we don't have to specify it here when loading the trace info to be displayed.

```
3> edbg:file().

(h)elp (a)t [<N>] (d)own (u)p (t)op (b)ottom
(s)how <N> [<ArgN>] (r)etval <N> ra(w) <N>
(pr)etty print record <N> <ArgN>
(f)ind <RegExp> [<ArgN> <ArgRegExp>] | ~r<RegExp>
(fp) following process (up) unfollowing process
(on)/(off) send_receive | memory
(p)agesize <N> (q)uit
(set) <Var> <N> [<ArgN>]  (let) <Var> <Expr>
(eval) <Expr>  (xall/xnall) <Mod>
```

As can be seen, we first get a compact help text showing what commands we can use. We then get the beginning of the trace output. Each line is prefixed with a number that we use for reference. The indentation shows the depth of the call chain. Beware of the process identifier which shows which process that were executing a particular function call.

```
 0: <0.255.0> yaws_server:gserv_loop/4
 1:  <0.258.0> yaws_server:gserv_loop/4
 2:   <0.233.0> yaws:month/1
 4:   <0.259.0> yaws_server:peername/2
 6:   <0.258.0> yaws_server:close_accepted_if_max/2
 8:   <0.258.0> yaws_server:acceptor/1
10:   <0.258.0> yaws_server:gserv_loop/4
11:    <0.281.0> yaws_server:acceptor0/2
12:     <0.281.0> yaws_server:do_accept/1
13:      <0.259.0> yaws_server:aloop/4
14:       <0.259.0> yaws_server:init_db/0
16:       <0.259.0> yaws:http_get_headers/2
17:        <0.259.0> yaws:do_http_get_headers/2
18:         <0.259.0> yaws:http_recv_request/2
19:          <0.259.0> yaws:setopts/3
tlist>
```

As you can see, we get a pretty output where we can follow the chain of execution without drowning in output which would be the case if we should have displayed the contents of all the arguments to the functions.

Instead, we can now inspect a particular call of interest (line 4). We use the `(s)how` command to display the function clause heads in order to help us decide which argument to inspect.

```
tlist> s 4

Call: yaws_server:peername/2
-----------------------------------

peername(CliSock, ssl) ->

peername(CliSock, nossl) ->

-----------------------------------
```

To show what the second argument contained, we add `2` to the `show` command:

```
tlist> s 4 2

Call: yaws_server:peername/2 , argument 2:
-----------------------------------
nossl
```

Apparently it contained the atom `nossl`.

We can also see what the function returned:

```
tlist> r 4

Call: yaws_server:peername/2 , return value:
-----------------------------------
{{127,0,0,1},35871}
```

If we know the argument to be a record, we can pretty print it. First we find what argument we are interested in:

```
tlist> s 177

Call: yaws_server:handle_request/3
-----------------------------------

handle_request(CliSock, ARG, _N)
  when is_record(ARG#arg.state, rewrite_response) ->

handle_request(CliSock, ARG, N) ->

-----------------------------------
```

Ok, we want to pretty print the second argument, the `#arg{}` record:

```
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

To display (again) the function call chain, you use the `a(t)` command. With no arguments it just re-display the trace output. If you want to go to a particular line you just give that as an argument. Example, go to line 10 in the example above:

```
tlist> a 10
10:   <0.258.0> yaws_server:gserv_loop/4
11:    <0.281.0> yaws_server:acceptor0/2
12:     <0.281.0> yaws_server:do_accept/1
13:      <0.259.0> yaws_server:aloop/4
 ...snip...
 
```

To change the number of lines shown of the trace output. Set it with the `p(age)` command. Example, display (roughly) 50 lines:

```
tlist> p 50
```

The trace output can be huge so we can search for a particular function call that we are interested in. Note that you can specify a RegExp for searching among the Mod:Fun calls.

```
tlist> f yaws:decode_b
32:           <0.537.0> yaws:decode_base64/1
33:            <0.537.0> yaws:decode_base64/2
34:             <0.537.0> yaws:d/1
  ...snip...
```

We can also search in a particular argument of a particular function call. Here the 2'nd argument of yaws:setopts should contain 'packet\_size':

```
tlist> f yaws:setopts 2 packet_size
22:         <0.537.0> yaws:setopts/3
24:         <0.537.0> yaws:do_recv/3
26:         <0.537.0> yaws:http_collect_headers/5
...snip...

tlist> s 22 2

Call: yaws:setopts/3 , argument 2:
-----------------------------------
[{packet,httph},{packet_size,16384}]
```

To search among the return values we prefix our search string with the `fr` (Find Return value):

```
tlist> fr GET
184:           <0.537.0> yaws:make_allow_header/1
187:          <0.537.0> yaws_server:deliver_accumulated/1
188:           <0.537.0> yaws:outh_get_content_encoding/0
190:           <0.537.0> yaws:outh_set_content_encoding/1
 ...snip...

tlist> r 184

Call: yaws:make_allow_header/1 , return value:
-----------------------------------
["Allow: GET, POST, OPTIONS, HEAD\r\n"]
```

### Follow/Unfollow a Process

You can filter out trace messages shown for a particular process by using the `fp` (follow process) and `up` (unfollow process) commands. The process followed is the process shown on the line you have specified with the `at` command.

Example:

```
253:            <0.931.0> foo_session:connect/8
254:             <0.931.0> foo:new/6
256: <0.247.0> bar:add_stuff/2
257:  <0.247.0> bar:'-add_stuff/2-fun-1-'/1
tlist> fp
following process: <0.931.0>
tlist> a
253:            <0.931.0> foo_session:connect/8
254:             <0.931.0> foo:new/6
266:              <0.931.0> foo:make_progress/1
299:              <0.931.0> foo:'-make_progress_funs/1-fun-0-'/4
```

### edbg:fhelp()

To get a short help summary from the Erlang shell, call: `edbg:fhelp()` Example:

```
1> edbg:fhelp().

edbg:fstart(ModFunList, Opts)
edbg:fstop()
edbg:file()
edbg:file(FileName)
edbg:fpid(Pid)
edbg:fpid(Pid, Opts)

ModFunList is a list of module names (atoms) or tuples {ModName, FunName}.

Opts is a list of option tuples:

{log_file, FileName}     : file where to store trace output
                           default is: edbg.trace_result
{cfg_file, FileName}     : file where to store config
                           default is: edbg_trace.config
                           use 'false' to turn it off
{max_msgs, MaxNumOfMsgs} : max number of trace messages
                           default = 1000
{trace_time, Seconds}    : max time to trace
                           default = 10 seconds
{trace_spec, Spec}       : see the erlang:trace/3 docs
                           default = all
dump_output_eager        : trace output goes to file often
dump_output_lazy         : trace output goes to file when done (default)
monotonic_ts             : show the elapsed monotonic nano seconds
send_receive             : trace send/receive messages from 'known' pids
memory                   : track the memory usage of the 'known' pids
```

We will now go through the various options more in detail.

### The monotonic\_ts Option

By using the option `monotonic_ts`, we will also see the elapsed monotonic time, in nano seconds, from the first received trace message.

```
1> edbg:fstart([yaws_server],[{max_msgs,10000},
                              monotonic_ts]).
```

The trace output will now have a timestamp appended to each function call.

```
0: <0.297.0> yaws_server:peername/2 - 0
2: <0.296.0> yaws_server:close_accepted_if_max/2 - 37276
4: <0.296.0> yaws_server:acceptor/1 - 46436
6: <0.296.0> yaws_server:gserv_loop/4 - 72287
7: <0.315.0> yaws_server:acceptor0/2 - 104797
8:  <0.315.0> yaws_server:do_accept/1 - 110475
9: <0.297.0> yaws_server:aloop/4 - 142479
```

### The send\_receive Option

By using the option \`send\_receive\`\`, we will also see sent and received messages by any "known" pid. With "known" pid we mean any process for which we have received any previous trace info.

```
1> edbg:fstart([yaws_server],[{max_msgs,10000},
                              send_receive]).
```

Messages will now be shown, inlined in the output in the order the trace info is received. The `<<<` marker indicates an incoming message to a particular process and `>>>` indicates an outgoing message from a particular process to a (another) process.

```
  0: <0.651.0> yaws_server:gserv_loop/4
  1:  <<< Receive(<0.651.0>)  timeout...
  2:  <0.651.0> yaws_server:gserv_loop/4
  3:   <<< Receive(<0.651.0>)  {<0.652.0>,next,{ok,...
  4: <0.652.0> yaws_server:peername/2
  6: >>> Send(<0.652.0>) -> To(<0.626.0>)  {'$gen_call',{<0.652...
  7: <<< Receive(<0.652.0>)  {#Ref<0.733272738.27...
  8: <0.652.0> yaws_server:aloop/4
  9:   <0.651.0> yaws_server:close_accepted_if_max/2
 11:   <0.651.0> yaws_server:acceptor/1
 13:   <0.651.0> yaws_server:gserv_loop/4
 
```

The shown messages are truncated for brevity. We can show the full content of the message:

```
 tlist> s 6

Message sent by: <0.652.0>  to: <0.626.0>
{'$gen_call',{<0.652.0>,#Ref<0.733272738.2716598273.214730>},get_filter}

tlist> s 7

Message received by: <0.652.0>
{#Ref<0.733272738.2716598273.214730>,undefined}
```

We can toggle on/off the display of the sent and received messages in order to get a cleaner output:

```
tlist> off send_receive
turning off display of send/receive messages

tlist> a
  0: <0.651.0> yaws_server:gserv_loop/4
  2:  <0.651.0> yaws_server:gserv_loop/4
  4: <0.652.0> yaws_server:peername/2
  8: <0.652.0> yaws_server:aloop/4
  9:   <0.651.0> yaws_server:close_accepted_if_max/2
 11:   <0.651.0> yaws_server:acceptor/1
 13:   <0.651.0> yaws_server:gserv_loop/4
```

### The Memory Option

By using the option `memory`, we will also track the memory usage. Not however that this is not a built in support from the trace functions; instead `edbg` reads the memory of a process as soon as it receive a trace message. This will not result in a fully accurate number, so you need to keep that in mind. Still, it may give you some valuable hints.

```
1> edbg:fstart([yaws_server],[{max_msgs,1000},memory]).

   ... snip...

 0: <0.297.0>(14016) yaws_server:peername/2
 2: <0.296.0>(21848) yaws_server:close_accepted_if_max/2
 4: <0.296.0>(21848) yaws_server:acceptor/1
 6: <0.297.0>(42624) yaws_server:aloop/4
 7: <0.296.0>(21848) yaws_server:gserv_loop/4
 8: <0.314.0>(8944) yaws_server:acceptor0/2
 9:  <0.314.0>(8944) yaws_server:do_accept/1
10:  <0.297.0>(42624) yaws_server:init_db/0
12:  <0.297.0>(42624) yaws_server:fix_abs_uri/2
14:  <0.297.0>(42624) yaws_server:pick_sconf/4
15:   <0.297.0>(42624) yaws_server:pick_sconf/3
```

We can toggle on/off the display of the memory output:

```
tlist> off memory
turning off display of memory usage
```

### Trace a particular Process

By using `edbg:fpid/1,2` you trace a particular process; this will result in all Modules that execute code within that process will be traced.

By using `edbg:fpid/1` , you only have to specify the Process Identifier of the process to be traced. Some default options will be set as shown in the example below:

```
%% The Options are set to be:   
%%                                                                                                                                                                                                                                                                                                                                                                               
%%   [dump_output_eager,                                                                                                                                                                                   
%%    send_receive,                                                                                                                                                                                        
%%    {max_msgs, 1000000}
%%.  ]     
> edbg:fpid(<Pid>).
```

By using `edbg:fpid/2` you can set the Options yourself, which are the same as for `fstart/2`. See the example below:

```
> edbg:fpid(<Pid>, <ListOfOptions>).
```

> NOTE: When debugging a problem, you can add this to you code when you want to trace your process while running some part of the code. Example: ... , edbg:fpid(self()), some\_code(...), edbg:fstop(), ...

### Trace spawned and/or linked processes

By using any of the following options it is possible to extend the tracing to any spawned or linked process:

* **set\_on\_spawn** : Makes any process created by a traced process inherit its trace flags, including flag set\_on\_spawn.
* **set\_on\_first\_spawn** : Makes the first process created by a traced process inherit its trace flags, excluding flag set\_on\_first\_spawn.
* **set\_on\_link** : Makes any process linked by a traced process inherit its trace flags, including flag set\_on\_link.
* **set\_on\_first\_link** : Makes the first process linked to by a traced process inherit its trace flags, excluding flag set\_on\_first\_link.

### Default values in \~/.edbg

It is possible to define some default values in your `~/.edbg` file. The values are:

```
{log_file, <string>}.
{max_msgs, <integer>}.
{trace_time, <integer>}.
{monotonic_ts, <boolean>}.
{send_receive, <boolean>}.
{memory, <boolean>}.
```

Make sure to end each line with a dot!

### Evaluate Expressions

We can evaluate arbitrary Erlang expressions in order to explore alternatives to what was traced:

Here we will use the commands: `set`/`let`/`eval`/`xall`/`xnall` , where:

* set - Bind a variable to the specified argument or return value.
* let - Bind a variable to the result of evaluating an expression.
* eval - Evaluate an arbitrary expression.
* xall - Re-compile and load a module with the export-all flag set.
* xnall - Delete the current module and reload it from the code path.

In the help the commands are shown as:

```
tlist> h
   ...snip..
 (set) <Var> <N> [<ArgN>]  (let) <Var> <Expr>
 (eval) <Expr>  (xall/xnall) <Mod>
```

Let's look at the following function:

```
tlist> a 304
 304:     <0.399.0> restconf:keymember/3
 305:      <0.399.0> restconf:keymember/3

tlist> s 304
 Call: restconf:keymember/3
 -----------------------------------
 keymember(Keys, Pos, [Tuple|Tuples]) ->
```

First we study the argument values...

```
tlist> s 304 1
 Call: restconf:keymember/3 , argument 1:
 -----------------------------------
 [streamcontent,streamcontent_with_timeout,get_more]

tlist> s 304 3
 Call: restconf:keymember/3 , argument 3:
 -----------------------------------
 [{header,{"Last-Modified","Thu, 23 Feb 2023 08:36:23 GMT"}},
  {header,{"Etag","W/\"1677-141382-981909+json\""}},
  {status,201},
  {header,["Location: ",
           [["http","://","localhost",":8008"],
            [47,114,101,115,116,99,111,110,102,47,"data",47,"example-jukebox",
             58,"jukebox",[]]]]}]
            
```

...and then what the function return:

```
tlist> r 304
 Call: restconf:keymember/3 , return value:
 -----------------------------------
 false
```

Now let us explore what it returns if `status` would have been present in the list of the first argument.

First bind a variable to the first argument value:

```
tlist> set Keys 304 1
```

Bind a variable to a list where `status` is added:

```
tlist> let MyKeys [status|Keys].
[status,streamcontent,streamcontent_with_timeout,get_more]
```

Bind a variable to the third argument value:

```
tlist> set Tuples 304 3
```

Make a call to the function with the new argument values:

```
tlist> eval restconf:keymember(MyKeys, 1, Tuples).
 {'EXIT',{undef,[{restconf,keymember, ....snip...
```

Since the function is not exported we get a crash; so we can now recompile and load the module with the `export-all` flag set:

```
tlist> xall restconf
 Module: restconf
 MD5: 43cfd2bafb2e45b864ddbf90b7e2231f
 Object file: /home/tobbe/work/git/trunk/lib/rest/src/restconf
 Compiler options:  [debug_info,export_all,compressed]
   ....snip...
```

And now we can now make the call to the function:

```
tlist> eval restconf:keymember(MyKeys, 1, Tuples).
 true
```

Finally, we reload the original module (which does not have `export-all` set):

```
tlist> xnall restconf
 ....snip...
```

Verify that the function is not exported:

```
tlist> eval restconf:keymember(MyKeys, 1, Tuples).
 {'EXIT',{undef,[{restconf,keymember, ...
```

Note that you can also set a Variable to what a traced function returned by simply omitting to give the `ArgN` argument to the `set` command. Example, counting how many elements a function returned:

```
tlist> set X 20645
tlist> eval erlang:length(X).
161
```

You can also set a variable by using record-field syntax. Example, let's say we want to extract the `client_ip_port`field from a Yaws `#arg{}` record:

```
tlist> pr 24 1                                                                                                                                                                                                       
                                                                                                                                                                                                                  
Call: restconf:out/1 , argument 1:                                                                                                                                                                                   
-----------------------------------                                                                                                                                                                                  
#arg{clisock = #Port<0.112>,                                                                                                                                                                                         
     client_ip_port = {{127,0,0,1},37290},                                                                                                                                                                           
     headers = #headers{connection = undefined,accept = "*/*",host = "localhost:8080",
    ...

tlist> set IpPort 24 1 #arg.client_ip_port           
tlist> eval IpPort.                                  
{{127,0,0,1},37290}
```

### Elixir support

We can also use `edbg` in an Elixir context. To start tracing from the `iex` shell:

```
iex(2)> :edbg.fstart([Elixir.TableTennis.App,Elixir.TableTennis.Repo,
                      Elixir.TableTennisWeb.PlayerController],
                     max_msgs: 1000000)
```

Run your code, stop the tracing and list the result as usual:

```
iex(4)> :edbg.fstop()                                                                                                                                                                                                
:ok                                                                                                                                                                                                                  
iex(5)> :edbg.file() 
```

The output will look something like:

```
14: <0.698.0> 'Elixir.TableTennis.Repo':all/2                                                                                                                                                                        
15:  <0.698.0> 'Elixir.TableTennis.Repo':default_options/1                                                                                                                                                           
17:  <0.698.0> 'Elixir.TableTennis.Repo':prepare_query/3                                                                                                                                                             
20: <0.698.0> 'Elixir.TableTennis.Repo':put_dynamic_repo/1                                                                                                                                                           
22: <0.698.0> 'Elixir.TableTennisWeb.PlayerController':init/1                                                                                                                                                        
24: <0.698.0> 'Elixir.TableTennisWeb.PlayerController':call/2                                                                                                                                                        
25:  <0.698.0> 'Elixir.TableTennisWeb.PlayerController':phoenix_controller_pipeline/2                                                                                                                                
26:   <0.698.0> 'Elixir.TableTennisWeb.PlayerController':action/2                                                                                                                                                    
27:    <0.698.0> 'Elixir.TableTennisWeb.PlayerController':'action (overridable 2)'/2                                                                                                                                 
28:     <0.698.0> 'Elixir.TableTennisWeb.PlayerController':index/2                                                                                                                                                   
29:      <0.698.0> 'Elixir.TableTennis.App':list_players/0
```

It looks very much like for Erlang code. The exception is that not all source code can be shown. This is due to the fact that with Elixir you can inject source code at compile time, which makes it hard to locate where the actual source is defined. But for code that are not injected, it looks similar to the Erlang output:

```
tlist> s 28
    
Call: 'Elixir.TableTennisWeb.PlayerController':index/2
-----------------------------------
def index(conn, _params) do

-----------------------------------
tlist> s 28 2
      
Call: 'Elixir.TableTennisWeb.PlayerController':index/2 , argument 2:
-----------------------------------
#{}
tlist> s 28 1
    
Call: 'Elixir.TableTennisWeb.PlayerController':index/2 , argument 1:
-----------------------------------
#{'__struct__' => 'Elixir.Plug.Conn',
  adapter =>
      {'Elixir.Plug.Cowboy.Conn',#{bindings => #{},body_length => 0,
                                   cert => undefined,has_body => false,
...snip...
```

We can also do a search as expected while other functionality won't work, for example, we can't pretty print records.

For situations where you may not run on a r/w file system, e.g in a Nerves device context, you can do like:

```
Nerves CLI help: https://hexdocs.pm/nerves/iex-with-nerves.html                                                                                                                                                     
                                                                                                                                                                                                                   
Toolshed imported. Run h(Toolshed) for more info.                                                                                                                                                                   
iex(1)> :edbg.fstart([Circuits.I2C], max_msgs: 10000 , log_file: '/data/edbg.trace', cfg_file: false)                                                                                                               
:ok                                                                                                                                                                                                                 
iex(2)> alias Circuits.I2C                                                                                                                                                                                          
Circuits.I2C                                                                                                                                                                                                        
iex(3)> I2C.detect_devices()                                                                                                                                                                                        
Devices on I2C bus "i2c-1":                                                                                                                                                                                         
* 72  (0x48)                                                                                                                                                                                                       
* 88  (0x58)                                                                                                                                                                                                       
* 119  (0x77)                                                                                                                                                                                                      
                                                                                                                                                                                                                   
3 devices detected on 1 I2C buses                                                                                                                                                                                   
iex(4)> :edbg.fstop()                                                                                                                                                                                               
:ok                                                                                                                                                                                                                 
iex(5)> :edbg.file('/data/edbg.trace')                                                                                                                                                                              
                                                                                                                                                                                                                    
(h)elp (a)t [<N>] (d)own (u)p (t)op (b)ottom                                                                                                                                                                       
(s)how <N> [<ArgN>] (r)etval <N> ra(w) <N>                                                                                                                                                                         
(pr)etty print record <N> <ArgN>                                                                                                                                                                                   
(f)ind <RegExp> [<ArgN> <ArgRegExp>]  (fr) <RetRegExp>                                                                                                                                                             
(on)/(off) send_receive | memory                                                                                                                                                                                   
(p)agesize <N> (q)uit                                                                                                                                                                                              
(set) <Var> <N> [<ArgN>]  (let) <Var> <Expr>                                                                                                                                                                       
(eval) <Expr>  (xall/xnall) <Mod>                                                                                                                                                                                  
 0: <0.1190.0> 'Elixir.Circuits.I2C':'__info__'/1                                                                                                                                                                 
 2: <0.1190.0> 'Elixir.Circuits.I2C':detect_devices/0                                                                                                                                                             
 3:  <0.1190.0> 'Elixir.Circuits.I2C':bus_names/0                                                                                                                                                                 
 4:   <0.1190.0> 'Elixir.Circuits.I2C':'-bus_names/0-fun-0-'/1                                                                                                                                                    
 7:  <0.1190.0> 'Elixir.Circuits.I2C':detect_and_print/2                                                                                                                                                          
 8:   <0.1190.0> 'Elixir.Circuits.I2C':detect_devices/1                                                                                                                                                           
 9:    <0.1190.0> 'Elixir.Circuits.I2C':open/1                                                                                                                                                                    
11:    <0.1190.0> 'Elixir.Circuits.I2C':detect_devices/1                                                                                                                                                          
12:     <0.1190.0> 'Elixir.Circuits.I2C':'-detect_devices/1-fun-0-'/2                                                                                                                                             
13:      <0.1190.0> 'Elixir.Circuits.I2C':'device_present?'/2                                                                                                                                                     
14:       <0.1190.0> 'Elixir.Circuits.I2C':read/3                                                                                                                                                                 
15:        <0.1190.0> 'Elixir.Circuits.I2C':read/4                                                                                                                                                                
16:         <0.1190.0> 'Elixir.Circuits.I2C':retry/2 
```

In this case it may even be a good idea to copy the '/data/edbg.trace' trace output file to your development host for further inspection.

But let's continue this trace session on our Nerves device:

```
tlist> r 15                                                                                                                                                                                                         
                                                                                                                                                                                                                
Call: 'Elixir.Circuits.I2C':read/4 , return value:                                                                                                                                                                  
-----------------------------------                                                                                                                                                                                 
{error,i2c_nak}  
```

There seem to be a lot of retries made when trying to read from the I2C sensors. We can go to the bottom and find when we succeed.

```
tlist> b                                                                                                                                                                                                            
tlist> u                                                                                                                                                                                                            
1408:         <0.1190.0> 'Elixir.Circuits.I2C':retry/2                                                                                                                                                              
1409:          <0.1190.0> 'Elixir.Circuits.I2C':'-read/4-fun-0-'/3                                                                                                                                                  
1417:    <0.1190.0> 'Elixir.Circuits.I2C':close/1                                                                                                                                                                   
1420:   <0.1190.0> 'Elixir.Circuits.I2C':'-detect_and_print/2-fun-0-'/1                                                                                                                                             
1422:   <0.1190.0> 'Elixir.Circuits.I2C':'-detect_and_print/2-fun-0-'/1                                                                                                                                             
1424:   <0.1190.0> 'Elixir.Circuits.I2C':'-detect_and_print/2-fun-0-'/1                                                                                                                                             
tlist> u                                                                                                                                                                                                            
1392:     <0.1190.0> 'Elixir.Circuits.I2C':'-detect_devices/1-fun-0-'/2                                                                                                                                             
1393:      <0.1190.0> 'Elixir.Circuits.I2C':'device_present?'/2                                                                                                                                                     
1394:       <0.1190.0> 'Elixir.Circuits.I2C':read/3                                                                                                                                                                 
1395:        <0.1190.0> 'Elixir.Circuits.I2C':read/4                                                                                                                                                                
1396:         <0.1190.0> 'Elixir.Circuits.I2C':retry/2                                                                                                                                                              
1397:          <0.1190.0> 'Elixir.Circuits.I2C':'-read/4-fun-0-'/3                                                                                                                                                  
1404:     <0.1190.0> 'Elixir.Circuits.I2C':'-detect_devices/1-fun-0-'/2                                                                                                                                             
1405:      <0.1190.0> 'Elixir.Circuits.I2C':'device_present?'/2                                                                                                                                                     
1406:       <0.1190.0> 'Elixir.Circuits.I2C':read/3                                                                                                                                                                 
1407:        <0.1190.0> 'Elixir.Circuits.I2C':read/4                                                                                                                                                                
1408:         <0.1190.0> 'Elixir.Circuits.I2C':retry/2                                                                                                                                                              
1409:          <0.1190.0> 'Elixir.Circuits.I2C':'-read/4-fun-0-'/3                                                                                                                                                  
1417:    <0.1190.0> 'Elixir.Circuits.I2C':close/1  
```

Now we can see what the read operation returns when it succeeds:

```
tlist> r 1406                                                                                                                                                                                                       
                                                                                                                                                                                                                    
Call: 'Elixir.Circuits.I2C':read/3 , return value:                                                                                                                                                                  
-----------------------------------                                                                                                                                                                                 
{ok,<<"9">>}
```

We can study the input arguments:

```
tlist> s 1406 1                                                                                                                                                                                                     
                                                                                                                                                                                                                 
Call: 'Elixir.Circuits.I2C':read/3 , argument 1:                                                                                                                                                                    
-----------------------------------                                                                                                                                                                                 
#Ref<0.1309996122.4082761737.112790>                                                                                                                                                                                
tlist> s 1406 2                                                                                                                                                                                                     
                                                                                                                                                                                                                    
Call: 'Elixir.Circuits.I2C':read/3 , argument 2:
-----------------------------------
119                                                                                                       
tlist> s 1406 3                                                                                           
                                                    
Call: 'Elixir.Circuits.I2C':read/3 , argument 3:    
-----------------------------------                                                                       
1   
```

To be able to show the source of the function head we need to copy the trace output file to our development host where we have the source code:

```
❯ scp nerves-5023:/data/edbg.trace .                                                                       
edbg.trace 

iex(1)> :edbg.file('edbg.trace')

tlist> a 1406
1406:       <0.1190.0> 'Elixir.Circuits.I2C':read/3
1407:        <0.1190.0> 'Elixir.Circuits.I2C':read/4
1408:         <0.1190.0> 'Elixir.Circuits.I2C':retry/2
1409:          <0.1190.0> 'Elixir.Circuits.I2C':'-read/4-fun-0-'/3
1417:    <0.1190.0> 'Elixir.Circuits.I2C':close/1
tlist> s 1406
        
Call: 'Elixir.Circuits.I2C':read/3
-----------------------------------
def read(i2c_bus, address, bytes_to_read, opts \\ []) do

                    
```
