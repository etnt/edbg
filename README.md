# E D B G

A tty based interface to the Erlang debugger and tracer.

## Table of Contents
1. [Install](#install)
2. [Trace examples](#trace-examples)
3. [Supervisor examples](#supervisor-examples)
4. [Custom Color](#color)
5. [Debug Usage](#dbg-usage)
6. [Trace Usage](#trace-usage)
6. [Supervisor Usage](#supervisor-usage)
7. [Debug examples](#dbg-examples)

Useful if you, for example, work from home but still
want to debug your code at your work desktop, or if you
simply don't like the standard debugger GUI.

Note that lots of the debugger functionality is already supported by the
standard OTP `int.erl` module. What the edbg debugger brings is basically the
tty based attached mode and the possibility to make use of interactively
defined conditional break points.

The edbg tracing functionality is (hopefully) a somewhat more novel approach
and actually is what I myself is using all the time. In fact it is so good that
I now have moved the Trace Example chapter just below the Installation ditto.
It is all you need... ;-)

<a name="install"></a>
## INSTALL
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

NOTE: If you don't like rebar (like me) and favour plain Make + Erlc then:
```
   Run: make old
   Add: code:add_path("YOUR-PATH-HERE/edbg/ebin").
   Add: code:add_path("YOUR-PATH-HERE/edbg/deps/pp_record/ebin").
     to your ~/.erlang file.
```

<a name="trace-examples"></a>
## TRACE EXAMPLES
```erlang
   # TRACE THE SPECIFIED MODULES BELOW.
   # THE TRACE OUTPUT WILL BE STORED ON FILE.
   1> edbg:fstart([yaws_server,yaws,yaws_config],[{max_msgs,10000}]).
   ok

   # NOW RUN A CALL TOWARD YAWS !!

   2> edbg:fstop().
   ok

   # WE RELY ON USING THE DEFAULT FILENAME, HENCE NO NEED TO
   # SPECIFY IT HERE WHEN LOADING THE TRACE INFO.
   3> edbg:file().

    (h)elp (a)t [<N>] (d)own (u)p (t)op (b)ottom
    (s)how <N> [<ArgN>] (r)etval <N> ra(w) <N>
    (pr)etty print record <N> <ArgN>
    (f)ind <M>:<Fx> [<ArgN> <ArgVal>] | <RetVal>
    (p)agesize <N> (q)uit
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

   # TO INSPECT A PARTICULAR CALL (without drowning in output),
   # WE USE THE (s)how COMMAND WHICH WILL DISPLAY THE FUNCTION
   # CLAUSE HEADS IN ORDER TO HELP US DECIDE WHICH ARGUMENT TO INSPECT.
   tlist> s 4

   Call: yaws_server:peername/2
   -----------------------------------

   peername(CliSock, ssl) ->

   peername(CliSock, nossl) ->

   -----------------------------------

   # SHOW WHAT THE SECOND ARGUMENT CONTAINED
   tlist> s 4 2

   Call: yaws_server:peername/2 , argument 2:
   -----------------------------------
   nossl

   # WHAT DID THE FUNCTION CALL RETURN?
   tlist> r 4

   Call: yaws_server:peername/2 , return value:
   -----------------------------------
   {{127,0,0,1},35871}

   # IF WE KNOW THE ARGUMENT TO BE A RECORD, WE CAN PRETTY PRINT IT
   tlist> s 177

   Call: yaws_server:handle_request/3
   -----------------------------------

   handle_request(CliSock, ARG, _N)
     when is_record(ARG#arg.state, rewrite_response) ->

   handle_request(CliSock, ARG, N) ->

   -----------------------------------
   tlist> s 177 2

   Call: yaws_server:handle_request/3 , argument 2:
   -----------------------------------
   {arg,#Port<0.6886>,
        {{127,0,0,1},35871},
        {headers,undefined,"*/*","localhost:8008",undefined,undefined,undefined,
                 undefined,undefined,undefined,undefined,"curl/7.35.0",undefined,
                 [],undefined,undefined,undefined,undefined,undefined,
                 {"admin","admin","Basic YWRtaW46YWRtaW4="},
                 undefined,undefined,[]},
        ....

   tlist> pr 177 2

   Call: yaws_server:handle_request/3 , argument 2:
   -----------------------------------
   #arg{clisock = #Port<0.6886>,
        client_ip_port = {{127,0,0,1},35871},
        headers = #headers{connection = undefined,accept = "*/*",
                           host = "localhost:8008",if_modified_since = undefined,
                           if_match = undefined,if_none_match = undefined,
                           if_range = undefined,if_unmodified_since = undefined,
                           range = undefined,referer = undefined,
                           user_agent = "curl/7.35.0",accept_ranges = undefined,
                           cookie = [],keep_alive = undefined,location = undefined,
                           content_length = undefined,content_type = undefined,
                           content_encoding = undefined,
                           authorization = {"admin","admin","Basic YWRtaW46YWRtaW4="},
                           transfer_encoding = undefined,x_forwarded_for = undefined,
                           other = []},
        req = #http_request{method = 'GET',
                            path = {abs_path,"/restconf/data"},
       ....

   # HERE IS AN EXAMPLE OF HOW WE CAN SEARCH FOR PARTICULAR
   # FUNCTION CALL THAT WE ARE INTERESTED IN.
   # NOTE THAT IT IS ENOUGH TO JUST SPECIFY A PREFIX OF THE FUNCTION NAME
   tlist> f yaws:decode_b
    32:           <0.537.0> yaws:decode_base64/1
    33:            <0.537.0> yaws:decode_base64/2
    34:             <0.537.0> yaws:d/1
      ...


   # WE CAN ALSO SEARCH IN A PARTICULAR ARGUMENT
   # (the 2:nd argument should contain 'packet_size')
   tlist> f yaws:setopts 2 packet_size
    22:         <0.537.0> yaws:setopts/3
    24:         <0.537.0> yaws:do_recv/3
    26:         <0.537.0> yaws:http_collect_headers/5
    ...

   tlist> s 22 2

   Call: yaws:setopts/3 , argument 2:
   -----------------------------------
   [{packet,httph},{packet_size,16384}]


   # WE CAN ALSO SEARCH AMONG THE RETURN VALUES:
   tlist> f GET
   184:           <0.537.0> yaws:make_allow_header/1
   187:          <0.537.0> yaws_server:deliver_accumulated/1
   188:           <0.537.0> yaws:outh_get_content_encoding/0
   190:           <0.537.0> yaws:outh_set_content_encoding/1
     ...

   tlist> r 184

   Call: yaws:make_allow_header/1 , return value:
   -----------------------------------
   ["Allow: GET, POST, OPTIONS, HEAD\r\n"]
 ```

By using the option 'monotonic_ts', we will also see the
elapsed monotonic time, in nano seconds, from the first
received trace message.

```erlang
   # BY ADDING THE OPTION 'monotonic_ts' WHEN STARTING
   # THE TRACE; WE WILL SEE THE ELAPSED NANO SECONDS FOR
   # EACH CALL, COUNTED FROM THE FIRST TRACED CALL.
   (confd@hedlund)1> edbg:fstart([yaws_server],[{max_msgs,10000},
                                                 monotonic_ts]).
   ok
   (confd@hedlund)2> edbg:fstop().
   ok
   (confd@hedlund)3> edbg:file().

    (h)elp (a)t [<N>] (d)own (u)p (t)op (b)ottom
    (s)how <N> [<ArgN>] (r)etval <N> ra(w) <N>
    (pr)etty print record <N> <ArgN>
    (f)ind <M>:<Fx> [<ArgN> <ArgVal>] | <RetVal>
    (p)agesize <N> (q)uit
      0: <0.297.0> yaws_server:peername/2 - 0
      2: <0.296.0> yaws_server:close_accepted_if_max/2 - 37276
      4: <0.296.0> yaws_server:acceptor/1 - 46436
      6: <0.296.0> yaws_server:gserv_loop/4 - 72287
      7: <0.315.0> yaws_server:acceptor0/2 - 104797
      8:  <0.315.0> yaws_server:do_accept/1 - 110475
      9: <0.297.0> yaws_server:aloop/4 - 142479
     10:  <0.297.0> yaws_server:init_db/0 - 164453
     12:  <0.297.0> yaws_server:fix_abs_uri/2 - 216505
     14:  <0.297.0> yaws_server:pick_sconf/4 - 220942
     15:   <0.297.0> yaws_server:pick_sconf/3 - 225815
     16:    <0.297.0> yaws_server:pick_host/4 - 230099
     17:     <0.297.0> yaws_server:comp_sname/2 - 235914
     18:      <0.297.0> yaws_server:comp_sname/2 - 238042
     19:       <0.297.0> yaws_server:comp_sname/2 - 239546
     20:        <0.297.0> yaws_server:comp_sname/2 - 240632
   tlist>
```

By using the option 'send_receive', we will also see sent and
received messages by any 'known' pid.


```erlang
   # BY ADDING THE OPTION 'send_receive' WHEN STARTING
   # THE TRACE; WE CAN NOW SEE AND INSPECT MESSAGES
   # SENT AND RECEIVED BY ANY KNOWN PID.
   (confd@hedlund)1> edbg:fstart([yaws_server],[{max_msgs,10000},
                                                 send_receive]).
   ok
   (confd@hedlund)2> edbg:fstop().
   ok
   (confd@hedlund)3> edbg:file().

    (h)elp (a)t [<N>] (d)own (u)p (t)op (b)ottom
    (s)how <N> [<ArgN>] (r)etval <N> ra(w) <N>
    (pr)etty print record <N> <ArgN>
    (f)ind <M>:<Fx> [<ArgN> <ArgVal>] | <RetVal>
    (on)/(off) send_receive
    (p)agesize <N> (q)uit
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
     14: <0.799.0> yaws_server:acceptor0/2
     15:  >>> Send(<0.799.0>) -> To(yaws_trace)  {'$gen_cast',{open,<...
     16:  <0.799.0> yaws_server:do_accept/1
     17:  <0.652.0> yaws_server:init_db/0
     19:  <<< Receive(<0.652.0>)  {inet_async,#Port<0....
     20:  <<< Receive(<0.652.0>)  {inet_async,#Port<0....
   tlist> s 6

   Message sent by: <0.652.0>  to: <0.626.0>
   {'$gen_call',{<0.652.0>,#Ref<0.733272738.2716598273.214730>},get_filter}

   tlist> s 7

   Message received by: <0.652.0>
   {#Ref<0.733272738.2716598273.214730>,undefined}

   # NOTE THAT WE CAN TOGGLE ON/OFF THE DISPLAY OF THE
   # SENT AND RECEIVED MESSAGES
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
     14: <0.799.0> yaws_server:acceptor0/2
     16:  <0.799.0> yaws_server:do_accept/1
     17:  <0.652.0> yaws_server:init_db/0

    tlist>
```

By using the option 'memory', we will also track the memory usage.

```erlang
   # BY ADDING THE OPTION 'memory' WHEN STARTING
   # THE TRACE; WE CAN NOW TRACK THE MEMORY USAGE OF THE
   # TRACED PROCESSES
   (confd@hedlund)1> edbg:fstart([yaws_server],[{max_msgs,1000},memory]).
   ok
   (confd@hedlund)2> edbg:fstop().
   ok
   (confd@hedlund)3> edbg:file().

    (h)elp (a)t [<N>] (d)own (u)p (t)op (b)ottom
    (s)how <N> [<ArgN>] (r)etval <N> ra(w) <N>
    (pr)etty print record <N> <ArgN>
    (f)ind <M>:<Fx> [<ArgN> <ArgVal>] | <RetVal>
    (on)/(off) send_receive | memory
    (p)agesize <N> (q)uit
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
    16:    <0.297.0>(42624) yaws_server:pick_host/4
    17:     <0.297.0>(42624) yaws_server:comp_sname/2
    18:      <0.297.0>(42624) yaws_server:comp_sname/2
    19:       <0.297.0>(42624) yaws_server:comp_sname/2
    20:        <0.297.0>(42624) yaws_server:comp_sname/2

   tlist> off memory
   turning off display of memory usage

   tlist> at
     0: <0.297.0> yaws_server:peername/2
     2: <0.296.0> yaws_server:close_accepted_if_max/2
     4: <0.296.0> yaws_server:acceptor/1
     6: <0.297.0> yaws_server:aloop/4
     7: <0.296.0> yaws_server:gserv_loop/4
     8: <0.314.0> yaws_server:acceptor0/2
     9:  <0.314.0> yaws_server:do_accept/1
    10:  <0.297.0> yaws_server:init_db/0
    12:  <0.297.0> yaws_server:fix_abs_uri/2
    14:  <0.297.0> yaws_server:pick_sconf/4
    15:   <0.297.0> yaws_server:pick_sconf/3
    16:    <0.297.0> yaws_server:pick_host/4
    17:     <0.297.0> yaws_server:comp_sname/2
    18:      <0.297.0> yaws_server:comp_sname/2
    19:       <0.297.0> yaws_server:comp_sname/2
    20:        <0.297.0> yaws_server:comp_sname/2
```


<a name="supervisor-examples"></a>
## SUPERVISOR EXAMPLES

``` erlang
    # DISPLAY ALL SUPERVISORS WE HAVE FOUND.
    # EACH LINE HAS A NUMBER THAT CAN BE REFERENCED.
    # THE 'S' MEAN THAT THE PROCESS IS A SUPERVISOR.
    (abc@ozzy)1> edbg:suptrees().
    1:S kernel_safe_sup <0.74.0> []
    2:S kernel_sup <0.49.0> [erl_distribution]
    23:S logger_sup <0.75.0> []
    27:S net_sup <0.57.0> []

     (h)elp e(x)pand [<N>] (s)hrink [<N>]
     (p)rocess info [<N> [<M>]] (b)acktrace [<N> [<M>]]
     (r)efresh (q)uit

    # LET US EXPAND SUPERVISOR 23 TO SEE ITS CHILDREN.
    # NOTE THE INDENTATION AND THE 'G' WHICH MEAN: GEN_SERVER PROCESS
    # ('W' MEAN: SOME OTHER WORKER PROCESS)
    suptrees> x 23
    1:S kernel_safe_sup <0.74.0> []
    2:S kernel_sup <0.49.0> [erl_distribution]
    23:S logger_sup <0.75.0> []
    24:G   default <0.79.0> [logger_h_common]
    25:G   logger_proxy <0.77.0> [logger_proxy]
    26:G   logger_handler_watcher <0.76.0> [logger_handler_watcher]
    27:S net_sup <0.57.0> []


    # PRINT THE PROCESS-INFO OF WORKER 24
    # NOTE THE LIST OF LINKED PROCESSES 
    suptrees> p 24
    
    === Process Info: <0.79.0>
    [{registered_name,logger_std_h_default},
     {current_function,{gen_server,loop,7}},
     {initial_call,{proc_lib,init_p,5}},
     {status,waiting},
     {message_queue_len,0},
     {links,[<0.75.0>,<0.80.0>]},
     {dictionary,
         [{'$ancest...snip...  
         
    # WE CAN ALSO PRINT THE PROCESS INFO OF ANY LINKED PROCESSES.
    # LET US PRINT THE PROCESS-INFO OF THE SECOND PROCESS IN THE LINKS LIST.
    suptrees> p 24 2
    
    === Process Info: <0.80.0>
    [{current_function,{logger_std_h,file_ctrl_loop,1}},
     {initial_call,{erlang,apply,2}},
     {status,waiting},
     {message_queue_len,0},
     {links,[<0.79.0>]},
     {diction.....snip...
     
    # WE CAN CONTINUE LIKE THIS...
    suptrees> p 24 2 1
    
    === Process Info: <0.79.0>
    [{registered_name,logger_std_h_default},
     {current_function,{gen_server,loop,7}},
     {initial_call,{proc_lib,init_p,5}},
     {status,waiting},
     {message_queue_len,0},
     {links,[<0.75.0>,<0.80.0>]},
     {dictionary,....snip...


    # WE CAN ALSO PRINT THE PROCESS BACKTRACE IN THE SAME WAY
    suptrees> b 24 2 
    
    === Process Backtrace: <0.80.0>
    Program counter: 0x00007f73c42c00d0 (logger_std_h:file_ctrl_loop/1 + 56)
    CP: 0x0000000000000000 (invalid)
    
    0x00007f73c420d618 Return addr 0x000055c399710288 (<terminate process normally>)
    y(0)     []
    y(1)     []
    y(2)     #{dev=>standard_io,handler_name=>default}


    # WE CAN ALSO SETUP A MONITOR FOR A PROCESS
    suptrees> m 161 2
    
    Monitoring: <0.343.0>
    
      ...do stuff, crunch...
      
    Monitor got DOWN from: <0.343.0> , Reason: shutdown


    # WE CAN PRINT THE STATE OF A GEN_SERVER.
    # LET SAY WE HAVE THE FOLLOWING:
    1:S kernel_safe_sup <0.74.0> []
    2:S kernel_sup <0.49.0> [erl_distribution]
    3:S   logger_sup <0.75.0> []
    4:G     default <0.79.0> [logger_h_common]
      ....snip...

    # NOW PRINT THE STATE OF THE <0.79.0> GEN_SERVER PROCESS.
    # NOTE THAT WE WILL TRY TO PRETTY PRINT IT IF POSSIBLE.
    suptrees> g 4
    
    Process State: <0.79.0>
    #{burst_limit_enable => true,burst_limit_max_count => 500,
      burst_limit_window_time => 1000,burst_msg_count => 0,
      burst_win_ts => -576460751792378,
      cb_state =>
          #{ctrl_sync_count => 20,filesync_repeat_interval => no_repeat,
            handler_state => #{file_ctrl_pid => <0.80.0>,type => standard_io},
            id => default,last_op => sync,module => logger_std_h},
      drop_mode_qlen => 200,flush_qlen => 1000,id => logger_std_h_default,
      idle => true,last_load_ts => -576460751792378,last_qlen => 0,mode => async,
      mode_ref => #Ref<0.3359499227.323092491.38039>,module => logger_h_common,
      overload_kill_enable => false,overload_kill_mem_size => 3000000,
      overload_kill_qlen => 20000,overload_kill_restart_after => 5000,
      sync_mode_qlen => 10}


    # YOU CAN START TRACING ON A PROCESS LIKE THIS
    suptrees> ts 4
    
      ...do stuff while tracing...
      
    # ...STOP THE TRACING...
    suptrees> te
    
    # ...SHOW THE TRACE OUTPUT (LIKE: edbg:file/0)
    suptrees> tf
 
```

<a name="color"></a>
## CUSTOM COLOR
Custom colors may be set using the environment variable EDBG_COLOR.

It should contain a SPACE separated string with ITEM=COLOR entries.

Valid ITEMs are:
```
   att    - attention color    (default: whiteb)
   warn   - warning color      (default: yellow)
   err    - error color        (default: red)
   cur    - current line color (default: green)
```

Valid COLORs are:
```
   black,   blackb
   red,     redb
   green,   greenb
   yellow,  yellowb
   blue,    blueb
   magenta, magentab
   cyan,    cyanb
   white,   whiteb
```

Colors ending in 'b' are the bright variants.

Example:
```
   export EDBG_COLOR="att=yellowb warn=magenta"
```

Colors not specified in EDBG_COLOR will keep their defaults.


<a name="dbg-usage"></a>
## DEBUG USAGE

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

<a name="trace-usage"></a>
## TRACE USAGE
The `fstart` functions is a newer interface which is using the
Erlang trace BIFs directly (i.e not using the dbg.erl module).
They also store the trace output on file so that it survives
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

### edbg:tstart(Mod, Mods [,Opts])
This is an older and obsoleted interface for starting the tracing.
Start tracing the first time `Mod` (an atom) is called, in any process,
and in any spawned process from such a process.
Do also trace any call to the modules `Mods` (list of atoms).

`Opts` is a list of any of the following options:

* `{setup_mod, Mod}` : the module that will setup the tracing; default is `edbg_trace_filter`.
* `{trace_max}` : max number of trace messages; default is 10000.

The trace-start setup will automatically be stored on file
for easy reload.

### edbg:tlist()
Enter trace list mode, where the trace output can be inspected.

### edbg:tquit()
Stop and quit the edbg tracer.

### edbg:lts()
Load the latest trace-start setup from file.


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

See the [Supervisor examples](#supervisor-examples) for how to use it.

<a name="dbg-examples"></a>
## DEBUG EXAMPLES
```erlang
   # LOAD THE yaws_server MODULE INTO THE DEBUGGER AND SET A BREAK POINT
   1> edbg:i(yaws_server, 1189).
   ok

   # NOW RUN THE TEST...

   # LIST DEBUGGED PROCESSES
   2> edbg:plist().
   <0.536.0>  yaws_server:gserv_loop/4   waiting
   <0.648.0>  yaws_server:acceptor0/2    break       yaws_server:1189
   <0.537.0>  yaws_server:POST/4         exit        Reason: shutdown
   <0.516.0>  yaws_server:safe_decode_p  idle
   <0.663.0>  yaws_server:acceptor0/2    running
   ok

   # ATTATCH TO A DEBUGGED PROCESS
   # NOTE THE LINE WHERE WE HAVE STOPPED ON THE BREAK POINT: '1189>'
   3> edbg:attach(0,648,0).

    (h)elp (a)t <Ctx> (n)ext (s)tep (f)inish (c)ontinue s(k)ip
    (m)essages (t)oggle trace (q)uit (br)eakpoints (p)rocesses
    (de)lete/(di)sable/(en)able/(te)st/(b)reak <Line | Mod Line>
    (v)ar <variable> (e)val <expr> (i)nterpret <Mod>  conte(x)t <Ctx>
    (u)p (d)own (l)ist module <Mod Line <Ctx>>
     MODULE: yaws_server.erl
     1184:         _ ->
     1185:             ok
     1186:     end,
     1187:
     1188:     process_flag(trap_exit, false),
     1189>     init_db(),
     1190:     SSL = GS#gs.ssl,
     1191:     Head = yaws:http_get_headers(CliSock, SSL),
     1192:     process_flag(trap_exit, true),
     1193:     ?Debug("Head = ~p~n", [Head]),
     1194:     case Head of

   # NOW WE CAN SINGLE STEP AS USUAL
   # NOTE THE LINE INDICATION FOR A BREAK POINT: '1189*'
   (<0.648.0>)> n
   MODULE: yaws_server.erl
   1185:             ok
   1186:     end,
   1187:
   1188:     process_flag(trap_exit, false),
   1189*     init_db(),
   1190>     SSL = GS#gs.ssl,
   1191:     Head = yaws:http_get_headers(CliSock, SSL),
   1192:     process_flag(trap_exit, true),
   1193:     ?Debug("Head = ~p~n", [Head]),
   1194:     case Head of
   1195:         {error, {too_many_headers, ReqTooMany}} ->

   # WE STEP ONE MORE LINE AND CHECK THE CONTENT OF A VARIABLE
   # NOTE THAT WE ONLY NEED TO SPECIFY A PREFIX OF THE VARIABLE NAME
   (<0.648.0>)> n
   MODULE: yaws_server.erl
   1186:     end,
   1187:
   1188:     process_flag(trap_exit, false),
   1189*     init_db(),
   1190:     SSL = GS#gs.ssl,
   1191>     Head = yaws:http_get_headers(CliSock, SSL),
   1192:     process_flag(trap_exit, true),
   1193:     ?Debug("Head = ~p~n", [Head]),
   1194:     case Head of
   1195:         {error, {too_many_headers, ReqTooMany}} ->
   1196:             %% RFC 6585 status code 431

   (<0.648.0>)> v SS
   'SSL' = nossl

   # SUPPLYING AN EMPTY PROMPT REPEATS LAST COMMAND
   (<0.648.0>)> 
   'SSL' = nossl

   # IF WE KNOW THE VARIABLE CONTAINS A RECORD, WE CAN PRETTY PRINT IT
   (<0.660.0>)> pr GS
   'GS' =
   #gs{gconf =
           #gconf{
               yaws_dir = undefined,trace = false,flags = 64,
               logdir = "./logs",ebin_dir = [],src_dir = [],runmods = [],
       ....


   # AFTER SOME MORE 'next' OPERATIONS, WE ARE HERE:
   (<0.648.0>)> n
   MODULE: yaws_server.erl
   1198:             SC = pick_sconf(GS#gs.gconf, #headers{}, GS#gs.group),
   1199:             put(sc, SC),
   1200:             put(outh, #outh{}),
   1201:             deliver_431(CliSock, ReqTooMany);
   1202:         {Req0, H0} when Req0#http_request.method /= bad_request ->
   1203>             {Req, H} = fix_abs_uri(Req0, H0),
   1204:             ?Debug("{Req, H} = ~p~n", [{Req, H}]),
   1205:             SC = pick_sconf(GS#gs.gconf, H, GS#gs.group),
   1206:             put(outh, #outh{}),
   1207:             put(sc, SC),
   1208:             DispatchResult = case SC#sconf.dispatch_mod of

   # WE CAN NOW STEP INTO A FUNCTION AS USUAL
   (<0.648.0>)> s
   MODULE: yaws_server.erl
   91420: deepforeach(F, X) ->
   1421:     F(X).
   1422:
   1423:
   1424: fix_abs_uri(Req, H) ->
   1425>     case Req#http_request.path of
   1426:         {absoluteURI, _Scheme, Host0, Port, RawPath} ->
   1427:             Host = case Port of
   1428:                        P when is_integer(P) ->
   1429:                            Host0 ++ [$: | integer_to_list(P)];
   1430:                                                 % Is this ok?

   # WE CAN GO UP IN THE CALL CHAIN...
   (<0.648.0>)> u
   MODULE: yaws_server.erl
   1198:             SC = pick_sconf(GS#gs.gconf, #headers{}, GS#gs.group),
   1199:             put(sc, SC),
   1200:             put(outh, #outh{}),
   1201:             deliver_431(CliSock, ReqTooMany);
   1202:         {Req0, H0} when Req0#http_request.method /= bad_request ->
   1203>             {Req, H} = fix_abs_uri(Req0, H0),
   1204:             ?Debug("{Req, H} = ~p~n", [{Req, H}]),
   1205:             SC = pick_sconf(GS#gs.gconf, H, GS#gs.group),
   1206:             put(outh, #outh{}),
   1207:             put(sc, SC),
   1208:             DispatchResult = case SC#sconf.dispatch_mod of


   # ...AND DOWN AGAIN
   (<0.648.0>)> d
   MODULE: yaws_server.erl
   1420: deepforeach(F, X) ->
   1421:     F(X).
   1422:
   1423:
   1424: fix_abs_uri(Req, H) ->
   1425>     case Req#http_request.path of
   1426:         {absoluteURI, _Scheme, Host0, Port, RawPath} ->
   1427:             Host = case Port of
   1428:                        P when is_integer(P) ->
   1429:                            Host0 ++ [$: | integer_to_list(P)];
   1430:                                                 % Is this ok?

   # WE CAN FINISH THIS FUNCTION
   (<0.648.0>)> f
   MODULE: yaws_server.erl
   1199:             put(sc, SC),
   1200:             put(outh, #outh{}),
   1201:             deliver_431(CliSock, ReqTooMany);
   1202:         {Req0, H0} when Req0#http_request.method /= bad_request ->
   1203:             {Req, H} = fix_abs_uri(Req0, H0),
   1204>             ?Debug("{Req, H} = ~p~n", [{Req, H}]),
   1205:             SC = pick_sconf(GS#gs.gconf, H, GS#gs.group),
   1206:             put(outh, #outh{}),
   1207:             put(sc, SC),
   1208:             DispatchResult = case SC#sconf.dispatch_mod of
   1209:                                  undefined ->


   # AT THIS POINT WE REALIZE THAT WE WOULD LIKE TO ENTER A NEW MODULE...
   (<0.648.0>)> n
   MODULE: yaws_server.erl
   1232:                     ?TC([{record, SC, sconf}]),
   1233:                     ?Debug("Headers = ~s~n", [?format_record(H, headers)]),
   1234:                     ?Debug("Request = ~s~n",
   1235:                            [?format_record(Req, http_request)]),
   1236:                     run_trace_filter(GS, IP, Req, H),
   1237>                     yaws_stats:hit(),
   1238:                     check_keepalive_maxuses(GS, Num),
   1239:                     Call = case yaws_shaper:check(SC, IP) of
   1240:                                allow ->
   1241:                                    call_method(Req#http_request.method,CliSock,
   1242:                                                IPPort,Req,H);

   # ...SO WE LOAD THE 'yaws_stats' MODULE INTO THE DEBUGGER...
   (<0.648.0>)> i yaws_stats
   Interpreted modules: [yaws_server,yaws_stats]

   # ...WE CAN NOW ENTER THAT MODULE
   (<0.648.0>)> s
   MODULE: yaws_stats.erl
   41: stop(Pid) ->
   42:     gen_server:cast(Pid, {stop}).
   43:
   44:
   45: hit() ->
   46>     case get_stats() of
   47:         undefined ->
   48:             ok;
   49:         Pid ->
   50:             gen_server:cast(Pid, {hit})
   51:     end.

   # WE CAN EVALUATE EXPRESSIONS
   (<0.648.0>)> e io:format("Test of eval: ~p~n",[[X*2 || X <- [1,2,3]]]).
   Test of eval: [2,4,6]
   EVALUATED VALUE:
   ok

   # WE CAN LIST BREAK POINTS
   (<0.648.0>)> br

   BREAKPOINTS
    yaws_server:1189  Status=active Trigger=enable Cond=null


   # WE CAN LIST DEBUGGED PROCESSES
   (<0.648.0>)> p
   <0.536.0>  yaws_server:gserv_loop/4   waiting
   <0.648.0>  yaws_server:acceptor0/2    break       yaws_server:1238
   <0.537.0>  yaws_server:POST/4         exit        Reason: shutdown
   <0.516.0>  yaws_server:safe_decode_p  idle
   <0.663.0>  yaws_server:acceptor0/2    running

   # WE CAN LIST ANY MODULE (not just debugged ones)
   (<0.648.0>)> l yaws_api 343
   MODULE: yaws_api.erl
   338:     parse_arg_value(Line, Key, [C|Value], Quote, true).
   339:
   340:
   341: %%
   342:
   343> make_parse_line_reply(Key, Value, Rest) ->
   344:     {{yaws:funreverse(Key, fun yaws:to_lowerchar/1),
   345:       lists:reverse(Value)}, Rest}.
   346:
   347:
   348: -record(mp_parse_state, {

   # TO SEE WHERE WE ARE AT THE MOMENT:
   (<0.648.0>)> a
   MODULE: yaws_server.erl
   1233:                     ?Debug("Headers = ~s~n", [?format_record(H, headers)]),
   1234:                     ?Debug("Request = ~s~n",
   1235:                            [?format_record(Req, http_request)]),
   1236:                     run_trace_filter(GS, IP, Req, H),
   1237:                     yaws_stats:hit(),
   1238>                     check_keepalive_maxuses(GS, Num),
   1239:                     Call = case yaws_shaper:check(SC, IP) of
   1240:                                allow ->
   1241:                                    call_method(Req#http_request.method,CliSock,
   1242:                                                IPPort,Req,H);
   1243:                                {deny, Status, Msg} ->

   # WE CAN CHANGE THE AMOUNT OF CONTEXT TO BE SHOWN
  (<0.648.0>)> x 15

  (<0.648.0>)> a
   MODULE: yaws_server.erl
   1223:                         true  -> {ok, Num+1};
   1224:                         false -> aloop(CliSock, IPPort, GS, Num+1)
   1225:                     end;
   1226:                 closed ->
   1227:                     %% Dispatcher closed the socket
   1228:                     erase_transients(),
   1229:                     {ok, Num+1};
   1230:                 continue ->
   1231:                     ?Debug("SC: ~s", [?format_record(SC, sconf)]),
   1232:                     ?TC([{record, SC, sconf}]),
   1233:                     ?Debug("Headers = ~s~n", [?format_record(H, headers)]),
   1234:                     ?Debug("Request = ~s~n",
   1235:                            [?format_record(Req, http_request)]),
   1236:                     run_trace_filter(GS, IP, Req, H),
   1237:                     yaws_stats:hit(),
   1238>                     check_keepalive_maxuses(GS, Num),
   1239:                     Call = case yaws_shaper:check(SC, IP) of
   1240:                                allow ->
   1241:                                    call_method(Req#http_request.method,CliSock,
   1242:                                                IPPort,Req,H);
   1243:                                {deny, Status, Msg} ->
   1244:                                    deliver_xxx(CliSock, Req, Status, Msg)
   1245:                            end,
   1246:                     Call2 = fix_keepalive_maxuses(Call),
   1247:                     handle_method_result(Call2, CliSock, IPPort,
   1248:                                          GS, Req, H, Num)
   1249:             end;
   1250:         closed ->
   1251:             case yaws_trace:get_type(GS#gs.gconf) of
   1252:                 undefined -> ok;
   1253:                 _         -> yaws_trace:write(from_client, "closed\n")
```

Here is an example of an interactively defined conditional break point
(not possible from the standard GUI debugger):

```erlang
  # FIRST WE DEFINE THE TRIGGER FUNCTION.
  # IT WILL GET THE CURRENT VARIABLE BINDINGS AS THE ARGUMENT
  # AND MUST RETURN EITHER 'true' OR 'false'
  1> F = fun(Bindings) -> case int:get_binding('SSL', Bindings) of
                             {value,nossl} -> true;
                             _ -> false
                          end
         end.
  #Fun<erl_eval.6.50752066>

  # NOW WE SET OUR INTERCTIVELY DEFINED BREAK POINT
  2> edbg:it(yaws_server, 1191, F).
  ok

  # WE RUN OUR TEST...

  # LIST THE DEBUGGED PROCESSES
  3> edbg:plist().
  <0.537.0>  yaws_server:gserv_loop/4   waiting
  <0.538.0>  yaws_server:POST/4         exit        Reason: shutdown
  <0.517.0>  yaws_server:safe_decode_p  idle
  <0.666.0>  yaws_server:acceptor0/2    break       yaws_server:1191
  <0.519.0>  yaws_server:handle_call/3  idle
  <0.718.0>  yaws_server:acceptor0/2    running
  ok

  # ATTACH OURSELVES TO THE FIRST PROCESS STOPPED AT A BREAK POINT
  4> edbg:a().

   (h)elp (a)t <Ctx> (n)ext (s)tep (f)inish (c)ontinue s(k)ip
   (m)essages (t)oggle trace (q)uit (br)eakpoints (p)rocesses
   (de)lete/(di)sable/(en)able/(te)st/(b)reak <Line | Mod Line>
   (v)ar <variable> (e)val <expr> (i)nterpret <Mod>  conte(x)t <Ctx>
   (u)p (d)own (l)ist module <Mod Line <Ctx>>
   MODULE: yaws_server.erl
     1186:     end,
     1187:
     1188:     process_flag(trap_exit, false),
     1189:     init_db(),
     1190:     SSL = GS#gs.ssl,
     1191>     Head = yaws:http_get_headers(CliSock, SSL),
     1192:     process_flag(trap_exit, true),
     1193:     ?Debug("Head = ~p~n", [Head]),
     1194:     case Head of
     1195:         {error, {too_many_headers, ReqTooMany}} ->
     1196:             %% RFC 6585 status code 431

  (<0.666.0>)> v SSL
     'SSL' = nossl
```

