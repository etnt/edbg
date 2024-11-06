# Supervisors

Display all supervisors we have found in our system. Each line has a number that can be referenced, where the 'S' mean that the process is a supervisor.

```
(abc@ozzy)1> edbg:suptrees().
1:S kernel_safe_sup <0.74.0> []
2:S kernel_sup <0.49.0> [erl_distribution]
23:S logger_sup <0.75.0> []
27:S net_sup <0.57.0> []

 (h)elp e(x)pand [<N>] (s)hrink [<N>]
 (p)rocess info [<N> [<M>]] (b)acktrace [<N> [<M>]]
 (r)efresh (q)uit
```

Note the short help text; we will now go through what those commands does.

First let us expand supervisor 23 to see its children. Note the indentation and the 'G' which mean that the process is a gen\_server process, where 'W' mean some other worker process.

```
suptrees> x 23
1:S kernel_safe_sup <0.74.0> []
2:S kernel_sup <0.49.0> [erl_distribution]
23:S logger_sup <0.75.0> []
24:G   default <0.79.0> [logger_h_common]
25:G   logger_proxy <0.77.0> [logger_proxy]
26:G   logger_handler_watcher <0.76.0> [logger_handler_watcher]
27:S net_sup <0.57.0> []
```

Print the process-info of worker 24 Note the list of linked processes.

```
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
```

We can also print the process info of any linked processes. Let us print the process-info of the second process in the links list.

```
suptrees> p 24 2

=== Process Info: <0.80.0>
[{current_function,{logger_std_h,file_ctrl_loop,1}},
 {initial_call,{erlang,apply,2}},
 {status,waiting},
 {message_queue_len,0},
 {links,[<0.79.0>]},
 {diction.....snip...
```

We can continue like this...

```
suptrees> p 24 2 1

=== Process Info: <0.79.0>
[{registered_name,logger_std_h_default},
 {current_function,{gen_server,loop,7}},
 {initial_call,{proc_lib,init_p,5}},
 {status,waiting},
 {message_queue_len,0},
 {links,[<0.75.0>,<0.80.0>]},
 {dictionary,....snip...
```

We can also print the process backtrace in the same way:

```
suptrees> b 24 2 

=== Process Backtrace: <0.80.0>
Program counter: 0x00007f73c42c00d0 (logger_std_h:file_ctrl_loop/1 + 56)
CP: 0x0000000000000000 (invalid)

0x00007f73c420d618 Return addr 0x000055c399710288 (<terminate process normally>)
y(0)     []
y(1)     []
y(2)     #{dev=>standard_io,handler_name=>default}
```

We can also setup a monitor for a process:

```
suptrees> m 161 2

Monitoring: <0.343.0>

  ...do stuff, crunch...
  
Monitor got DOWN from: <0.343.0> , Reason: shutdown
```

We can print the state of a gen\_server. Let say we have the following:

```
1:S kernel_safe_sup <0.74.0> []
2:S kernel_sup <0.49.0> [erl_distribution]
3:S   logger_sup <0.75.0> []
4:G     default <0.79.0> [logger_h_common]
  ....snip...
```

Now print the state of the <0.79.0> gen\_server process. Note that we will try to pretty print it if possible.

```
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
```

You can start tracing on a process like this:

```
suptrees> ts 4

  ...do stuff while tracing...
  
```

You stop the tracing like this:

```
suptrees> te
```

You show the trace output (like with edbg:file/0) like this:

```
suptrees> tf
```

### Elixir support

It works all the same in an Elixir context, e.g here on a Nerves device:

```
iex(1)> :edbg.suptrees()
...snip...
52:S 'Elixir.NervesUEvent.Supervisor' <0.1065.0> ['Elixir.PropertyTable']
58:S 'Elixir.SensorHub.Supervisor' <0.1201.0> []
59:G   'Elixir.VEML6030' <0.1204.0> ['Elixir.VEML6030']
60:G   'Elixir.BMP280' <0.1203.0> ['Elixir.BMP280']
61:G   'Elixir.SGP30' <0.1202.0> ['Elixir.SGP30']
62:S 'Elixir.Shoehorn.Supervisor' <0.1051.0> []
...snip...

suptrees> g 60

Process State: <0.1203.0>
#{calibration =>
      #{par_gh1 => -70,par_gh2 => -5542,par_gh3 => 18,par_h1 => 819,
        par_h2 => 1002,par_h3 => 0,par_h4 => 45,par_h5 => 20,par_h6 => 120,
        par_h7 => -100,par_p1 => 36852,par_p10 => 30,par_p2 => -10254,
        par_p3 => 88,par_p4 => 10761,par_p5 => -289,par_p6 => 30,par_p7 => 43,
        par_p8 => -2794,par_p9 => -2536,par_t1 => 25937,par_t2 => 26424,
        par_t3 => 3,range_switching_error => 0,res_heat_range => 1,
        res_heat_val => 57,type => bme680},
  last_measurement =>
      #{'__struct__' => 'Elixir.BMP280.Measurement',
        altitude_m => -238.81177548373972,dew_point_c => 8.105856887666265,
        gas_resistance_ohms => 57747.96852610771,
        humidity_rh => 36.055175197091295,pressure_pa => 102863.57641182805,
        temperature_c => 24.082313736431388,timestamp_ms => 875183},
  sea_level_pa => 100000,sensor_type => bme680,
  transport =>
      #{'__struct__' => 'Elixir.BMP280.Transport',address => 119,
        i2c => #Ref<0.215455982.4082761740.104730>}}
```
