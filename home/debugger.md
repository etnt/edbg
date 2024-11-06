# Debugger

We can specify what module to debug and at the same time set a break point by using the `edbg:i/2` command, as in the example below:

```
1> edbg:i(yaws_server, 1189).
ok
```

Here we say that we want to debug the module: `yaws_server` and set a break point at row `1189`. We now run some test that can trigger the break point to trigger. To see if any process has hit the break point we do:

```
2> edbg:plist().
<0.536.0>  yaws_server:gserv_loop/4   waiting
<0.648.0>  yaws_server:acceptor0/2    break       yaws_server:1189
<0.537.0>  yaws_server:POST/4         exit        Reason: shutdown
<0.516.0>  yaws_server:safe_decode_p  idle
<0.663.0>  yaws_server:acceptor0/2    running
ok
```

From the list above we can see that the process `<0.648.0>` has hit the break point. We can now attach our self to that process so that we can start single-step through the execution:

```
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
```

First we see a compact help with the commands we can use. Note the line where we have stopped on the break point, line 1189, which is indicated with a right arrow: '1189>'.

We can now we can single step through the code, by using the `n(ext)` command:

```
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
```

Note how the arrow (`>`) which indicates the current line has moved to line 1190. The break point at line 1189 still exist, which is indicated with a star (`*`).

We step one more line and check the content of the variable: `SSL`. Note that we only need to specify a prefix of the variable name.

```
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
```

Supplying an empty prompt repeats last command

```
(<0.648.0>)> 
'SSL' = nossl
```

If we know the variable contains a record, we can pretty print it:

```
(<0.660.0>)> pr GS
'GS' =
#gs{gconf =
        #gconf{
            yaws_dir = undefined,trace = false,flags = 64,
            logdir = "./logs",ebin_dir = [],src_dir = [],runmods = [],
    ....
```

After some more `n(ext)` operations, we are here:

```
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
```

We can now step into a function using the `s(tep)` command (a `n(ext)` will not enter a function):

```
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
```

We can go up in the call chain...

```
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
```

...and down again:

```
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
```

We can let the function `f(inish)`:

```
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
```

At this point we realize that we would like to enter a new module, the `yaws_stats` module in the example. So we add that to the debugger using the `i(nterpret)` command:

```
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

(<0.648.0>)> i yaws_stats
Interpreted modules: [yaws_server,yaws_stats]
```

And we can now enter that module:

```
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
```

We can evaluate expressions:

```
(<0.648.0>)> e io:format("Test of eval: ~p~n",[[X*2 || X <- [1,2,3]]]).
Test of eval: [2,4,6]
EVALUATED VALUE:
ok
```

We can list the existing break points:

```
(<0.648.0>)> br

BREAKPOINTS
 yaws_server:1189  Status=active Trigger=enable Cond=null
```

We can list debugged processes:

```
(<0.648.0>)> p
<0.536.0>  yaws_server:gserv_loop/4   waiting
<0.648.0>  yaws_server:acceptor0/2    break       yaws_server:1238
<0.537.0>  yaws_server:POST/4         exit        Reason: shutdown
<0.516.0>  yaws_server:safe_decode_p  idle
<0.663.0>  yaws_server:acceptor0/2    running
```

We can list any module (not just debugged ones):

```
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
```

To see where we are at the moment:

```
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
```

We can change the amount of context to be shown:

```
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

Here is an example of an interactively defined conditional break point (not possible from the standard GUI debugger). First we define the trigger function. It will get the current variable bindings as the argument and must return either `true` OR `false`:

```
1> F = fun(Bindings) -> case int:get_binding('SSL', Bindings) of
                           {value,nossl} -> true;
                           _ -> false
                        end
       end.
#Fun<erl_eval.6.50752066>
```

Now we set our interactively defined break point:

```
2> edbg:it(yaws_server, 1191, F).
ok
```

We run our test...

List the debugged processes:

```
3> edbg:plist().
<0.537.0>  yaws_server:gserv_loop/4   waiting
<0.538.0>  yaws_server:POST/4         exit        Reason: shutdown
<0.517.0>  yaws_server:safe_decode_p  idle
<0.666.0>  yaws_server:acceptor0/2    break       yaws_server:1191
<0.519.0>  yaws_server:handle_call/3  idle
<0.718.0>  yaws_server:acceptor0/2    running
ok
```

Attach ourselves to the first (and in this example, only) process stopped at a break point:

```
4> edbg:a().

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
