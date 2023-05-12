%%% @private
%%
%% Custom colors may be set using the environment variable EDBG_COLOR.
%%
%% It should contain a SPACE separated string with ITEM=COLOR entries.
%%
%% Valid ITEMs are:
%%
%%   att    - attention color    (default: whiteb)
%%   warn   - warning color      (default: yellow)
%%   err    - error color        (default: red)
%%   cur    - current line color (default: green)
%%
%% Valid COLORs are:
%%
%%   black,   blackb
%%   red,     redb
%%   green,   greenb
%%   yellow,  yellowb
%%   blue,    blueb
%%   magenta, magentab
%%   cyan,    cyanb
%%   white,   whiteb
%%
%% Colors ending in 'b' are the bright variants.
%%
%% Example:
%%
%%   export EDBG_COLOR="att=yellowb warn=magenta"
%%
%% Colors not specified in EDBG_COLOR will keep their defaults.
%%
-module(edbg_color_srv).

-export([init/0,
         info_msg/2,
         att_msg/2,
         warn_msg/2,
         err_msg/2,
         cur_line_msg/2,
         info_str/2,
         att_str/2,
         warn_str/2,
         err_str/2,
         cur_line_str/2,
         c_hi/1,
         c_warn/1,
         c_err/1,
         help_hi/1]).

-define(SERVER, ?MODULE).

%%
%% PUBLIC API
%%

%% @private
info_msg(Fmt, Args) ->
    io:format(info_str(Fmt, Args)).

%% @private
att_msg(Fmt, Args) ->
    io:format(att_str(Fmt, Args)).

%% @private
warn_msg(Fmt, Args) ->
    io:format(warn_str(Fmt, Args)).

%% @private
err_msg(Fmt, Args) ->
    io:format(err_str(Fmt, Args)).

%% @private
cur_line_msg(Fmt, Args) ->
    io:format(cur_line_str(Fmt, Args)).

%% @private
info_str(Fmt, Args) ->
    call({info_str, Fmt, Args}).

%% @private
att_str(Fmt, Args) ->
    call({att_str, Fmt, Args}).

%% @private
warn_str(Fmt, Args) ->
    call({warn_str, Fmt, Args}).

%% @private
err_str(Fmt, Args) ->
    call({err_str, Fmt, Args}).

%% @private
cur_line_str(Fmt, Args) ->
    call({cur_line_str, Fmt, Args}).

%% @private
c_hi(Str) ->
    call({c_hi, Str}).

%% @private
c_warn(Str) ->
    call({c_warn, Str}).

%% @private
c_err(Str) ->
    call({c_err, Str}).

%% @private
help_hi(Str) ->
    %% highligt character(s) inside parentheses
    F = fun($), {normal, _Collect, Acc}) ->
                {in_parentheses, [], Acc};
           ($(, {in_parentheses, Collect, Acc}) ->
                Hi = [$(, c_hi(Collect), $)],
                {normal, [], [Hi | Acc]};
           (C, {M = in_parentheses, Collect, Acc}) ->
                {M, [C | Collect], Acc};
           (C, {M = normal, [], Acc}) ->
                {M, [], [C | Acc]}
        end,
    {_, _, Res} = lists:foldr(F, {normal, [], []}, lists:flatten(Str)),
    Res.

%% @private
init() ->
    case whereis(?SERVER) of
        undefined ->
            ok;
        Pid0 when is_pid(Pid0) ->
            unregister(?SERVER),
            Pid0 ! quit
    end,
    Pid = spawn(fun() -> color_srv() end),
    register(?SERVER, Pid),
    ok.

%%
%% INTERNAL
%%

color_srv() ->
    CMap0 = #{att => whiteb,
              err => red,
              warn => yellow,
              cur => green},

    CMap = case os:getenv("EDBG_COLOR") of
            false ->
                CMap0;
            ColorStr ->
                parse_env(ColorStr, CMap0)
        end,
    color_server_loop(CMap).

parse_env(ColorStr, CMap) ->
    parse_env_c(string:tokens(ColorStr, " "), CMap).

parse_env_c(["att="++Color|T], CMap) ->
    parse_env_c(T, set_color(att, Color, CMap));
parse_env_c(["warn="++Color|T], CMap) ->
    parse_env_c(T, set_color(warn, Color, CMap));
parse_env_c(["err="++Color|T], CMap) ->
    parse_env_c(T, set_color(err, Color, CMap));
parse_env_c(["cur="++Color|T], CMap) ->
    parse_env_c(T, set_color(cur, Color, CMap));
parse_env_c([Item|T], CMap) ->
    io:format(
      "warning: unknown option ~p in EDBG_COLOR - ignoring~n"
      "         valid color items are ~p~n", [Item, [att, warn, err, cur]]),
    parse_env_c(T, CMap);
parse_env_c([], CMap) ->
    CMap.

set_color(Item, Color, CMap) ->
    ValidColors = [black, blackb, red, redb, green, greenb, yellow,
                   yellowb, blue, blueb, magenta, magentab, cyan,
                   cyanb, white, whiteb],

    AColor = list_to_atom(Color),
    case lists:member(AColor, ValidColors) of
        true ->
            CMap#{Item => AColor};
        false ->
            io:format("warning: invalid color \"" ++ Color ++ "\""
                      " in EDBG_COLOR - ignoring~n"
                      "         valid colors are ~p", [ValidColors]),
            CMap
    end.

color_server_loop(CMap) ->
    receive
        quit ->
            exit(normal);
        {{info_str, Fmt, Args}, From, Ref} ->
            R = io_lib:format(lists:flatten(Fmt), Args),
            reply(From, Ref, R);
        {{att_str, Fmt, Args}, From, Ref} ->
            R = color_str(maps:get(att, CMap), Fmt, Args),
            reply(From, Ref, R);
        {{warn_str, Fmt, Args}, From, Ref} ->
            R = color_str(maps:get(warn, CMap), Fmt, Args),
            reply(From, Ref, R);
        {{err_str, Fmt, Args}, From, Ref} ->
            R = color_str(maps:get(err, CMap), Fmt, Args),
            reply(From, Ref, R);
        {{cur_line_str, Fmt, Args}, From, Ref} ->
            R = color_str(maps:get(cur, CMap), Fmt, Args),
            reply(From, Ref, R);
        {{c_hi, Str}, From, Ref} ->
            C = maps:get(att, CMap),
            reply(From, Ref, edbg_color:C(Str));
        {{c_warn, Str}, From, Ref} ->
            C = maps:get(warn, CMap),
            reply(From, Ref, edbg_color:C(Str));
        {{c_err, Str}, From, Ref} ->
            C = maps:get(err, CMap),
            reply(From, Ref, edbg_color:C(Str));
        _ ->
            undefined
    end,
    color_server_loop(CMap).

call(Msg) ->
    call(?SERVER, Msg).

call(Pid, Msg) ->
    Ref = make_ref(),
    Pid ! {Msg, self(), Ref},
    receive
        {Ref, Response} ->
            Response
    end.

reply(Pid, Ref, Term) ->
    Pid ! {Ref, Term}.

color_str(Color, Fmt, Args) when is_atom(Color) ->
    io_lib:format(lists:flatten(edbg_color:Color(Fmt)), Args).
