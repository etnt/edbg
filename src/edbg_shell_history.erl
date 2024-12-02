%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <kruskakli@gmail.com>
%%% @copyright (C) 20224, Torbjorn Tornkvist
%%% @doc The `edbg' shell (hisotry) prompt handling
%%%
%%% Since OTP-26 the Erlang shell was changed so that edbg's prompt-loop
%%% is not added to the shell history. In OTP-28 a io:setopts will be
%%% added as well but until then, the following code could be used.
%%%
%%% See the demo module at:
%%% https://www.erlang.org/doc/apps/stdlib/io_protocol.html#input-requests
%%%
%%%-------------------------------------------------------------------
-module(edbg_shell_history).

-export([until_newline/3,
         get_line/1
        ]).

until_newline(_ThisFar,eof,_MyStopCharacter) ->
    {done,eof,[]};
until_newline(ThisFar,CharList,MyStopCharacter) ->
    case
        lists:splitwith(fun(X) -> X =/= MyStopCharacter end,  CharList)
    of
        {L,[]} ->
            {more,ThisFar++L};
        {L2,[MyStopCharacter|Rest]} ->
            {done,ThisFar++L2++[MyStopCharacter],Rest}
    end.

get_line(Prompt) when is_atom(Prompt) ->
    get_line(Prompt, group_leader()).

get_line(Prompt, IoServer) when is_atom(Prompt) ->
    IoServer ! {io_request,
                self(),
                IoServer,
                {get_until, unicode, Prompt, ?MODULE, until_newline, [$\n]}},
    receive
        {io_reply, IoServer, Data} ->
            Data
    end.
