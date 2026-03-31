%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <kruskakli@gmail.com>
%%% @copyright (C) 20224, Torbjorn Tornkvist
%%% @doc The `edbg' shell (hisotry) prompt handling
%%%
%%% Since OTP-26 the Erlang shell was changed so that edbg's prompt-loop
%%% is not added to the shell history.
%%%
%%% - OTP >= 28: use io:setopts([{line_history, true}]) + io:get_line/1
%%% - OTP 26-27: use get_until IO protocol trick
%%% - OTP < 26: plain io:get_line/1 works
%%%
%%% See the demo module at:
%%% https://www.erlang.org/doc/apps/stdlib/io_protocol.html#input-requests
%%%
%%%-------------------------------------------------------------------
-module(edbg_shell_history).

-export([until_newline/3,
         get_line/1,
         get_line/2,
         enable/0
        ]).

until_newline(_ThisFar, eof, _MyStopCharacter) ->
    {done, eof, []};
until_newline(ThisFar, CharList, MyStopCharacter) ->
    case
        lists:splitwith(fun(X) -> X =/= MyStopCharacter end, CharList)
    of
        {L, []} ->
            {more, ThisFar ++ L};
        {L2, [MyStopCharacter | Rest]} ->
            {done, ThisFar ++ L2 ++ [MyStopCharacter], Rest}
    end.

%% @doc Enable shell history for the current process.
%%
%% On OTP >= 28 this calls io:setopts([{line_history, true}]).
%% On older versions it is a no-op (the get_until trick is used
%% transparently in get_line/1 instead).
%% @end
enable() ->
    case otp_release() of
        Vsn when Vsn >= 28 ->
            io:setopts([{line_history, true}]);
        _ ->
            ok
    end.

%% @doc Read a line from the user, with shell history support.
%%
%% On OTP >= 28, assumes enable/0 has been called so that
%% io:get_line adds input to shell history (with arrow key support).
%% On OTP 26-27, uses the get_until IO protocol request.
%% On older OTP versions, plain io:get_line/1 is used.
%% The Prompt can be either a string or an atom.
%% @end
get_line(Prompt) ->
    case otp_release() of
        Vsn when Vsn >= 28 ->
            %% io:setopts([{line_history, true}]) should have been
            %% called via enable/0 before entering the prompt loop.
            io:get_line(Prompt);
        Vsn when Vsn >= 26 ->
            get_line_via_protocol(to_atom(Prompt));
        _ ->
            io:get_line(Prompt)
    end.

get_line(Prompt, IoServer) when is_atom(Prompt) ->
    get_line_via_protocol(Prompt, IoServer).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

otp_release() ->
    try list_to_integer(erlang:system_info(otp_release))
    catch _:_ -> 0
    end.

to_atom(Prompt) when is_atom(Prompt) -> Prompt;
to_atom(Prompt) when is_list(Prompt) -> list_to_atom(Prompt).

get_line_via_protocol(Prompt) when is_atom(Prompt) ->
    get_line_via_protocol(Prompt, group_leader()).

get_line_via_protocol(Prompt, IoServer) when is_atom(Prompt) ->
    IoServer ! {io_request,
                self(),
                IoServer,
                {get_until, unicode, Prompt, ?MODULE, until_newline, [$\n]}},
    receive
        {io_reply, IoServer, Data} ->
            Data
    end.
