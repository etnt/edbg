%%% --------------------------------------------------------------------
%%% Created : 25 Aug 2017 by Torbjorn Tornkvist
%%%
%%% This module is responsible for setting up what we are tracing on
%%% and if/how we should do any kind of filtering of the trace messages.
%%%
%%% Any approved trace message should be sent to the edbg_tracer process
%%% for storage and display.
%%% --------------------------------------------------------------------
-module(edbg_trace_filter).

-export([start_tracer/1]).

-record(ts, {
          n = 0,
          max = 1000,
          mod = false,
          mods = [],
          traced_pids = [],
          opts = [],
          which_pid = all  % all | first
         }).

new_tstate(Mod, Mods, Opts) when is_atom(Mod) andalso
                                 is_list(Mods) andalso
                                 is_list(Opts) ->

    WhichPid = case lists:member(first, Opts) of
                   true  -> first;
                   false -> all
               end,

    #ts{mod       = Mod,
        mods      = Mods,
        opts      = Opts,
        which_pid = WhichPid}.


%% Executed in the context of the edbg_tracer process.
%% @private
start_tracer({Mod, Mods, Opts}) ->
    F = tracer_fun(self()),
    case dbg:tracer(process, {F, new_tstate(Mod, Mods, Opts)}) of
        {ok, _NewTracer} = Res ->
            dbg:p(all, [c,p]),
            [{ok,_} = dbg:tpl(M,'_',[{'_',[],[{return_trace}]}])
             || M <- [Mod|Mods]],
            Res;

        {error, _} = Error ->
            Error
    end.


%% More specific filtering of trace messages
tracer_fun(Pid) when is_pid(Pid) ->
    fun({trace, OldPid, spawn, NewPid, {_M, _F, _Args}},
        #ts{traced_pids = Tpids} = X) ->

            case lists:member(OldPid, Tpids) of
                true ->
                    %%ok = edbg_tracer:send(Pid, {N, Trace}),
                    X#ts{traced_pids = [NewPid|Tpids]};
                false ->
                    X
            end;

       (Trace, #ts{n           = N,
                   traced_pids = Tpids,
                   mod         = Tmod,
                   which_pid   = WhichPid} = X)
       when element(3, Trace) == call orelse
            element(3, Trace) == return_from ->

            Tpid = element(2, Trace),
            Tmfa = element(4, Trace),

            case {lists:member(Tpid, Tpids),Tmfa} of

                %% Trace from 'approved' process to be traced!
                {true,_} ->
                    ok = edbg_tracer:send(Pid, {N, Trace}),
                    X#ts{n = N+1};

                %% All processes where we see traces of 'Tmod' should be traced!
                {false, {Tmod,_,_}} when WhichPid == all ->
                    ok = edbg_tracer:send(Pid, {N, Trace}),
                    X#ts{n = N+1, traced_pids = [Tpid|Tpids]};

                %% Only the first process, where we see any trace of 'Tmod'
                %% should be traced!
                {false, {Tmod,_,_}} when (WhichPid == first) andalso
                                         (Tpids == []) ->
                    ok = edbg_tracer:send(Pid, {N, Trace}),
                    X#ts{n = N+1, traced_pids = [Tpid|Tpids]};

                %% Anything else should not be traced!
                _ ->
                    X
            end;

       %% Anything else should not be traced!
       (_Trace, X) ->
            X
    end.
