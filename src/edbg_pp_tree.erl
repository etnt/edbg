%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <kruskakli@gmail.com>
%%% @copyright (C) 2024, Torbjorn Tornkvist
%%% @doc Pretty print a tree of linked processes
%%%-------------------------------------------------------------------
-module(edbg_pp_tree).

-export([
    print/1,
    print/2
]).

print(Node) ->
    print(Node, []).

print(Node, Xinfo) ->
    io:format("~p~n", [Node]),
    init_visited(),
    push_visited(Node),
    Children = get_children(Node),
    {ok, 1 + print_children(Children, ["  |"], Xinfo)}.

print_children([], _, _) ->
    0;
print_children([H], Indent, Xinfo) ->
    pretty_print(H, Indent, Xinfo, _IsLast = true);
print_children([H | T], Indent, Xinfo) ->
    N = pretty_print(H, Indent, Xinfo, _IsLast = false),
    N + print_children(T, Indent, Xinfo).

pretty_print(Node, RIndent, Xinfo, IsLast) ->
    Indent =
        case IsLast of
            false -> lists:reverse(RIndent);
            true -> lists:reverse(["  +" | tl(RIndent)])
        end,
    io:format("~s--~p ~s~n", [Indent, Node, mk_xinfo(Node, Xinfo)]),
    Children = get_children(Node),
    R =
        case IsLast of
            true -> ["  |", "   " | tl(RIndent)];
            false -> ["  |" | RIndent]
        end,
    push_visited(Node),
    1 + print_children(Children, R, Xinfo).

get_children(Node) ->
    [
        C
     || C <- children(Node),
        false == is_visited(C)
    ].

children(Node) when is_pid(Node) orelse is_port(Node) ->
    try erlang:process_info(Node, links) of
        {links, Links} -> Links
    catch
        _:_ -> []
    end.

mk_xinfo(_Node, []) ->
    "";
mk_xinfo(Node, _) when is_port(Node) ->
    "";
mk_xinfo(Node, L) ->
    Pinfo = process_info(Node, L),
    lists:foldr(
        fun
            ({current_function, {M, F, A}}, Acc) ->
                [io_lib:format(", current=~p:~p/~p", [M, F, A]) | Acc];
            ({initial_call, {M, F, A}}, Acc) ->
                [io_lib:format(", init=~p:~p/~p", [M, F, A]) | Acc];
            ({trap_exit, Bool}, Acc) ->
                case Bool of
                    true -> [io_lib:format(", trap_exit=true", []) | Acc];
                    _ -> Acc
                end;
            ({Key, Val}, Acc) ->
                [io_lib:format(", ~p=~p", [Key, Val]) | Acc]
        end,
        [],
        Pinfo
    ).


init_visited() ->
    erase(visited),
    put(visited, sets:new()).

push_visited(Node) ->
    Visited = get(visited),
    put(visited, sets:add_element(Node, Visited)).

is_visited(Node) ->
    Visited = get(visited),
    sets:is_element(Node, Visited).
