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
    Visited = [Node],
    Children = get_children(Node, Visited),
    print_children(Children, ["  |"], Visited, Xinfo).

print_children([], _, _, _) ->
    ok;
print_children([H], Indent, Visited, Xinfo) ->
    pretty_print(H, Indent, Visited, Xinfo, _IsLast = true);
print_children([H | T], Indent, Visited, Xinfo) ->
    pretty_print(H, Indent, Visited, Xinfo, _IsLast = false),
    print_children(T, Indent, Visited, Xinfo).

pretty_print(Node, RIndent, Visited, Xinfo, IsLast) ->
    Indent =
        case IsLast of
            false -> lists:reverse(RIndent);
            true -> lists:reverse(["  +" | tl(RIndent)])
        end,
    io:format("~s--~p ~s~n", [Indent, Node, mk_xinfo(Node, Xinfo)]),
    Children = get_children(Node, Visited),
    R =
        case IsLast of
            true -> ["  |", "   " | tl(RIndent)];
            false -> ["  |" | RIndent]
        end,
    print_children(Children, R, [Node | Visited], Xinfo).

get_children(Node, Visited) ->
    [
        C
     || C <- children(Node),
        false == lists:member(C, Visited)
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
