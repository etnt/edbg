%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <kruskakli@gmail.com>
%%% @copyright (C) 2024, Torbjorn Tornkvist
%%% @doc Pretty print a tree of linked processes
%%%-------------------------------------------------------------------
-module(edbg_pp_tree).

-export([
    print/1,
    print/2,
    print/3
]).

-define(init_maxdepth(MaxDepth), {0,MaxDepth}).
%%-define(below_maxdepth(D), element(1, D) < element(2, D)).
-define(above_maxdepth(D), element(1, D) > element(2, D)).
-define(inc_depth(D), {element(1, D) + 1, element(2, D)}).


print(Node) ->
    print(Node, []).

print(Node, Xinfo) when is_list(Xinfo) ->
    print(Node, nodepth, Xinfo).

print(Node, MaxDepth, Xinfo) when is_list(Xinfo) ->
    io:format("~p~n", [Node]),
    init_visited(),
    push_visited(Node),
    Children = get_children(Node),
    {ok, 1 + print_children(Children, ?inc_depth(?init_maxdepth(MaxDepth)), ["  |"], Xinfo)}.

print_children(_,Depth, _, _) when ?above_maxdepth(Depth) ->
    0;
print_children([],_, _, _) ->
    0;
print_children([H], Depth, Indent, Xinfo) ->
    pretty_print(H, Indent, Depth, Xinfo, _IsLast = true);
print_children([H | T], Depth, Indent, Xinfo) ->
    N = pretty_print(H, Indent, Depth, Xinfo, _IsLast = false),
    N + print_children(T, Depth, Indent, Xinfo).

pretty_print(Node, RIndent, Depth, Xinfo, IsLast) ->
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
    1 + print_children(Children, ?inc_depth(Depth), R, Xinfo).

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
            ({registered_name, Name}, Acc) ->
                case Name of
                    [] -> Acc;
                    _ -> [io_lib:format(", regname=~p",[Name]) | Acc]
                end;
            ({trap_exit, Bool}, Acc) ->
                case Bool of
                    true -> [io_lib:format(", trap_exit", []) | Acc];
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
