-module(edbg_pp_tree).
-export([print/1]).

% Recursive function to pretty print the tree
print(Node) ->
    io:format("~p~n", [Node]),
    Visited = [Node],
    Children = get_children(Node, Visited),
    print_children(Children, ["  |"], Visited).

print_children([], _, _) ->
    ok;
print_children([H], Indent, Visited) ->
    pretty_print(H, Indent, Visited, _IsLast = true);
print_children([H|T], Indent, Visited) ->
    pretty_print(H, Indent, Visited, _IsLast = false),
    print_children(T, Indent, Visited).


pretty_print(Node, RIndent, Visited, IsLast) ->
    Indent = case IsLast of 
                false -> lists:reverse(RIndent);
                true  -> lists:reverse(["  +" | tl(RIndent)])
             end,
    io:format("~s--~p~n", [Indent, Node]),
    Children = get_children(Node, Visited),
    R = case IsLast of
            true  -> ["  |" , "   " | tl(RIndent)];
            false -> ["  |" | RIndent]
        end,
    print_children(Children, R, [Node | Visited]).
 

get_children(Node, Visited) ->
    [C || C <- children(Node),
          false == lists:member(C,Visited)].

children(Node) when is_pid(Node) orelse is_port(Node) ->
    try erlang:process_info(Node, links) of
        {links, Links} -> Links
    catch
        _:_ -> []
    end.
