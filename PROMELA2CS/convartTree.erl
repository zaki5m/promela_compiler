-module(convartTree).
-export([start/1, start2/1]).

push(Stack, Value) ->
    [Value] ++ Stack.

pop(Stack) ->
    case Stack of
        [H|T] ->
            {H, T}
    end.

stackback(Tree, []) ->
    Tree;
stackback(Tree, Stack) ->
    {H, T} = pop(Stack),
    NewTree = genTree:make_node(H, Tree),
    stackback(NewTree, T).


listloop([], List, Stack) ->
    List;
listloop(ChildList, List, Stack) ->
    case hd(ChildList) of
        {T} ->
            Tree = genTree:make_node(T, []),
            Children = [Tree | List],
            listloop(tl(ChildList), Children, []);
        {NT, {T}} ->
            Tree = loop({NT, {T}}, Stack),
            Children = [Tree | List],
            listloop(tl(ChildList), Children, []);
        {NT, [H|T]} ->
            Tree = loop({NT, [H|T]}, Stack),
            Children = [Tree | List],
            listloop(tl(ChildList), Children, []);
        {NT, Tuple} ->
            Tree = loop({NT, Tuple}, Stack),
            Children = [Tree | List],
            listloop(tl(ChildList), Children, [])
    end.

start(File) ->
    {ok, F} = file:open(File, read),
    {ok, Tuple} = file:read_line(F),
    io:format("~p~n", [Tuple]),
    file:close(F),
    loop(Tuple, []).

start2(Tuple) ->
    loop(Tuple, []).

loop({NT, {T}}, Stack) ->
    io:format("AA~n"),
    Tree = genTree:make_node(NT, T),
    stackback(Tree, Stack);
loop({NT, [H|T]}, Stack) ->
    io:format("CC~n"),
    Children = listloop([H|T], [], []),
    Children2 = lists:reverse(Children),
    Tree = genTree:make_node(NT, Children2),
    stackback(Tree, Stack);
loop({NT, Tuple}, Stack) ->
    io:format("BB~n"),
    NewStack = push(Stack, NT),
    loop(Tuple, NewStack).










% enqueue(Queue, Node) ->
%     Queue ++ [Node].



% elinqueue([], Queue) ->
%     Queue;
% elinqueue(List, Queue) ->
%     NewQueue = enqueue(Queue, hd(List)),
%     elinqueue(lists:last(List) , NewQueue).


% loop(List, Stack) ->
%     case length(lists:last(List)) of
%         1 ->
%             NewStack = push(Stack, hd(List)),
%             loop(lists:last(List), NewStack);
%         Over2 ->
%             Queue = elinqueue(lists:last(List), []),
%             queueloop(Queue, []),
%             NewStack = push()