-module(convartTree).
-export([start/1, start2/1]).

push(Stack, Value) ->
    [Value] ++ Stack.

pop(Stack) ->
    case Stack of
        [H|T] ->
            {H, T}
    end.

%スタックに積んであったものを全部木にする
stackback(Tree, []) ->
    Tree;
    % {ok ,File} = file:open("result_tree.txt", write),
    % io:format(File, "~p~n", [Tree]),
    % file:close(File);
stackback(Tree, Stack) ->
    {H, T} = pop(Stack),
    NewTree = genTree:make_node(H, Tree),
    stackback(NewTree, T).

%子のタプルが入ったリストを受け取ってその要素それぞれをloopに渡して木を作り子の木のリストを作る
listloop([], List, Stack) ->
    List;
listloop(ChildList, List, Stack) ->
    % io:format("~p~n", [hd(ChildList)]),
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
            listloop(tl(ChildList), Children, []);
        %木のリストがあった場合は下請けへ
        [H|T] ->
            TreeList = treelistloop([], [H|T], Stack),
            Children = [TreeList | List],
            listloop(tl(ChildList), Children, [])
    end.

treelistloop(TreeList, [], _) ->
    lists:reverse(TreeList);
treelistloop(TreeList, List, Stack) ->
    Tree = loop(hd(List), Stack),
    NewTreeList = [Tree | TreeList],
    treelistloop(NewTreeList, tl(List), Stack).

start(File) ->
    {ok, F} = file:open(File, read),
    {ok, Tuple} = file:read_line(F),
    io:format("~p~n", [Tuple]),
    file:close(F),
    loop(Tuple, []).

start2(Tuple) ->
    Tree = loop(Tuple, []),
    {ok ,File} = file:open("result_tree.txt", write),
    io:format(File, "~p~n", [Tree]),
    file:close(File).

%Stackに木の親を積んでおいて子の木を下請けに作らせる
%{非終端,{終端}}であれば最後まで子供を作り終えたのでスタックを順にpopしていく
loop({NT, {T}}, Stack) ->
    Tree = genTree:make_node(NT, T),
    stackback(Tree, Stack);
%{非終端,[リスト]}であればlistloopにリストの中の子の木をそれぞれ作ってもらう
loop({NT, [H|T]}, Stack) ->
    Children = listloop([H|T], [], []),
    Children2 = lists:reverse(Children),
    Tree = genTree:make_node(NT, Children2),
    stackback(Tree, Stack);
%{非終端,{}}であれば単に親をスタックに積んで子のタプルをloopへ
loop({NT, Tuple}, Stack) ->
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