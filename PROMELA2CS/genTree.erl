-module(genTree).
-export([empty_tree/0, make_node/2, add_child/2]).

-record(tree, {value, children = []}).
%空の木を生成
empty_tree() ->
    #tree{}.

%新しいノードの作成
make_node(Value, Children) ->
    #tree{value = Value, children = Children}.

%既存のノードに子を追加
add_child(Tree, Child) ->   %Tree:コピー元, Child:追加する子
    Tree#tree{children = [Child | Tree#tree.children]}.


