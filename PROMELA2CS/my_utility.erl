-module(my_utility).
-export([push/2, pop/1, genedgeStack/4,listloop/4, genedge/4, addloc/2, addact/2]).
-include("record.hrl").

push(Stack, Value) ->
    [Value] ++ Stack.

pop(Stack) ->
    case Stack of
        [H|T] ->
            {H, T}
    end.

% list -> Nat -> pid -> Nat -> Nat
% スタックに積まれたstepの枝をはる関数，breakの場合は一個すすめbreakがない場合はTargetで指定したLocに戻る
genedgeStack([], _, _, I) ->
    I + 1;
genedgeStack(Stack, Target, Pid, I) ->
    {{Source, BreakFlag}, T} = pop(Stack),
    io:format("NewI:~pFlag:~p~n", [Source, BreakFlag]),
    case BreakFlag of
        nonbreak ->
            genedge(Source, {true,skip}, Target, Pid),
            genedgeStack(T, Target, Pid, I);
        break ->
            genedge(Source, {true,skip}, I+1, Pid),
            genedgeStack(T, Target, Pid, I)
    end.

%proctypeの子供のリストから[step]を見つけ出してpml2csに返す
listloop([], _, I, Fin) ->
    {Fin, I};
listloop([H|[]], Pid, I, Fin) ->
    {NewFin, NewI, _} = pml2cs:sequence(H, I, true, Pid, I, firstcall),
    listloop([], Pid, NewI, NewFin);
listloop([H|T], Pid, I, Fin) ->
    io:format("H:~p~n", [H]),
    listloop(T, Pid, I, Fin).

genedge(Source, Act, Target, Pid) ->
    Pid ! {self(), {arrow, {Source, Act, Target}}},
    receive
        {Pid, ok} ->
            ok
    end.

addloc(I, Pid) ->
    Pid ! {self(), {loc, I}},
    receive
        {Pid, ok} ->
            ok
    end.

addact(Act, Pid) ->
    Pid ! {self(), {act, Act}},
    receive
        {Pid, ok} ->
            ok
    end.