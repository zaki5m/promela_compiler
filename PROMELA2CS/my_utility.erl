-module(my_utility).
-export([push/2, pop/1, genedgeStack/6,listloop/3, genedge/4, addloc/2, addact/2]).
-include("record.hrl").

push(Stack, Value) ->
    [Value] ++ Stack.

pop(Stack) ->
    case Stack of
        [H|T] ->
            {H, T}
    end.

% 第5引数の0/1はBreakFlagが１のやつがスタックの中にあったかどうかで切り替わる
% あったばあいはbreakしたあとのIを返す
genedgeStack([], _, _, I, 0, _) ->
    I;
genedgeStack([], _, _, I, 1, 0) ->
    I + 1;
genedgeStack([], _, _, I, 1, 1) ->
    I;
genedgeStack(Stack, Target, Pid, I, 0, SeqFlag) ->
    {{Source, Act, BreakFlag},T} = pop(Stack),
    addact(Act, Pid),
    case BreakFlag of
        0 ->
            genedge(Source, Act, Target, Pid),
            genedgeStack(T, Target, Pid, I, 0, SeqFlag);
        1 ->
            case SeqFlag of
                0 ->
                    genedge(Source, Act, I+1, Pid),
                    genedgeStack(T, Target, Pid, I, 1, SeqFlag);
                1 ->
                    genedge(Source, Act, Target, Pid),
                    genedgeStack(T, Target, Pid, I, 1, SeqFlag)
            end
    end;
genedgeStack(Stack, Target, Pid, I, 1, SeqFlag) ->
    {{Source, Act, BreakFlag},T} = pop(Stack),
    addact(Act, Pid),
    case BreakFlag of
        0 ->
            genedge(Source, Act, Target, Pid),
            genedgeStack(T, Target, Pid, I, 1, SeqFlag);
        1 ->
            case SeqFlag of
                0 ->
                    genedge(Source, Act, I+1, Pid),
                    genedgeStack(T, Target, Pid, I, 1, SeqFlag);
                1 ->
                    genedge(Source, Act, Target, Pid),
                    genedgeStack(T, Target, Pid, I, 1, SeqFlag)
            end
    end.
    % case Stack of
    %     [_|[]] ->
    %         fin;
    %     [_|T] ->
    %         genedgeStack(T, Target, Pid)
    % end.

%proctypeの子供のリストから[step]を見つけ出してpml2csに返す
listloop([], _, _) ->
    fin;
listloop([H|[]], Pid, I) ->
    pml2cs:sequence(H, Pid, I),
    listloop([], Pid, I);
listloop([H|T], Pid, I) ->
    io:format("H:~p~n", [H]),
    listloop(T, Pid, I).

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