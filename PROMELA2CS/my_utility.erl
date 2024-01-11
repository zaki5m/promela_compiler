-module(my_utility).
-export([push/2, pop/1, genedgeStack/5,listloop/5, genedge/4, checkminus1/1, checkedge/2, addloc/2, addact/2, addchan/2, changeminusedge/2, addmtype/2]).
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
genedgeStack([], _, Pid, I, _) ->
    addloc(I+1, Pid),
    I + 1;
genedgeStack(Stack, Target, Pid, I, MinimumNum) ->
    {{Source, BreakFlag}, T} = pop(Stack),
    % io:format("NewI:~pFlag:~p~n", [Source, BreakFlag]),
    case BreakFlag of
        nonbreak ->
            genedge(Source, {true,skip}, Target, Pid),
            genedgeStack(T, Target, Pid, I, MinimumNum);
        break ->
            genedge(Source, {true,skip}, I+1, Pid),
            genedgeStack(T, Target, Pid, I, MinimumNum);
        ifbreak ->
            genedge(Source, {true,skip}, MinimumNum, Pid),
            genedgeStack(T, Target, Pid, I, MinimumNum)
    end.

%proctypeの子供のリストから[step]を見つけ出してpml2csに返す
listloop([], _, I, Fin, Name) ->
    {Fin, I, Name};
listloop([H|[]], Pid, I, Fin, Name) ->
    {NewFin, NewI, _} = pml2cs:sequence(H, I, true, Pid, I, firstcall),
    listloop([], Pid, NewI, NewFin, Name);
listloop([H|T], Pid, I, Fin, Name) ->
    case is_list(H) of
        true ->
            Tmp = hd(H),
            case Tmp#tree.value of
                name ->
                    NewName = Tmp#tree.children,
                    listloop(T, Pid, I, Fin, NewName);
                _ ->
                    listloop(T, Pid, I, Fin, Name)
            end;
            % io:format("H:~p~n", [H]),
        false ->
            % io:format("H:~p~n", [H]),
            listloop(T, Pid, I, Fin, Name)
    end.

changeminusedge(Pid, Target) ->
    Pid ! {self(), {changeminus, Target}},
    receive
        {Pid, ok} ->
            ok
    end.

genedge(Source, Act, Target, Pid) ->
    Pid ! {self(), {edge, {Source, Act, Target}}},
    receive
        {Pid, ok} ->
            ok
    end.

checkminus1(Pid) ->
    Pid ! {self(), {checkminus1}},
    receive
        {Pid, Result} ->
            Result
    end.

checkedge(Edge, Pid) ->
    Pid ! {self(), {check, Edge}},
    receive
        {Pid, Result} ->
            Result
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

addchan(Chan, Pid) ->
    Pid ! {self(), {chan, Chan}},
    receive
        {Pid, ok} ->
            ok
    end.

addmtype(Mtype, Pid) ->
    Pid ! {self(), {mtype, Mtype}},
    receive
        {Pid, ok} ->
            ok
    end.