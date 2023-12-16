-module(pml2cs).
-export([start/1, sequence/5]).
-import(lists, [map/2]).
-include("record.hrl").

start(Tree) ->
    spec(Tree).

spec(Tree) ->
    Module = Tree#tree.children,
    module(Module).

% list([module]) -> atom
% Moduleのリストの各要素を見てプロセスならPG生成の関数へ
module([]) ->
    fin;
module([Module|Modules]) ->
    Child = Module#tree.children,
    case Child of
        [_|_] ->
            io:format("hensuu sengen desu~n");
        %プロセスならPG生成の関数へ
        Proc when Proc#tree.value =:= proctype orelse Proc#tree.value =:= init ->
            Pid = spawn(fun setmanager:start/0),
            genPG(Child, Pid);
        Decl when Decl#tree.value =:= utype orelse Decl#tree.value =:= mtype ->
            io:format("declaration desu~n");
        _ ->
            io:format("module is neither a process nor declaration: ~p~n", [Child#tree.value])
    end,
    module(Modules).

%record(tree) -> pid -> atom
genPG(Proc, Pid) ->
    ProcChild = Proc#tree.children,
    {Fin, NewI} = my_utility:listloop(ProcChild, Pid, 0, fin),       %proctype/initの子供のリストから[step]を見つけ出してpml2csに返す関数を呼びだし
    % プロセスが終了するならexitに枝をはる
    case Fin of
        fin ->
            my_utility:genedge(NewI, {true,skip}, exit, Pid);
        notfin ->
            pass
    end,
    Pid ! {self(), fin}.

% List([step]) -> Nat -> record -> pid -> Nat -> tuple
%stepのリストを受け取り各stepをstepを解析する関数へ渡し戻り値によって枝を次のLocにはるか判断する
sequence([], _, _, _, I) ->
    {fin, I, nonbreak};
sequence([Step|Steps], Source, Guard, Pid, I) ->
    my_utility:addloc(I, Pid),
    {StepAtom, Result} = step(Step, Pid, I),
    case StepAtom of
        stmnt2 ->       %do
            {NewI, Flag} = Result,
            %doの中にbreakがない場合は無限ループするのでこれ以降のstepに枝をはらない
            case Flag of
                nonbreak ->
                    {notfin, NewI, nonbreak};
                break ->
                    % my_utility:genedge(I, {true,skip}, NewI, Pid),
                    sequence(Steps, NewI, true, Pid, NewI)
            end;
        stmnt10 ->                  %break
            Act = Result,
            my_utility:addact({Guard,Act}, Pid),
            NewI = I + 1,
            my_utility:genedge(Source, {Guard,Act}, NewI, Pid),
            {fin, NewI, break};
        stmnt81314 ->
            Act = Result,
            my_utility:addact({Guard,Act}, Pid),
            NewI = I + 1,
            my_utility:genedge(Source, {Guard,Act}, NewI, Pid),
            sequence(Steps, NewI, true, Pid, NewI);
        one_decl ->
            Act = Result,
            my_utility:addact({Guard,Act}, Pid),
            NewI = I + 1,
            my_utility:genedge(Source, {Guard,Act}, NewI, Pid),
            sequence(Steps, NewI, true, Pid, NewI)
    end.

%stepを解析する関数,step1ならstmntを解析する関数へ，step2ならone_declを解析する関数へ
% tuple(step tree) -> tuple({stepがなにか, Reult})
step(Step, Pid, I) ->
    case Step#tree.value of
        step1 ->
            [Stmnt|StmntOpt] = Step#tree.children,
            {StmntAtom, Result} = stmnt(Stmnt, Pid, I),
            {StmntAtom, Result};
        step2 ->
            Result = one_decl(Step#tree.children, Pid, I),
            Result
    end.
% list(one_decl) -> pid -> Nat -> tuple
one_decl([One_decl], Pid, I) ->
    io:format("one_decl~n"),
    Act = One_decl#tree.children,
    {one_decl, Act}.

%tuple(tree record) -> tuple({atom, Result})
stmnt(Stmnt, Pid, I_) when Stmnt#tree.value == stmnt1->      %stmnt1: if
    io:format("stmnt1~n");
stmnt(Stmnt, Pid, I) when Stmnt#tree.value == stmnt2 ->     %stmnt2: do
    io:format("stmnt2~n"),
    Result = options(Stmnt#tree.children,I, [], Pid, I, nonbreak),    %Result -> {NewI, BreakFlag}
    {stmnt2, Result};
stmnt(Stmnt, Pid, I) when Stmnt#tree.value == stmnt4 ->     %stmnt4: atomic
    io:format("stmnt4~n");
stmnt(Stmnt, Pid, I) when Stmnt#tree.value == stmnt9 ->     %stmnt9: else
    io:format("stmnt9~n");
stmnt(Stmnt, Pid, I) when Stmnt#tree.value == stmnt10 ->    %stmnt10: break
    io:format("stmnt10~n"),
    Act = Stmnt,
    {stmnt10, Act};
stmnt(Stmnt, Pid, I) when Stmnt#tree.value == stmnt8 orelse Stmnt#tree.value == stmnt13 orelse Stmnt#tree.value == stmnt14 ->       %stmnt8: send receive assign expr, stmnt13: printm stmnt14: printf
    io:format("stmnt8or13or14:~p~n", [Stmnt]),
    Act = Stmnt,
    {stmnt81314, Act}.

stmnt4guard(Stmnt4guard, Pid, I) when Stmnt4guard#tree.value == stmnt4guard1->
    Stmnt4guard#tree.children;
stmnt4guard(Stmnt4guard, Pid, I) when Stmnt4guard#tree.value == stmnt4guard2->
    Stmnt4guard#tree.children.

% list([optionseq]) -> tuple({optionの中で進めた分だけインクリメントしたI, Flag})
% optionseqの下の[step]の最後のstepの遷移先をスタックにつんで最後にスタックの中身のエッジをはる(Targetはdoの繰り返し先)
% 第五引数のFlagはbreakを含んでいるかどうか
options([], Source, Stack, Pid, I, Flag) ->
    NewI = my_utility:genedgeStack(Stack, Source, Pid, I),
    {NewI, Flag};
options([Optionseq|Optionseqs], Source, Stack, Pid, I, Flag) ->               %breakの場合はスタックに積まずに再帰
    Result = optionseq(Optionseq, Source, Pid, I),
    {_, NewI, BreakFlag} = Result,
    NewStack = my_utility:push(Stack, {NewI, BreakFlag}),
    io:format("Flag:~p~n", [Flag]),
    case Flag of
        nonbreak ->
            options(Optionseqs, Source, NewStack, Pid, NewI, BreakFlag);
        break ->
            options(Optionseqs, Source, NewStack, Pid, NewI, Flag)
    end.

% record -> Nat -> pid -> Nat -> tuple
% [step]の最後の要素のstepのActを返す
optionseq(Optionseq, Source, Pid, I) ->
    [GuardTree|StepList] = Optionseq#tree.children,
    Guard = guard(GuardTree, Pid, I),
    Result = sequence(hd(StepList), Source, Guard, Pid, I),     %Result->{fin, NewI, BreakFlag}
    Result.

guard(GuardTree, Pid, I) ->
    [Stmnt4guard|GuardOpt] = GuardTree#tree.children,
    Guard = stmnt4guard(Stmnt4guard, Pid, I),
    Guard.