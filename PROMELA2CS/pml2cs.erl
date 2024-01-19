-module(pml2cs).
-export([start/1, start2/1, sequence/6]).
-import(lists, [map/2]).
-include("record.hrl").

start(Tree) ->
    {Numproc, PGList, ChanList, MtypeList, GlobalVarList} = spec(Tree),
    % io:format("ChanList:~p~nMtypeList:~p~nGlobalVarList:~p~n", [ChanList, MtypeList, GlobalVarList]),
    {ok, Out1} = file:open("CSedges.out", write),
    file:write_file("CSedges", PGList),
    file:close(Out1),
    cs2csprime:start(Numproc, PGList, ChanList, MtypeList, GlobalVarList).

start2(Tree) ->
    {_, PGList, ChanList, MtypeList, GlobalVarList} = spec(Tree),
    io:format("~p~n", [PGList]),
    % {ok, Out1} = file:open("CSedges.out", write),
    % file:write_file("CSedges", PGList),
    % file:close(Out1),
    cs2erl:start(PGList, ChanList, MtypeList).

spec(Tree) ->
    Module = Tree#tree.children,
    {Numproc, PGList, ChanList, MtypeList, GlobalVarList} = module(Module, 0, [], [], [], []),
    {Numproc, PGList, ChanList, MtypeList, GlobalVarList}.

% list([module]) -> atom
% Moduleのリストの各要素を見てプロセスならPG生成の関数へ
module([], Numproc, PGList, ChanList, MtypeList, GlobalVarList) ->
    {Numproc, PGList, ChanList, MtypeList, GlobalVarList};
module([Module|Modules], Numproc, PGList, ChanList, MtypeList, GlobalVarList) ->
    Child = Module#tree.children,
    case Child of
        [_|_] ->                                        % decl_lst
            Pid = spawn(fun() -> setmanager:addchan([]) end),
            NewGlobalVarList = decl_lst(Child, GlobalVarList, Pid),
            Pid ! {self(), fin},
            receive
                {Pid, Chan} ->
                    NewChanList = ChanList ++ Chan
            end,
            module(Modules, Numproc, PGList, NewChanList, MtypeList, NewGlobalVarList);
        %プロセスならPG生成の関数へ
        Proc when Proc#tree.value =:= proctype orelse Proc#tree.value =:= init ->       %proctype, init
            Pid = spawn(fun setmanager:start/0),
            ProcName = genPG(Child, Pid),
            Pid ! {self(),fin},
            receive
                {Pid, {Loc, Act, Arrow}} ->
                PG = {ProcName, {Loc, Act, Arrow}},
                NewPGList = PGList ++ [PG]
            end,
            module(Modules, Numproc+1, NewPGList, ChanList, MtypeList, GlobalVarList);
        Decl when Decl#tree.value =:= mtype ->     % utype, mtype
            Pid = spawn(fun() -> setmanager:addmtype([]) end),
            NameList = Decl#tree.children,
            mtype(hd(NameList), Pid),
            Pid ! {self(), fin},
            receive
                {Pid, Mtype} ->
                    NewMtypeList = MtypeList ++ Mtype
            end,
            module(Modules, Numproc, PGList, ChanList, NewMtypeList, GlobalVarList);
        _ ->
            io:format("module is neither a process nor declaration: ~p~n", [Child#tree.value])
    end.
    % Pid ! {self(), fin},
    % receive
    %     {Pid, {Loc, Act, Arrow, Chan}} ->
    %         io:format("aaa"),
    % % {Loc, _, Arrow} = Pid ! {self(), fin},
    %     % arrange_edgelist:start(Loc, Arrow)
    %         PG = {Numproc, {Loc, Act, Arrow, Chan}},
    %         NewPGList = PGList ++ [PG]
    % end,
mtype([], _) ->
    fin;
mtype([NameTree|Names], Pid) ->
    Name = NameTree#tree.children,
    my_utility:addmtype(Name, Pid),
    mtype(Names, Pid).

decl_lst([], GlobalVarList, _) ->
    GlobalVarList;
decl_lst([One_decl|One_decls], GlobalVarList, Pid) ->
    One_declChild  = One_decl#tree.children,
    io:format("One_declChild:~p~n", [One_declChild]),
    TypenameTree = hd(lists:nth(2, One_declChild)),
    case TypenameTree#tree.children of
        chan ->
            Ivar = lists:nth(3, One_declChild),
            ivar(Ivar, Pid, chan),
            decl_lst(One_decls,GlobalVarList, Pid);
        _ ->
            Ivar = lists:nth(3, One_declChild),
            NewGlobalVarList = ivar(Ivar, Pid, GlobalVarList),
            decl_lst(One_decls,NewGlobalVarList, Pid)
    end.

ivar([], _, chan) ->
    [];
ivar([], _, GlobalVarList) ->
    GlobalVarList;
ivar([Ivar|Ivars], Pid, chan) ->
    IvarChild = Ivar#tree.children,
    channame(hd(IvarChild), Pid),
    ivar(Ivars, Pid, chan);
ivar([Ivar|Ivars], Pid, GlobalVarList) ->
    IvarChild = Ivar#tree.children,
    % io:format("IvarChild:~p~n", [IvarChild]),
    Tmp1 = hd(IvarChild),
    Tmp2 = hd(Tmp1),
    Varname = Tmp2#tree.children,
    NewGlobalVarList = [Varname | GlobalVarList],
    ivar(Ivars, Pid, NewGlobalVarList).

channame([], _) ->
    fin;
channame([Name|Names], Pid) ->
    my_utility:addchan(Name#tree.children, Pid),
    channame(Names, Pid).

%record(tree) -> pid -> atom
genPG(Proc, Pid) ->
    ProcChild = Proc#tree.children,
    my_utility:addloc(0, Pid),
    {Fin, NewI, ProcName} = my_utility:listloop(ProcChild, Pid, 0, fin, name),       %proctype/initの子供のリストから[step]を見つけ出してpml2csに返す関数を呼びだし
    % プロセスが終了するならexitに枝をはる
    case Fin of
        fin ->
            % my_utility:addloc(NewI, Pid),
            my_utility:genedge(NewI, {true,skip}, exit, Pid),
            ProcName;
        notfin ->
            pass
    end.
    % Pid ! {self(), fin},
    % receive
    %     {Pid, {Loc, _, Arrow}} ->
    % % {Loc, _, Arrow} = Pid ! {self(), fin},
    %     arrange_edgelist:start(Loc, Arrow)
    % end.

% List([step]) -> Nat -> record -> pid -> Nat -> atom -> tuple
% stepのリストを受け取り各stepをstepを解析する関数へ渡し戻り値によって枝を次のLocにはるか判断する
% IfFlagがifdayoの場合はifから呼ばれたので最後は必ずbreakするようにする
sequence([], _, _, _, I, ifdayo) ->
    {fin, I, break};
sequence([], _, _, _, I, _) ->
    {fin, I, nonbreak};
sequence([Step|Steps], Source, Guard, Pid, I, IfFlag) ->
    % my_utility:addloc(I, Pid),
    {StepAtom, Result} = step(Step, Pid, I),
    % io:format("StepAtom:~p, Result:~p~n", [StepAtom, Result]),
    case StepAtom of
        stmnt1 ->       %if
            {NewI, Flag} = Result,
            case Flag of
                nonbreak ->
                    sequence(Steps, NewI, true, Pid, NewI, IfFlag);
                break ->
                    % {fin, NewI, break}
                    sequence(Steps, NewI, true, Pid, NewI, IfFlag)
            end;
        stmnt2 ->       %do
            {NewI, Flag} = Result,
            %doの中にbreakがない場合は無限ループするのでこれ以降のstepに枝をはらない
            case Flag of
                nonbreak ->
                    Judge = my_utility:checkminus1(Pid),
                    case Judge of
                        true ->
                            my_utility:changeminusedge(Pid, NewI),
                            sequence(Steps, NewI, true, Pid, NewI, IfFlag);
                        false ->
                            {notfin, NewI, nonbreak}
                    end;
                break ->
                    Judge = my_utility:checkminus1(Pid),
                    case Judge of
                        true ->
                            my_utility:changeminusedge(Pid, NewI),
                            sequence(Steps, NewI, true, Pid, NewI, IfFlag);
                        false ->
                            sequence(Steps, NewI, true, Pid, NewI, IfFlag)
                    end;
                ifbreak ->
                    sequence(Steps, NewI, true, Pid, NewI, IfFlag)
            end;

        stmnt9 ->           %else
            Act = Result,
            my_utility:addact({Guard,Act}, Pid),
            NewI = I + 1,
            my_utility:addloc(NewI, Pid),
            my_utility:genedge(Source, {Guard,Act}, NewI, Pid),
            sequence(Steps, NewI, true, Pid, NewI, IfFlag);
        stmnt10 ->                  %break
            Act = Result,
            my_utility:addact({Guard,Act}, Pid),
            NewI = I + 1,
            my_utility:addloc(NewI, Pid),
            my_utility:genedge(Source, {Guard,Act}, NewI, Pid),
            case IfFlag of
                ifdayo ->
                    {fin, NewI, ifbreak};
                _ ->
                    {fin, NewI, break}
            end;
        stmnt81314 ->
            Act = Result,
            my_utility:addact({Guard,Act}, Pid),
            NewI = I + 1,
            my_utility:addloc(NewI, Pid),
            my_utility:genedge(Source, {Guard,Act}, NewI, Pid),
            sequence(Steps, NewI, true, Pid, NewI, IfFlag);
        one_decl ->
            Act = Result,
            my_utility:addact({Guard,Act}, Pid),
            NewI = I + 1,
            my_utility:addloc(NewI, Pid),
            my_utility:genedge(Source, {Guard,Act}, NewI, Pid),
            sequence(Steps, NewI, true, Pid, NewI, IfFlag)
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
    Act = One_decl,
    {one_decl, Act}.

%tuple(tree record) -> tuple({atom, Result})
stmnt(Stmnt, Pid, I) when Stmnt#tree.value == stmnt1->      %stmnt1: if
    Pid ! {self(), checkminlocnum},
    receive
        {Pid, MinimumNum} when MinimumNum >= 0 ->
            Result = options(Stmnt#tree.children, I, [], Pid, I, break, ifdayo, -1);
        {Pid, MinimumNum} ->
            Result = options(Stmnt#tree.children, I, [], Pid, I, break, ifdayo, MinimumNum)
    end,
    {stmnt1, Result};
stmnt(Stmnt, Pid, I) when Stmnt#tree.value == stmnt2 ->     %stmnt2: do
    Pid ! {self(), checkminlocnum},
    receive
        {Pid, MinimumNum} when MinimumNum >= 0 ->
            Result = options(Stmnt#tree.children, I, [], Pid, I, nonbreak, dodayo, -1);     %Result -> {NewI, BreakFlag}
        {Pid, MinimumNum} ->
            Result = options(Stmnt#tree.children, I, [], Pid, I, nonbreak, dodayo, MinimumNum-1)
    end,
    {stmnt2, Result};
% stmnt(Stmnt, Pid, I) when Stmnt#tree.value == stmnt4 ->     %stmnt4: atomic

stmnt(Stmnt, _, _) when Stmnt#tree.value == stmnt9 ->     %stmnt9: else
    Act = Stmnt,
    {stmnt9, Act};
stmnt(Stmnt, _, _) when Stmnt#tree.value == stmnt10 ->    %stmnt10: break
    Act = Stmnt,
    {stmnt10, Act};
stmnt(Stmnt, _, _) when Stmnt#tree.value == stmnt8 orelse Stmnt#tree.value == stmnt13 orelse Stmnt#tree.value == stmnt14 ->       %stmnt8: send receive assign expr, stmnt13: printm stmnt14: printf
    Act = Stmnt,
    {stmnt81314, Act}.

stmnt4guard(Stmnt4guard, Pid, I) when Stmnt4guard#tree.value == stmnt4guard1->
    % Stmnt4guard#tree.children;
    Stmnt4guard;
stmnt4guard(Stmnt4guard, Pid, I) when Stmnt4guard#tree.value == stmnt4guard2->
    % Stmnt4guard#tree.children.
    Stmnt4guard.

% list([optionseq]) -> Nat -> list -> pid -> Nat -> atom -> atom -> tuple({optionの中で進めた分だけインクリメントしたI, Flag})
% optionseqの下の[step]の最後のstepの遷移先をスタックにつんで最後にスタックの中身のエッジをはる(Targetはdoの繰り返し先)
% 第五引数のFlagはbreakを含んでいるかどうか
% IfFlagはこのoption関数が　stmnt関数内のifのケースから呼ばれたかどうか
options([], Source, Stack, Pid, I, Flag, _, MinimumNum) ->
    NewI = my_utility:genedgeStack(Stack, Source, Pid, I, MinimumNum),
    {NewI, Flag};
options([Optionseq|Optionseqs], Source, Stack, Pid, I, Flag, IfFlag, MinimumNum) ->               %breakの場合はスタックに積まずに再帰
    Result = optionseq(Optionseq, Source, Pid, I, IfFlag),
    {_, NewI, BreakFlag} = Result,
    NewStack = my_utility:push(Stack, {NewI, BreakFlag}),
    case Flag of
        nonbreak ->
            options(Optionseqs, Source, NewStack, Pid, NewI, BreakFlag, IfFlag, MinimumNum);
        break ->
            options(Optionseqs, Source, NewStack, Pid, NewI, Flag, IfFlag, MinimumNum);
        ifbreak ->
            options(Optionseqs, Source, NewStack, Pid, NewI, Flag, IfFlag, MinimumNum)
    end.

% record -> Nat -> pid -> Nat　-> atom -> tuple
% [step]の最後の要素のstepのActを返す
optionseq(Optionseq, Source, Pid, I, IfFlag) ->
    [GuardTree|StepList] = Optionseq#tree.children,
    Guard = guard(GuardTree, Pid, I),
    Tmp = hd(StepList),
    Step = hd(Tmp),
    StepChild = Step#tree.children,
    StmntTree = hd(StepChild),
    case StmntTree#tree.value of
        stmnt1 ->
            my_utility:addact({Guard,skip}, Pid),
            NewI = I + 1,
            my_utility:addloc(NewI, Pid),
            my_utility:genedge(Source, {Guard,skip}, NewI, Pid),
            Result = sequence(hd(StepList), Source, Guard, Pid, NewI, IfFlag),
            Result;
        stmnt2 ->
            my_utility:addact({Guard,skip}, Pid),
            NewI = I + 1,
            my_utility:addloc(NewI, Pid),
            my_utility:genedge(Source, {Guard,skip}, NewI, Pid),
            Result = sequence(hd(StepList), Source, Guard, Pid, NewI, IfFlag),
            Result;
        _ ->
            Result = sequence(hd(StepList), Source, Guard, Pid, I, IfFlag),     %Result->{fin, NewI, BreakFlag}
            Result
    end.

guard(GuardTree, Pid, I) ->
    [Stmnt4guard|GuardOpt] = GuardTree#tree.children,
    Guard = stmnt4guard(Stmnt4guard, Pid, I),
    Guard.