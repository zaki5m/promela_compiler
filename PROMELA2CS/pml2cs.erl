-module(pml2cs).
-export([start/1, module/2, sequence/3]).
-include("record.hrl").

start(Tree) ->
    spec(Tree, 0).

spec(Tree, Pid) when is_tuple(Tree#tree.children) ->
    Module = Tree#tree.children,
    module(Module, Pid);
    % io:format("tuple: ~p~n ", [Module]);
spec(Tree, Pid) when is_list(Tree#tree.children) ->
    Modulelist = Tree#tree.children,
    my_utility:listloop(Modulelist, Pid);
    % io:format("list: ~p~n ", [Module]);
spec(Tree, Pid) ->
    io:format("spec is nether a list nor a tule: ~p~n ", [Tree]).

module(Module, Pid) ->
    Child = Module#tree.children,
    ChildValue = Child#tree.value,
    case ChildValue of
        NT when NT =:= proctype orelse NT =:= init ->
            NewPid = spawn(fun setmanager:start/0),
            genPG(Child, NewPid);
        NT when NT =:= utype orelse NT =:= mtype orelse NT =:= decl_lst ->
            io:format("hensuu sengen desu~n");
        _ ->
            io:format("module is nether a process nor declaration: ~p~n", [ChildValue])
    end.

genPG(Proc, Pid) ->
    ProcChild = Proc#tree.children,
    my_utility:listloop4seq(ProcChild, Pid, 0).

sequence(Sequence, Pid, I) when is_tuple(Sequence#tree.children) ->
    Step = Sequence#tree.children,
    step(Step, Pid, I);
sequence(Sequence, Pid, I) when is_list(Sequence#tree.children) ->
    Steps = Sequence#tree.children,
    steps(Steps, Pid, I).

step(Step, Pid, I) ->
    my_utility:addloc(I, Pid),
    StepChild = Step#tree.children,
    StepChildValue = StepChild#tree.value,
    case StepChildValue of
        stmnt ->
            stmnt(Step#tree.children, Step, Pid, I);
        one_decl ->
            Act = {true, StepChild#tree.children},
            my_utility:addact(Act, Pid),
            my_utility:genedge(I, Act, exit, Pid),
            Pid ! {self(), fin}
    end.

steps([Step|Steps], Pid, I) ->
    my_utility:addloc(I, Pid),
    StepChild = Step#tree.children,
    StepChildValue = StepChild#tree.value,
    case StepChildValue of
        stmnt ->
            stepsstmnt(StepChild, Steps, Pid, I);
        one_decl ->
            Act = {true,StepChild#tree.children},
            my_utility:addact(Act, Pid),
            my_utility:genedge(I, Act, I + 1, Pid),
            NewI = I + 1,
            Tmp = hd(Steps),
            steps2(Tmp, Pid, NewI)
    end.

steps2(Steps, Pid, I) ->
    my_utility:addloc(I, Pid),
    StepsChild = Steps#tree.children,
    [_ | Sequence] = StepsChild,
    io:format("sequence:~p~n", [Sequence]),
    case length(Sequence) of
        1 ->
            step(hd(Sequence), Pid, I);
        _ ->
            steps(Sequence, Pid, I)
    end.
    % steps(Sequence, Pid, I).

    % StepChildValue = StepChild#tree.value,
    % case StepChildValue of
    %     stmnt ->
    %         io:format("stepsstmnt:~p~n", [StepChild]),
    %         Pid!{self(), fin};
    %     one_decl ->
    %         Act = StepChild#tree.children,
    %         my_utility:addact(Act, Pid),
    %         case Steps of
    %             [] ->
    %                 my_utility:genedge(I, Act, exit, Pid),
    %                 NewI = I,
    %                 Pid!{self(), fin};
    %             _ ->
    %                 my_utility:genedge(I, Act, I + 1, Pid),
    %                 NewI = I + 2,
    %                 steps2(Steps, Pid, I)
    %         end
    % end.


stmnt(Stmnt, Step, Pid, I) when is_tuple(Stmnt#tree.children) ->
    StmntChild = Stmnt#tree.children,
    StmntChildValue = StmntChild#tree.value,
    case StmntChildValue of
        X when X =:= else ->
            my_utility:addact({true,"else"}, Pid),
            my_utility:genedge(I, "true:else", exit, Pid),
            NewI = I + 1,
            Pid ! {self(), fin};
        X when X =:= assign ->
            Act = {true,StmntChild#tree.children},
            my_utility:addact(Act, Pid),
            my_utility:genedge(I, Act, exit, Pid),
            NewI = I + 1,
            Pid ! {self(), fin}
    end.

% stmnt(Stmnt Step, Pid, I) when is_list(Stmnt#tree.children) ->
%     GetchildValue = fun(Child) -> Child#tree.value end,

%     StmntChildValue = GetchildValue(Stmnt#tree.children),
%     case StmntChildValue of
%         % if or do

stepsstmnt(Stmnt, Steps, Pid, I) when is_tuple(Stmnt#tree.children) ->
    StmntChild = Stmnt#tree.children,
    StmntChildValue = StmntChild#tree.value,
    case StmntChildValue of
        X when X =:= else ->
            Act = {true, "else"},
            my_utility:addact(Act, Pid),
            my_utility:genedge(I, Act, I + 1, Pid),
            NewI = I + 2,
            steps2(Steps, Pid, NewI);
        X when X =:= assign ->
            Act = {true, StmntChild#tree.children},
            my_utility:addact(Act, Pid),
            my_utility:genedge(I, Act, I + 1, Pid),
            NewI = I + 2,
            steps2(Steps, Pid, NewI)
    end.