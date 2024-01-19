-module(csprime2erl).
-export([start/5]).
-include("record.hrl").

% checkSR(Tree) ->
%     TreeChild = Tree#tree.children,
%     case Tree#tree.value of
%         stmnt4guard2 when TreeChild#tree.value == send1 ->
%             SendChild = TreeChild#tree.children
%             {sr, }
%         stmnt4guard2 when TreeChild#tree.value == receive1
%         stmnt8 when TreeChild#tree.value == send1 ->
%         stmnt8 when TreeChild#tree.value == receive1 ->
%         _ ->
%             notsr
%     end.

start(CSprime, IndividualPGEdgeList, ChanList, MtypeList, GlobalVarList) ->
    {State, Act, Edge} = CSprime,
    generl(Edge,IndividualPGEdgeList, ChanList, MtypeList).


generl(_, [], _, _) ->
    fin;
generl(CSprimeEdges, [PGEdge|PGEdges], ChanList, MtypeList) ->
    {ModuleName, Edges} = PGEdge,
    File = erlwriter:openfile(),
    FMPid = spawn(fun() -> funmanager:start(0) end),
    FPid = spawn(fun() -> erlwriter:filewrite(File) end),
    generlutility:moduleSetup(ModuleName, FPid),
    genmodule(Edges, ChanList, MtypeList, CSprimeEdges, FPid, FMPid),
    generlutility:genexit(FPid),
    erlwriter:closefile(File),
    FMPid ! {self(), fin},
    receive
        {FMPid, ok} ->
            io:format("FMfin")
    end,
    FPid ! {self(), fin},
    receive
        {FPid, ok} ->
            io:format("Ffin")
    end,
    generl(CSprimeEdges, PGEdges, ChanList, MtypeList).

genmodule([], _, _, _, _, _) ->
    fin;
genmodule([Edge|Edges], ChanList, MtypeList, CSprimeEdges, FPid, FMPid) ->
    {Source, ActLabel, Target} = Edge,
    {Guard, _} = ActLabel,
    {TmpList, NewEdges} = lists:partition(fun(X) -> {TmpS, _, _} = X, TmpS == Source end, Edges),
    SameSourceEdgeList = [Edge | TmpList],
    GuardVarList = generlutility:getguardvar(SameSourceEdgeList, []),
    generlutility:startfun(FMPid, FPid, Source),
    generlutility:declGuardVar(GuardVarList, FPid),
    case length(TmpList) of
        0 when Guard == true ->
            VarListChangeFlag = defFun(ActLabel, ChanList, MtypeList, FPid, FMPid),
            generlutility:endfun(FMPid, FPid, Target, VarListChangeFlag),
            generlutility:write({self(), {nl, "."}}, FPid);
        _ ->        %guardによる分岐がある時
            generlutility:write({self(), {nl, "   if"}}, FPid),
            defFunwithGuard(SameSourceEdgeList, ChanList, MtypeList, FPid, FMPid, [])
    end,
    genmodule(NewEdges, ChanList, MtypeList, CSprimeEdges, FPid, FMPid).

defFun(ActLabel, ChanList, MtypeList, FPid, FMPid) ->
    {_, Act} = ActLabel,
    case Act of
        skip ->
            nochange;
        _ ->
            case Act#tree.value of
                stmnt10 ->              %break
                    VarListChangeFlag = analyzeStmnt10(ChanList, MtypeList, FPid, FMPid),
                    VarListChangeFlag;
                stmnt8 ->
                    ActChild = Act#tree.children,
                    VarListChangeFlag = analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid),
                    VarListChangeFlag;
                stmnt13 ->
                    VarListChangeFlag = analyzeStmnt13(ChanList, MtypeList, FPid, FMPid),
                    VarListChangeFlag;
                stmnt14 ->
                    VarListChangeFlag = analyzeStmnt14(ChanList, MtypeList, FPid, FMPid),
                    VarListChangeFlag
                % one_decl ->
            end
        % _ ->
        %     writeguard(Guard, ChanList, MtypeList, FPid, FMPid),
        %     case Act#tree.value of
        %         stmnt10 ->              %break
        %             VarListChangeFlag = analyzeStmnt10(ChanList, MtypeList, FPid, FMPid),
        %             VarListChangeFlag;
        %         stmnt8 ->
        %             ActChild = Act#tree.children,
        %             VarListChangeFlag = analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid),
        %             VarListChangeFlag;
        %         stmnt13 ->
        %             VarListChangeFlag = analyzeStmnt13(ChanList, MtypeList, FPid, FMPid),
        %             VarListChangeFlag;
        %         stmnt14 ->
        %             VarListChangeFlag = analyzeStmnt14(ChanList, MtypeList, FPid, FMPid),
        %             VarListChangeFlag
        %         % one_decl ->
        %     end,
        %     generlutility:write({self(), {nl, "   end."}})
    end.

defFunwithGuard([], ChanList, MtypeList, FPid, FMPid, TrueEdge) ->
    case TrueEdge of
        [] ->
            generlutility:write({self(), {nl, "   end."}}, FPid);
        _ ->
            {_, {Guard, Act}, Target} = hd(TrueEdge),
            writeguard(Guard, ChanList, MtypeList, FPid, FMPid),
            case Act of
                skip ->
                    generlutility:endfun(FMPid, FPid, Target, nochage);
                _ ->
                    case Act#tree.value of
                        stmnt10 ->              %break
                            VarListChangeFlag = analyzeStmnt10(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt8 ->
                            ActChild = Act#tree.children,
                            VarListChangeFlag = analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt13 ->
                            VarListChangeFlag = analyzeStmnt13(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt14 ->
                            VarListChangeFlag = analyzeStmnt14(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag
                        % one_decl ->
                    end,
                    generlutility:endfun(FMPid, FPid, Target, VarListChangeFlag),
                    generlutility:write({self(), {nl, ""}}, FPid)
            end,
            generlutility:write({self(), {nl, "   end."}}, FPid)
    end;
defFunwithGuard([Edge|Edges],ChanList, MtypeList, FPid, FMPid, TrueEdge) ->
    {_, {Guard, Act}, Target} = Edge,
    case Guard of
        true ->
            NewTrueEdge =  [Edge|TrueEdge],
            defFunwithGuard(Edges, ChanList, MtypeList, FPid, FMPid, NewTrueEdge);
        _ when Guard#tree.value == stmnt4guard1 ->
            NewTrueEdge =  [Edge|TrueEdge],
            defFunwithGuard(Edges, ChanList, MtypeList, FPid, FMPid, NewTrueEdge);
        _ ->
            writeguard(Guard, ChanList, MtypeList, FPid, FMPid),
            case Act of
                skip ->
                    generlutility:endfun(FMPid, FPid, Target, nochage);
                _ ->
                    case Act#tree.value of
                        stmnt10 ->              %break
                            VarListChangeFlag = analyzeStmnt10(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt8 ->
                            ActChild = Act#tree.children,
                            VarListChangeFlag = analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt13 ->
                            VarListChangeFlag = analyzeStmnt13(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt14 ->
                            VarListChangeFlag = analyzeStmnt14(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag
                        % one_decl ->
                    end,
                    generlutility:endfun(FMPid, FPid, Target, VarListChangeFlag)
            end,
            case Edges of
                [] when TrueEdge == [] ->
                    generlutility:write({self(), {nl, ""}}, FPid);
                _ ->
                    generlutility:write({self(), {nl, ";"}}, FPid)
            end,
            defFunwithGuard(Edges, ChanList, MtypeList, FPid, FMPid, TrueEdge)
    end.


writeguard(Guard, ChanList, MtypeList, FPid, FMPid) ->
    GuardChild = Guard#tree.children,
    case Guard#tree.value of
        true ->
            generlutility:write({self(), {nl, "   true ->"}}, FPid),
            generlutility:write({self(), {append, "       "}}, FPid);
        stmnt4guard1 ->                             %else
            generlutility:write({self(), {nl, "   true ->"}}, FPid),
            generlutility:write({self(), {append, "       "}}, FPid);
        stmnt4guard2 when GuardChild#tree.value == expr1 ->
            generlutility:expr(GuardChild, FPid, guard);
            % guardvaluelistWrite(NewValueList, FPid);
        stmnt4guard2 when GuardChild#tree.value == expr2 ->
            generlutility:write({self(), {nl, "   ("}}, FPid),
            generlutility:expr(GuardChild, FPid, guard),
            generlutility:write({self(), {nl, "   )"}}, FPid)
    end.



analyzeStmnt8(ActChild, _, MtypeList, FPid, FMPid) when ActChild#tree.value == assign1 ->
    Assign1Child = ActChild#tree.children,        %[varref, any_expr]
    [VarrefTree|Tale] = Assign1Child,
    Varname = generlutility:varref(VarrefTree),
    Any_exprTree = hd(Tale),
    {ValueList, TmpVarList} = generlutility:any_expr(Any_exprTree, []),
    VarList = lists:usort(TmpVarList),
    NewValueList = lists:map(fun(X) -> case is_atom(X) of true -> atom_to_list(X); false -> X end end, ValueList),
    % startfun(FMPid, FPid, Source),
    generlutility:declVar(VarList, FPid),
    generlutility:writeListOperationForList(Varname, FPid),
    generlutility:valuelistWrite(NewValueList, FPid),
    generlutility:write({self(), {nl, "}|TmpVarList],"}}, FPid),
    changed;

analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid) when ActChild#tree.value == expr1 ->
    % Expr1Child = ActChild#tree.children,
    % {ValueList, TmpVarList} = generlutility:any_expr(Expr1Child, []),
    % VarList = lists:usort(TmpVarList),
    % NewValueList = lists:map(fun(X) -> case is_atom(X) of true -> atom_to_list(X); false -> X end end, ValueList),
    % % startfun(FMPid, FPid, Source),
    % declVar(VarList, FPid),
    % generlutility:write({self(), {append, "   "}}, FPid),
    % generlutility:valuelistWrite(NewValueList, FPid),
    % generlutility:write({self(), {nl, ","}}, FPid),
    generlutility:expr(ActChild, FPid, act),
    nochange;

analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid) when ActChild#tree.value == expr2 ->
    % Expr2Child = ActChild#tree.children,
    % generlutility:write({self(), {nl, "   ("}}, FPid),
    % VarListChangeFlag = analyzeStmnt8(Expr2Child, ChanList, MtypeList, FPid, FMPid),
    % generlutility:write({self(), {nl, "   )"}}, FPid),
    generlutility:expr(ActChild, FPid, act),
    nochange;

% analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == expr3 ->
%     Expr3Child = ActChild#tree.children,


analyzeStmnt8(ActChild, _, MtypeList, FPid, FMPid) ->
    Assign2Child = ActChild#tree.children,
    VarName = generlutility:varref(hd(Assign2Child)),
    % startfun(FMPid, FPid, Source),
    generlutility:declVar([VarName], FPid),
    generlutility:writeListOperationForList(VarName, FPid),
    TmpVar = atom_to_list(VarName),
    generlutility:write({self(), {append, TmpVar}}, FPid),
    case ActChild#tree.value of
        assign2 ->
            generlutility:write({self(), {nl, " + 1}|TmpVarList],"}}, FPid);
        assign3 ->
            generlutility:write({self(), {nl, " - 1}|TmpVarList],"}}, FPid)
    end,
    changed.
% analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == send1 ->
% analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == receive1 ->

analyzeStmnt10(ChanList, MtypeList, FPid, FMPid) ->
    nochange.

analyzeStmnt13(ChanList, MtypeList, FPid, FMPid) ->
    nochange.

analyzeStmnt14(ChanList, MtypeList, FPid, FMPid) ->
    nochange.