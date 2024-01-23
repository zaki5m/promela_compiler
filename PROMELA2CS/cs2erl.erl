-module(cs2erl).
-export([start/4]).
-include("record.hrl").

start(PGList, ChanList, MtypeList, GlobalVarList) ->
    generl(PGList, ChanList, MtypeList, GlobalVarList).

generl([], _, _, _) ->
    fin;
generl([PG|PGs], ChanList, MtypeList, GlobalVarList) ->
    {ModuleName, {_, _, Edges}} = PG,
    File = erlwriter:openfile(),
    FMPid = spawn(fun() -> funmanager4cs2erl:start([]) end),
    FPid = spawn(fun() -> erlwriter:filewrite(File) end),
    generlutility4cs2erl:moduleSetup(ModuleName, ChanList, FPid),
    genmodule(Edges, ChanList, MtypeList, GlobalVarList, FPid, FMPid),
    generlutility4cs2erl:genexit(FPid),
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
    generl(PGs, ChanList, MtypeList, GlobalVarList).

genmodule([], _, _, _, _, _) ->
    fin;
genmodule([Edge|Edges], ChanList, MtypeList, GlobalVarList, FPid, FMPid) ->
    {Source, ActLabel, Target} = Edge,
    {Guard, _} = ActLabel,
    {TmpList, NewEdges} = lists:partition(fun(X) -> {TmpS, _, _} = X, TmpS == Source end, Edges),
    SameSourceEdgeList = [Edge | TmpList],
    GuardVarList = generlutility4cs2erl:getguardvar(SameSourceEdgeList, []),
    NewGuardVarList = lists:filter(fun(X) -> (MtypeList == []) or lists:any(fun(Y) -> Y /= X end, MtypeList) end, GuardVarList),
    generlutility4cs2erl:startfun(FMPid, FPid, Source),
    generlutility4cs2erl:declGuardVar(NewGuardVarList, FPid),
    case length(TmpList) of
        0 when Guard == true ->
            VarListChangeFlag = defFun(ActLabel, ChanList, MtypeList, GlobalVarList, FPid, FMPid),
            generlutility4cs2erl:endfun(FMPid, FPid, Target, VarListChangeFlag),
            generlutility4cs2erl:write({self(), {nl, "."}}, FPid);
        _ ->        %guardによる分岐がある時
            generlutility4cs2erl:write({self(), {nl, "   if"}}, FPid),
            defFunwithGuard(SameSourceEdgeList, ChanList, MtypeList, GlobalVarList, FPid, FMPid, [])
    end,
    genmodule(NewEdges, ChanList, MtypeList, GlobalVarList, FPid, FMPid).

defFun(ActLabel, ChanList, MtypeList, GlobalVarList, FPid, FMPid) ->
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
                    VarListChangeFlag = analyzeStmnt8(ActChild, ChanList, MtypeList, GlobalVarList, FPid, FMPid),
                    VarListChangeFlag;
                stmnt13 ->
                    VarListChangeFlag = analyzeStmnt13(ChanList, MtypeList, FPid, FMPid),
                    VarListChangeFlag;
                stmnt14 ->
                    VarListChangeFlag = analyzeStmnt14(ChanList, MtypeList, FPid, FMPid),
                    VarListChangeFlag;
                one_decl ->
                    One_declChild = Act#tree.children,
                    IvarTree = lists:nth(3, One_declChild),
                    io:format("IvarTree~p~n", [IvarTree]),
                    VarList = generlutility4cs2erl:ivars(IvarTree, []),
                    case VarList of
                        [] ->
                            nochange;
                        _ ->
                            generlutility4cs2erl:write({self(), {append, "   NewVarList = VarList "}}, FPid),
                            generlutility4cs2erl:writeAddVarList(VarList, FPid),
                            changed
                    end
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
        %     generlutility4cs2erl:write({self(), {nl, "   end."}})
    end.

defFunwithGuard([], ChanList, MtypeList, GlobalVarList, FPid, FMPid, TrueEdge) ->
    case TrueEdge of
        [] ->
            generlutility4cs2erl:write({self(), {nl, "   end."}}, FPid);
        _ ->
            {_, {Guard, Act}, Target} = hd(TrueEdge),
            writeguard(Guard, ChanList, MtypeList, FPid, FMPid),
            case Act of
                skip ->
                    generlutility4cs2erl:endfun(FMPid, FPid, Target, nochange);
                _ ->
                    case Act#tree.value of
                        stmnt10 ->              %break
                            VarListChangeFlag = analyzeStmnt10(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt8 ->
                            ActChild = Act#tree.children,
                            VarListChangeFlag = analyzeStmnt8(ActChild, ChanList, MtypeList, GlobalVarList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt13 ->
                            VarListChangeFlag = analyzeStmnt13(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt14 ->
                            VarListChangeFlag = analyzeStmnt14(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        one_decl ->
                            One_declChild = Act#tree.children,
                            IvarTree = lists:nth(3, One_declChild),
                            VarList = generlutility4cs2erl:ivars(IvarTree, []),
                            case VarList of
                                [] ->
                                    VarListChangeFlag = nochange;
                                _ ->
                                    generlutility4cs2erl:write({self(), {append, "   NewVarList = VarList "}}, FPid),
                                    generlutility4cs2erl:writeAddVarList(VarList, FPid),
                                    VarListChangeFlag = changed
                            end
                    end,
                    generlutility4cs2erl:endfun(FMPid, FPid, Target, VarListChangeFlag),
                    generlutility4cs2erl:write({self(), {nl, ""}}, FPid)
            end,
            generlutility4cs2erl:write({self(), {nl, "   end."}}, FPid)
    end;
defFunwithGuard([Edge|Edges],ChanList, MtypeList, GlobalVarList, FPid, FMPid, TrueEdge) ->
    {_, {Guard, Act}, Target} = Edge,
    case Guard of
        true ->
            NewTrueEdge =  [Edge|TrueEdge],
            defFunwithGuard(Edges, ChanList, MtypeList, GlobalVarList, FPid, FMPid, NewTrueEdge);
        _ when Guard#tree.value == stmnt4guard1 ->
            NewTrueEdge =  [Edge|TrueEdge],
            defFunwithGuard(Edges, ChanList, MtypeList, GlobalVarList, FPid, FMPid, NewTrueEdge);
        _ ->
            writeguard(Guard, ChanList, MtypeList, FPid, FMPid),
            case Act of
                skip ->
                    generlutility4cs2erl:endfun(FMPid, FPid, Target, nochange);
                _ ->
                    case Act#tree.value of
                        stmnt10 ->              %break
                            VarListChangeFlag = analyzeStmnt10(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt8 ->
                            ActChild = Act#tree.children,
                            VarListChangeFlag = analyzeStmnt8(ActChild, ChanList, MtypeList, GlobalVarList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt13 ->
                            VarListChangeFlag = analyzeStmnt13(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        stmnt14 ->
                            VarListChangeFlag = analyzeStmnt14(ChanList, MtypeList, FPid, FMPid),
                            VarListChangeFlag;
                        one_decl ->
                            VarListChangeFlag = nochange
                    end,
                    generlutility4cs2erl:endfun(FMPid, FPid, Target, VarListChangeFlag)
            end,
            case Edges of
                [] when TrueEdge == [] ->
                    generlutility4cs2erl:write({self(), {nl, ""}}, FPid);
                _ ->
                    generlutility4cs2erl:write({self(), {nl, ";"}}, FPid)
            end,
            defFunwithGuard(Edges, ChanList, MtypeList, GlobalVarList, FPid, FMPid, TrueEdge)
    end.


writeguard(Guard, ChanList, MtypeList, FPid, FMPid) ->
    GuardChild = Guard#tree.children,
    case Guard#tree.value of
        true ->
            generlutility4cs2erl:write({self(), {nl, "   true ->"}}, FPid),
            generlutility4cs2erl:write({self(), {append, "       "}}, FPid);
        stmnt4guard1 ->                             %else
            generlutility4cs2erl:write({self(), {nl, "   true ->"}}, FPid),
            generlutility4cs2erl:write({self(), {append, "       "}}, FPid);
        stmnt4guard2 when GuardChild#tree.value == expr1 ->
            generlutility4cs2erl:expr(GuardChild, MtypeList, FPid, guard);
            % guardvaluelistWrite(NewValueList, FPid);
        stmnt4guard2 when GuardChild#tree.value == expr2 ->
            generlutility4cs2erl:write({self(), {nl, "   ("}}, FPid),
            generlutility4cs2erl:expr(GuardChild, MtypeList, FPid, guard),
            generlutility4cs2erl:write({self(), {nl, "   )"}}, FPid)
    end.


analyzeStmnt8(ActChild, ChanList, MtypeList, GlobalVarList, FPid, FMPid) when ActChild#tree.value == send1 ->
    Send1Child = ActChild#tree.children,
    [VarrefTree|Tale] = Send1Child,
    Channame = atom_to_list(generlutility4cs2erl:varref(VarrefTree)),
    Send_argsTree = hd(Tale),
    {ValueList, VarList} = generlutility4cs2erl:send_args(Send_argsTree),
    generlutility4cs2erl:declVar(VarList, FPid),
    generlutility4cs2erl:write({self(), {append, "  {_, "}}, FPid),
    generlutility4cs2erl:write({self(), {append, Channame}}, FPid),
    generlutility4cs2erl:write({self(), {append, "Pid} = hd(lists:filter(fun(X) -> {TmpChanname, _} = X, TmpChanname == \""}}, FPid),
    generlutility4cs2erl:write({self(), {append, Channame}}, FPid),
    generlutility4cs2erl:write({self(), {nl, "\" end, ChanPidList)),"}}, FPid),
    generlutility4cs2erl:write({self(), {append, "  erlutility:sender("}}, FPid),
    generlutility4cs2erl:valuelistWrite(ValueList, FPid),
    generlutility4cs2erl:write({self(), {append, ","}}, FPid),
    generlutility4cs2erl:write({self(), {append, Channame}}, FPid),
    generlutility4cs2erl:write({self(), {nl, "Pid),"}}, FPid),
    nochange;

analyzeStmnt8(ActChild, ChanList, MtypeList, GlobalVarList, FPid, FMPid) when ActChild#tree.value == receive1 ->
    Receive1Child = ActChild#tree.children,
    [VarrefTree|Tale] = Receive1Child,
    Channame = atom_to_list(generlutility4cs2erl:varref(VarrefTree)),
    RecvArgsTree = hd(Tale),
    Tmp = RecvArgsTree#tree.children,
    Tmp2 = hd(Tmp),
    RecvArg = hd(Tmp2),
    {Atom, Value} = generlutility4cs2erl:recv_args(RecvArg),
    case Atom of
        var ->
            % generlutility4cs2erl:declVar([Value], FPid),
            generlutility4cs2erl:write({self(), {append, "  {_, "}}, FPid),
            generlutility4cs2erl:write({self(), {append, Channame}}, FPid),
            generlutility4cs2erl:write({self(), {append, "Pid} = hd(lists:filter(fun(X) -> {TmpChanname, _} = X, TmpChanname == \""}}, FPid),
            generlutility4cs2erl:write({self(), {append, Channame}}, FPid),
            generlutility4cs2erl:write({self(), {nl, "\" end, ChanPidList)),"}}, FPid),
            generlutility4cs2erl:write({self(), {append, "{true, {Varname, Value}} = erlutility:receivere({var, "}}, FPid),
            generlutility4cs2erl:write({self(), {append, Value}}, FPid),
            generlutility4cs2erl:write({self(), {append, "}, "}}, FPid),
            generlutility4cs2erl:write({self(), {append, Channame}}, FPid),
            generlutility4cs2erl:write({self(), {nl, "Pid),"}}, FPid),
            generlutility4cs2erl:writeListOperationForList("Varname", FPid),
            generlutility4cs2erl:write({self(), {nl, "Value}|TmpVarList],"}}, FPid),
            changed;
        const ->
            generlutility4cs2erl:write({self(), {append, "  {_, "}}, FPid),
            generlutility4cs2erl:write({self(), {append, Channame}}, FPid),
            generlutility4cs2erl:write({self(), {append, "Pid} = hd(lists:filter(fun(X) -> {TmpChanname, _} = X, TmpChanname == \""}}, FPid),
            generlutility4cs2erl:write({self(), {append, Channame}}, FPid),
            generlutility4cs2erl:write({self(), {nl, "\" end, ChanPidList)),"}}, FPid),
            generlutility4cs2erl:write({self(), {append, "{true, Value} = erlutility:receiver({const,"}}, FPid),
            generlutility4cs2erl:write({self(), {append, Value}}, FPid),
            generlutility4cs2erl:write({self(), {append, "}, "}}, FPid),
            generlutility4cs2erl:write({self(), {append, Channame}}, FPid),
            generlutility4cs2erl:write({self(), {nl, "Pid),"}}, FPid),
            nochange
    end;

analyzeStmnt8(ActChild, _, MtypeList, GlobalVarList, FPid, FMPid) when ActChild#tree.value == assign1 ->
    Assign1Child = ActChild#tree.children,        %[varref, any_expr]
    [VarrefTree|Tale] = Assign1Child,
    Varname = generlutility4cs2erl:varref(VarrefTree),
    Any_exprTree = hd(Tale),
    {ValueList, TmpVarList} = generlutility4cs2erl:any_expr(Any_exprTree, []),
    VarList = lists:usort(TmpVarList),
    NewValueList = lists:map(fun(X) -> case is_atom(X) of true -> atom_to_list(X); false -> X end end, ValueList),
    % startfun(FMPid, FPid, Source),
    generlutility4cs2erl:declVar(VarList, FPid),
    generlutility4cs2erl:writeListOperationForList(Varname, FPid),
    generlutility4cs2erl:valuelistWrite(NewValueList, FPid),
    generlutility4cs2erl:write({self(), {nl, "}|TmpVarList],"}}, FPid),
    changed;

analyzeStmnt8(ActChild, ChanList, MtypeList, GlobalVarList, FPid, FMPid) when ActChild#tree.value == expr1 ->
    % Expr1Child = ActChild#tree.children,
    % {ValueList, TmpVarList} = generlutility4cs2erl:any_expr(Expr1Child, []),
    % VarList = lists:usort(TmpVarList),
    % NewValueList = lists:map(fun(X) -> case is_atom(X) of true -> atom_to_list(X); false -> X end end, ValueList),
    % % startfun(FMPid, FPid, Source),
    % declVar(VarList, FPid),
    % generlutility4cs2erl:write({self(), {append, "   "}}, FPid),
    % generlutility4cs2erl:valuelistWrite(NewValueList, FPid),
    % generlutility4cs2erl:write({self(), {nl, ","}}, FPid),
    generlutility4cs2erl:expr(ActChild, MtypeList, FPid, act),
    nochange;

analyzeStmnt8(ActChild, ChanList, MtypeList, GlobalVarList, FPid, FMPid) when ActChild#tree.value == expr2 ->
    % Expr2Child = ActChild#tree.children,
    % generlutility4cs2erl:write({self(), {nl, "   ("}}, FPid),
    % VarListChangeFlag = analyzeStmnt8(Expr2Child, ChanList, MtypeList, FPid, FMPid),
    % generlutility4cs2erl:write({self(), {nl, "   )"}}, FPid),
    generlutility4cs2erl:expr(ActChild, MtypeList, FPid, act),
    nochange;

% analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == expr3 ->
%     Expr3Child = ActChild#tree.children,


analyzeStmnt8(ActChild, _, MtypeList, GlobalVarList, FPid, FMPid) ->
    Assign2Child = ActChild#tree.children,
    VarName = generlutility4cs2erl:varref(hd(Assign2Child)),
    % startfun(FMPid, FPid, Source),
    generlutility4cs2erl:declVar([VarName], FPid),
    generlutility4cs2erl:writeListOperationForList(VarName, FPid),
    TmpVar = atom_to_list(VarName),
    generlutility4cs2erl:write({self(), {append, TmpVar}}, FPid),
    case ActChild#tree.value of
        assign2 ->
            generlutility4cs2erl:write({self(), {nl, " + 1}|TmpVarList],"}}, FPid);
        assign3 ->
            generlutility4cs2erl:write({self(), {nl, " - 1}|TmpVarList],"}}, FPid)
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