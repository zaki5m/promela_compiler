-module(csprime2erl).
-export([start/4]).
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

write(Msg, FPid) ->
    FPid ! Msg,
    receive
        {FPid, fin} ->
            fin
    end.

valuelistWrite([], _) ->
    fin;
valuelistWrite([Value|Values], FPid) ->
    write({self(), {append, Value}}, FPid),
    write({self(), {append, " "}}, FPid),
    valuelistWrite(Values, FPid).

startfun(FMPid, FPid, Source) ->
    FMPid ! {self(), {deffun, Source}},
    receive
        {FMPid, Funname} ->
            % io:format("~p~n", [Funname]),
            write({self(), {append, f}}, FPid),
            write({self(), {append, Funname}}, FPid),
            write({self(), {nl, "(VarList) ->"}}, FPid)
    end.

endfun(FMPid, FPid, Target, VarListChangeFlag) ->
    {Loc, _} = Target,
    case Loc of
        exit ->
            write({self(), {append, "   "}}, FPid),
            write({self(), {append, exit}}, FPid),
            write({self(), {nl, "()."}}, FPid);
        _ ->
            FMPid ! {self(), {deffun, Target}},
            receive
                {FMPid, Funname} ->
                    write({self(), {append, "   "}}, FPid),
                    write({self(), {append, f}}, FPid),
                    write({self(), {append, Funname}}, FPid),
                    case VarListChangeFlag of
                        nochange ->
                            write({self(), {nl, "(VarList)."}}, FPid);
                        changed ->
                            write({self(), {nl, "(NewVarList)."}}, FPid)
                    end
            end
    end.

genexit(FPid) ->
    write({self(), {append, exit}}, FPid),
    % write({self(), {nl, "(GPid) ->"}}, FPid),
    write({self(), {nl, "() ->"}}, FPid),
    write({self(), {append, "   fin."}}, FPid).

moduleSetup(ModuleName, FPid) ->
    write({self(), {append, "-module("}}, FPid),
    write({self(), {append, ModuleName}}, FPid),
    write({self(), {nl, ")."}}, FPid),
    write({self(), {nl, "-export([start/0])."}}, FPid),
    write({self(), {nl, "start() ->"}}, FPid),
    % write({self(), {nl, "   GPid = spawn(fun() -> globalvarmanager:loop() end),"}}, FPid),
    % write({self(), {nl, "   f0(GPid)."}}, FPid).
    write({self(), {nl, "   f0([])."}}, FPid).

start(CSprime, IndividualPGEdgeList, ChanList, MtypeList) ->
    {State, Act, Edge} = CSprime,
    generl(Edge,IndividualPGEdgeList, ChanList, MtypeList).


generl(_, [], _, _) ->
    fin;
generl(CSprimeEdges, [PGEdge|PGEdges], ChanList, MtypeList) ->
    {ModuleName, Edges} = PGEdge,
    File = erlwriter:openfile(),
    FMPid = spawn(fun() -> funmanager:start(0) end),
    FPid = spawn(fun() -> erlwriter:filewrite(File) end),
    moduleSetup(ModuleName, FPid),
    genmodule(Edges, ChanList, MtypeList, CSprimeEdges, FPid, FMPid),
    genexit(FPid),
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
    TmpList = lists:filter(fun(X) -> {TmpS, _, _} = X, TmpS == Source end, Edges),
    case length(TmpList) of
        0 ->
            defFun(Edge, ChanList, MtypeList, FPid, FMPid)
        % _ ->        %guardによる分岐がある時
        %     SameSourceEdgeList = [Edge | TmpList],
        %     defFunwithGuard(SameSourceEdgeList)
    end,
    genmodule(Edges, ChanList, MtypeList, CSprimeEdges, FPid, FMPid).

defFun(Edge, ChanList, MtypeList, FPid, FMPid) ->
    {Source, ActLabel, Target} = Edge,
    % {SLoc, SChanList} = Source,
    % % {TLoc, TChanList} = Target,
    {Guard, Act} = ActLabel,
    case Guard of
        true when Act == skip ->
            startfun(FMPid, FPid, Source),
            endfun(FMPid, FPid, Target, nochange);
        true ->
            startfun(FMPid, FPid, Source),
            case Act#tree.value of
                stmnt10 ->              %break
                    analyzeStmnt10(ChanList, MtypeList, FPid, FMPid, Target);
                stmnt8 ->
                    ActChild = Act#tree.children,
                    analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Target);
                stmnt13 ->
                    analyzeStmnt13(ChanList, MtypeList, FPid, FMPid, Target);
                stmnt14 ->
                    analyzeStmnt14(ChanList, MtypeList, FPid, FMPid, Target)
                % one_decl ->
            end;
        _ ->
            startfun(FMPid, FPid, Source)
            % writeguard(Guard, FPid)
    end.

% writeguard(GuardTree, FPid) ->
%     write({self(), {nl, "   if"}}, FPid),
%     case Guard#tree.value of
%     stmnt4guard1 ->                             %else
%         write({self(), {nl, "      true ->"}}, FPid);
%     stmnt4guard2 ->


analyzeStmnt8(ActChild, _, MtypeList, FPid, FMPid, Target) when ActChild#tree.value == assign1 ->
    Assign1Child = ActChild#tree.children,        %[varref, any_expr]
    [VarrefTree|Tale] = Assign1Child,
    Varname = varref(VarrefTree),
    Any_exprTree = hd(Tale),
    {ValueList, TmpVarList} = any_expr(Any_exprTree, []),
    VarList = lists:usort(TmpVarList),
    NewValueList = lists:map(fun(X) -> case is_atom(X) of true -> atom_to_list(X); false -> X end end, ValueList),
    % startfun(FMPid, FPid, Source),
    declVar(VarList, FPid),
    write({self(), {append, "   TmpVarList = lists:filter(fun(X) -> {TmpVarname, _} = X, TmpVarname /= "}}, FPid),
    write({self(), {append, Varname}}, FPid),
    write({self(), {nl, " end, VarList),"}}, FPid),
    write({self(), {append, "   NewVarList = [{"}}, FPid),
    write({self(), {append, Varname}}, FPid),
    write({self(), {append, ","}}, FPid),
    valuelistWrite(NewValueList, FPid),
    write({self(), {nl, "}|TmpVarList],"}}, FPid),
    endfun(FMPid, FPid, Target, changed);

analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Target) when ActChild#tree.value == expr1 ->
    Expr1Child = ActChild#tree.children,
    {ValueList, TmpVarList} = any_expr(Expr1Child, []),
    VarList = lists:usort(TmpVarList),
    NewValueList = lists:map(fun(X) -> case is_atom(X) of true -> atom_to_list(X); false -> X end end, ValueList),
    % startfun(FMPid, FPid, Source),
    declVar(VarList, FPid),
    write({self(), {append, "   "}}, FPid),
    valuelistWrite(NewValueList, FPid),
    write({self(), {nl, ","}}, FPid),
    endfun(FMPid, FPid, Target, nochange);

analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Target) when ActChild#tree.value == expr2 ->
    Expr2Child = ActChild#tree.children,
    write({self(), {nl, "   ("}}, FPid),
    analyzeStmnt8(Expr2Child, ChanList, MtypeList, FPid, FMPid, Target),
    write({self(), {nl, "   )"}}, FPid);

% analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == expr3 ->
%     Expr3Child = ActChild#tree.children,


analyzeStmnt8(ActChild, _, MtypeList, FPid, FMPid, Target) ->
    Assign2Child = ActChild#tree.children,
    VarName = varref(hd(Assign2Child)),
    % startfun(FMPid, FPid, Source),
    declVar([VarName], FPid),
    write({self(), {append, "   TmpVarList = lists:filter(fun(X) -> {TmpVarname, _} = X, TmpVarname /= "}}, FPid),
    write({self(), {append, VarName}}, FPid),
    write({self(), {nl, " end, VarList),"}}, FPid),
    TmpVar = atom_to_list(VarName),
    write({self(), {append, "   NewVarList = [{"}}, FPid),
    write({self(), {append, VarName}}, FPid),
    write({self(), {append, ","}}, FPid),
    write({self(), {append, TmpVar}}, FPid),
    case ActChild#tree.value of
        assign2 ->
            write({self(), {nl, " + 1}|TmpVarList],"}}, FPid);
        assign3 ->
            write({self(), {nl, " - 1}|TmpVarList],"}}, FPid)
    end,
    endfun(FMPid, FPid, Target, changed).
% analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == send1 ->
% analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == receive1 ->

analyzeStmnt10(ChanList, MtypeList, FPid, FMPid, Target) ->
    endfun(FMPid, FPid, Target, nochange).

analyzeStmnt13(ChanList, MtypeList, FPid, FMPid, Target) ->
    endfun(FMPid, FPid, Target, nochange).

analyzeStmnt14(ChanList, MtypeList, FPid, FMPid, Target) ->
    endfun(FMPid, FPid, Target, nochange).

varref(VarrefTreeList) ->
    Tmp = hd(VarrefTreeList),
    VarrefChild = Tmp#tree.children,
    Tmp2 = hd(VarrefChild),
    NameTree = hd(Tmp2),
    Name = NameTree#tree.children,
    Name.

any_expr(Any_exprTree, VarList) ->
    ValueList = [],
    Tmp = hd(Any_exprTree),
    case Tmp#tree.value of
        any_expr1 ->
            Any_exprChild = Tmp#tree.children,
            io:format("Any1:~p~n", [Any_exprChild]),
            {TmpList, NewVarList} = any_expr(Any_exprChild, VarList),
            NewValueList = ['('] ++ TmpList ++ [')'],
            {NewValueList, NewVarList};
        any_expr2 ->
            Any_exprChild = Tmp#tree.children,
            {NewValueList, NewVarList} = calc(Any_exprChild, VarList),
            {NewValueList, NewVarList};
        any_expr6 ->
            Any_exprChild = Tmp#tree.children,
            case is_list(Any_exprChild) of
                true ->
                    Name = varref(Any_exprChild),
                    NewValueList = ValueList ++ [Name],
                    NewVarList = [Name|VarList],
                    {NewValueList, NewVarList};
                false ->
                    Const = Any_exprChild#tree.children,
                    NewValueList = ValueList ++ [Const],
                    {NewValueList, VarList}
            end;
        _ ->
            skip
    end.

calc(Any_exprChild, VarList) ->
    {LH, OpTree, RH} = list_to_tuple(Any_exprChild),
    % LAny_exprTree = hd(LH),
    % RAny_exprTree = hd(RH),
    {LHVlueList, LHVarList} = any_expr(LH, VarList),
    TmpOp = OpTree#tree.children,
    Op = convbinarop(TmpOp),
    {RHValueList, RHVarList} = any_expr(RH, LHVarList),
    ValueList =  LHVlueList ++ [Op] ++ RHValueList,
    {ValueList, RHVarList}.

declVar([], _) ->
    fin;
declVar([Var|Vars], FPid) ->
    write({self(), {append, "   {_, "}}, FPid),
    TmpVar = atom_to_list(Var),
    write({self(), {append, TmpVar}}, FPid),
    write({self(), {append, "} = lists:filter(fun(X) -> {Tmpname, _} = X, Tmpname == "}}, FPid),
    write({self(), {append, Var}}, FPid),
    write({self(), {nl, " end, VarList),"}}, FPid),
    declVar(Vars, FPid).

convbinarop(BinOpAtom) when is_tuple(BinOpAtom) ->
    NewBinOpAtom = BinOpAtom#tree.children,
    case NewBinOpAtom of
        '&&' ->
            'and';
        '||' ->
            'or'
    end;
convbinarop(BinOpAtom) ->
    case BinOpAtom of
        '/' ->
            'div';
        '!=' ->
            '/=';
        '%' ->
            'rem';
        '&' ->
            'band';
        '|' ->
            'bor';
        _ ->
            BinOpAtom
    end.