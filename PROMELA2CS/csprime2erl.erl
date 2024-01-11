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
startfun(FMPid, FPid, Source) ->
    FMPid ! {self(), {deffun, Source}},
    receive
        {FMPid, Funname} ->
            % io:format("~p~n", [Funname]),
            write({self(), {append, f}}, FPid),
            write({self(), {append, Funname}}, FPid),
            write({self(), {nl, "(GPid) ->"}}, FPid)
    end.

endfun(FMPid, FPid, Target) ->
    {Loc, Chan} = Target,
    case Loc of
        exit ->
            write({self(), {append, "   "}}, FPid),
            write({self(), {append, exit}}, FPid),
            write({self(), {nl, "(GPid)."}}, FPid);
        _ ->
            FMPid ! {self(), {deffun, Target}},
            receive
                {FMPid, Funname} ->
                    write({self(), {append, "   "}}, FPid),
                    write({self(), {append, f}}, FPid),
                    write({self(), {append, Funname}}, FPid),
                    write({self(), {nl, "(GPid)."}}, FPid)
            end
    end.

genexit(FPid) ->
    write({self(), {append, exit}}, FPid),
    io:format("aaa~n"),
    write({self(), {nl, "(GPid) ->"}}, FPid),
    write({self(), {append, "   fin."}}, FPid).

moduleSetup(ModuleName, FPid) ->
    write({self(), {append, "-module("}}, FPid),
    write({self(), {append, ModuleName}}, FPid),
    write({self(), {nl, ")."}}, FPid),
    write({self(), {nl, "-export([start/0])."}}, FPid),
    write({self(), {nl, "start() ->"}}, FPid),
    write({self(), {nl, "   GPid = spawn(fun() -> globalvarmanager:loop() end),"}}, FPid),
    write({self(), {nl, "   f0(GPid)."}}, FPid).

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
    % {SLocList, SChanList} = Source,
    % {TLocList, TChanList} = Target,
    {Guard, Act} = ActLabel,
    case Guard of
        true when Act == skip ->
            startfun(FMPid, FPid, Source),
            endfun(FMPid, FPid, Target);
        true ->
            case Act#tree.value of
                stmnt10 ->              %break
                    startfun(FMPid, FPid, Source),
                    endfun(FMPid, FPid, Target);
                stmnt8 ->
                    ActChild = Act#tree.children,
                    analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target);

                stmnt13 ->
                    startfun(FMPid, FPid, Source),
                    % write({self(), {append, "   "}}),
                    % write({self(), {append, }})
                    endfun(FMPid, FPid, Target);
                stmnt14 ->
                    startfun(FMPid, FPid, Source),
                    endfun(FMPid, FPid, Target)
                % one_decl ->
            end
    end.

analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == assign1 ->
    Assign1Child = ActChild#tree.children,        %[varref, any_expr]
    [VarrefTree|Tale] = Assign1Child,
    Varname = varref(VarrefTree),
    Any_exprTree = hd(Tale),
    Value = any_expr(Any_exprTree),
    startfun(FMPid, FPid, Source),
    write({self(), {append, "   GPid ! {self(), {reg, "}}, FPid),
    write({self(), {append, Varname}}, FPid),
    write({self(), {append, ", "}}, FPid),
    write({self(), {append, Value}}, FPid),
    write({self(), {nl, "}}"}}, FPid),
    write({self(), {nl, "   receive"}}, FPid),
    write({self(), {nl, "       {GPid, fin} ->"}}, FPid),
    write({self(), {nl, "           ok"}}, FPid),
    write({self(), {nl, "   end,"}}, FPid),
    endfun(FMPid, FPid, Target).

% analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == send1 ->
% analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == receive1 ->
% analyzeStmnt8(ActChild, ChanList, MtypeList, FPid, FMPid, Source, Target) when ActChild#tree.value == expr ->

analyzeStmnt13(StmntchildTree) ->
    StmntchildTree#tree.value.

varref(VarrefTree) ->
    Tmp = hd(VarrefTree),
    VarrefChild = Tmp#tree.children,
    Tmp2 = hd(VarrefChild),
    NameTree = hd(Tmp2),
    Name = NameTree#tree.value,
    Name.

any_expr(Any_exprTree) ->
    Tmp = hd(Any_exprTree),
    case Tmp#tree.value of
        any_expr6 ->
            Any_exprChild = Tmp#tree.children,
            case Any_exprChild#tree.value of
                varref ->
                    Name = varref(Any_exprChild#tree.children),
                    Name;
                const ->
                    Const = Any_exprChild#tree.children,
                    Const
            end;
        _ ->
            skip
    end.