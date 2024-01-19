-module(separateCSprime).
-export([start/3]).
-incluse("record.hrl").

abstractIthPG(Edge, I) ->
    {Source, ActLabel, Target} = Edge,
    {SLocList, SChanList} = Source,
    {TLocList, TChanList} = Target,
    IthSLoc = lists:nth(I, SLocList),
    IthTLoc = lists:nth(I, TLocList),
    NewEdge = {{IthSLoc,SChanList}, ActLabel, {IthTLoc, TChanList}},
    NewEdge.

removeSameEdge([], ExistEdgeList) ->
    ExistEdgeList;

removeSameEdge([Edge|Edges], ExistEdgeList) ->
    {{SourceLoc, _}, _, {TargetLoc, _}} = Edge,
    case lists:any(fun(X) -> {{TmpSourceLoc, _}, _, {TmpTargetLoc, _}} = X, {SourceLoc, TargetLoc} == {TmpSourceLoc, TmpTargetLoc} end, ExistEdgeList) of
        true ->
            removeSameEdge(Edges, ExistEdgeList);
        false ->
            removeSameEdge(Edges, [Edge|ExistEdgeList])
    end.

start(CSprime, NumProc, PGNameList) ->
    {_, _, Edges} = CSprime,
    IndividualPGEdgeList = separater(Edges, NumProc, 1, [], PGNameList),
    NewIndividualPGEdgeList = lists:map(fun(X) -> {ModuleName, TmpEdges} = X, {ModuleName, removeSameEdge(lists:filter(fun(Y) -> {{SourceLoc, _}, _ , {TargetLoc, _}} = Y, SourceLoc /= TargetLoc end, TmpEdges), [])} end, IndividualPGEdgeList),
    NewIndividualPGEdgeList.

separater(_, NumProc, I, IndividualPGEdgeList, _) when I > NumProc ->
    IndividualPGEdgeList;
separater(Edges, NumProc, I, IndividualPGEdgeList, PGNameList) ->
    % {Source, ActLabel, Target} = Edge,
    Tmp = genIndividualPG(Edges, I),
    IthPGEdges = lists:filter(fun(X) -> {Source, _, Target} = X, Source =/= Target end, Tmp),
    IthPGEdges2 = removeDuplicate(IthPGEdges, []),
    PGName = lists:nth(I, PGNameList),
    NewPGEdgesList = IndividualPGEdgeList ++ [{PGName, IthPGEdges2}],
    separater(Edges, NumProc, I+1, NewPGEdgesList, PGNameList).

genIndividualPG(Edges, I) ->
    IthPG = lists:map(fun(X) -> abstractIthPG(X, I) end, Edges),
    IthPG.

removeDuplicate([], List) ->
    List;
removeDuplicate([Edge|Edges], List) ->
    case lists:any(fun(X) -> X == Edge end, List) of
        true ->
            removeDuplicate(Edges, List);
        false ->
            removeDuplicate(Edges, [Edge|List])
    end.