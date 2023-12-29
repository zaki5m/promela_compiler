-module(setmanager).
-export([start/0, addchan/1]).
-include("record.hrl").

judgeGuardsr(Guard) ->
    case Guard#tree.value of
        stmnt4guard1 ->
            notsr;
        stmnt4guard2 ->
            GuardChild = Guard#tree.children,
            case GuardChild#tree.value of
                send1 ->
                    sr;
                receive1 ->
                    sr;
                _ ->
                    notsr
            end
    end.

judgeActsr(Act) when Act == skip ->
        notsr;
judgeActsr(Act) ->
    case Act#tree.value of
        stmnt8 ->
            ActChild = Act#tree.children,
            case ActChild#tree.value of
                send1 ->
                    sr;
                receive1 ->
                    sr;
                _ ->
                    notsr
            end;
        _ ->
            notsr
    end.

% guard,actの両方にreceive,sendが来ているエッジの分割をする関数
splitReceiveSend([], _, NewEdgeList, AddedLoc) ->
    {NewEdgeList, AddedLoc};
splitReceiveSend([Edge|Edges], I, NewEdgeList, AddedLoc) ->
    {Source, ActLabel, Target} = Edge,
    case ActLabel of
        {true, Act} ->
            TmpList = NewEdgeList ++ [Edge],
            splitReceiveSend(Edges, I, TmpList, AddedLoc);
        {Guard, Act} ->
            % JudgeGResult = judgeGuardsr(Guard),
            case judgeGuardsr(Guard) of
                notsr ->
                    TmpList = NewEdgeList ++ [Edge],
                    splitReceiveSend(Edges, I, TmpList, AddedLoc);
                sr ->
                    case judgeActsr(Act) of
                        notsr ->
                            TmpList = NewEdgeList ++ [Edge],
                            splitReceiveSend(Edges, I, TmpList, AddedLoc);
                        sr ->
                            NewEdge1 = {Source, {Guard, skip}, I},
                            NewEdge2 = {I, {true, Act}, Target},
                            TmpList = NewEdgeList ++ [NewEdge1] ++ [NewEdge2],
                            NewAddedLoc = AddedLoc ++ [I],
                            splitReceiveSend(Edges, I+1, TmpList, NewAddedLoc)
                    end
            end
    end.



% 重複した要素を削除する関数
remove_duplicates([]) ->
    [];
remove_duplicates([X]) ->
    [X];
remove_duplicates([X, X | Rest]) ->
    remove_duplicates([X | Rest]);
remove_duplicates([X, Y | Rest]) ->
    [X | remove_duplicates([Y | Rest])].

start() ->
    addel([], [], []).

addel(Loc, Act, Edge) ->
    receive
        {From, {check, Value}} ->
            Result = lists:member(Value, Edge),
            From ! {self(), Result},
            addel(Loc, Act, Edge);
        {From, {loc, Value}} ->
            % io:format("aaa~n"),
            NewLoc = Loc ++ [Value],
            From ! {self(), ok},
            addel(NewLoc, Act, Edge);
        {From, {act, Value}} ->
            % io:format("bbb~n"),
            NewAct = Act ++ [Value],
            From ! {self(), ok},
            addel(Loc, NewAct, Edge);
        {From, {edge, Value}} ->
            % io:format("ccc~n"),
            NewEdge = Edge ++ [Value],
            From ! {self(), ok},
            addel(Loc, Act, NewEdge);
        {From, fin} ->
            % io:format("ddd~n"),
            NewLoc = remove_duplicates(Loc),
            LenNewLoc = length(NewLoc),
            {NewEdgeList, AddedLoc} = splitReceiveSend(Edge, LenNewLoc, [], []),
            NewLoc2 = NewLoc ++ AddedLoc,
            % io:format("~p~p~p~n", [NewLoc, Act, Edge]),
            From ! {self(), {NewLoc2, Act, NewEdgeList}}
    end.

addchan(Chan) ->
    receive
        {From, {chan, Value}} ->
            NewChan = Chan ++ [Value],
            From ! {self(), ok},
            addchan(NewChan);
        {From, fin} ->
            % io:format("~p~n", [Chan]),
            From ! {self(), Chan}
    end.