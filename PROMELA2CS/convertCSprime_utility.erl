-module(convertCSprime_utility).
-export([operateChan/1, operatePG/1, searchChanPid/2, searchSendarg/1, searchReceivearg/1]).
-include("record.hrl").

judge(Edge, Loc) ->
    {Source, _, _} = Edge,
    Source == Loc.  % boolean


operatePG(Edges) ->
    receive
        {From, fin} ->
            fin;
        {From, Loc, NumPG} ->
            FilteredEdges = lists:filter(fun(X) -> judge(X, Loc) end, Edges),   % Edgeのリストの要素でLocから遷移するエッジを抽出する
            MappedEdges = lists:map(fun(X) -> {NumPG, X} end, FilteredEdges),
            From ! {self(), MappedEdges},
            operatePG(Edges)
    end.

operateChan(ValueList) ->
    receive
        {From, fin} ->
            fin;
        {From, read} ->
            case length(ValueList) of
                0 ->
                    From ! {self(), stay},
                    operateChan(ValueList);
                1 ->
                    NewValueList = tl(ValueList),
                    From ! {self(), NewValueList},
                    operateChan(NewValueList)
            end;
        {From, {write, Value}} ->
            case length(ValueList) of
                1 ->
                    From ! {self(), stay},
                    operateChan(ValueList);
                0 ->
                    NewValueList = ValueList ++ [Value],
                    From ! {self(), NewValueList},
                    operateChan(NewValueList)
            end
    end.

searchChanPid(Channame, ChanPidList) ->
    Tmp = lists:filter(fun(X) -> {Name,_} = X, Name == Channame end, ChanPidList),
    {_, Pid} = hd(Tmp),
    Pid.

searchSendarg(SendArgs) ->
    Tmp = SendArgs#tree.children, % [any_expr]
    Any_exprTree = hd(Tmp),
    Tmp2 = Any_exprTree#tree.children,
    case is_list(Tmp2) of
        true ->
            VarrefTree = hd(Tmp2),
            Tmp3 = VarrefTree#tree.children,
            Tmp4 =hd(Tmp3),
            NameTree = hd(Tmp4),
            NameTree#tree.children;
        false ->
            Tmp2#tree.children
    end.

searchReceivearg(SendArgs) ->
    Tmp = SendArgs#tree.children, % [any_expr]
    Any_exprTree = hd(Tmp),
    Tmp5 = hd(Any_exprTree),
    Tmp2 = Tmp5#tree.children,
    case is_list(Tmp2) of
        true ->
            VarrefTree = hd(Tmp2),
            Tmp3 = VarrefTree#tree.children,
            Tmp4 =hd(Tmp3),
            NameTree = hd(Tmp4),
            NameTree#tree.children;
        false ->
            Tmp2#tree.children
    end.