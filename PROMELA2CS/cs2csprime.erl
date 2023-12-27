-module(cs2csprime).
-export([start/3]).
-include("record.hrl").

% PGの数分だけLocListにinitialLocを追加し，CS'のイニシャルステートを生成する

genInitState(0, [], LocList, ChanValueList) ->
    {LocList, ChanValueList};
genInitState(0, [_|Chans], LocList, ChanValueList) ->
    NewChanValueList = ChanValueList ++ [[]],
    genInitState(0, Chans, LocList, NewChanValueList);
genInitState(Numproc, ChanList, LocList, ChanValueList) ->
    NewLocList = LocList ++ [0],
    genInitState(Numproc-1, ChanList, NewLocList, ChanValueList).

genChanPid([],PidList) ->
    PidList;
genChanPid([Chan|Chans], PidList) ->
    Pid = spawn(fun() -> convertCSprime_utility:operateChan([]) end),
    NewPidList = PidList ++ [{Chan, Pid}],
    genChanPid(Chans, NewPidList).

% list -> List -> List
% PGのリストを受け取り各PGを管理するための関数を評価するプロセスを生成し，そのPidのリストを返す
genPGpid([], PidList) ->
    PidList;
genPGpid([PG|PGs], PidList) ->
    {_, _, Edge} = PG,
    Pid = spawn(fun() -> convertCSprime_utility:operatePG(Edge) end),
    NewPidList = PidList ++ [Pid],
    genPGpid(PGs, NewPidList).

finChanPid([]) ->
    fin;
finChanPid([H|T]) ->
    {_, ChanPid} = H,
    ChanPid ! {self(), fin},
    finChanPid(T).

% Pidのリストを受け取り各Pidに終了命令を送る
finPGPid([]) ->
    fin;
finPGPid([Pid|Pids]) ->
    Pid ! {self(), fin},
    finPGPid(Pids).

start(Numproc, PGList, ChanList) ->
    SetPid = spawn(fun() -> setmanager:start() end),        %CS'のState,Act,Edgeを管理するモジュールの関数へのプロセス生成
    ChanPidList = genChanPid(ChanList, []),     %チャネルの中身を管理しているモジュールの関数へのプロセス生成
    PGPidList = genPGpid(PGList, []),
    InitState = genInitState(Numproc, ChanList, [], []),
    my_utility:addloc(InitState, SetPid),
    genCSprime([InitState], Numproc, ChanList, SetPid, ChanPidList, PGPidList),
    SetPid ! {self(), fin},
    receive
        {SetPid, {States, Acts, Edges}} ->
            io:format("~p~p~n", [States, Edges])
    end,
    finChanPid(ChanPidList),
    finPGPid(PGPidList).

genCSprime([], _, _, _, _, _) ->
    fin;
genCSprime([State|States], Numproc, ChanList, SetPid, ChanPidList, PGPidList) ->     %1
    NewStateListwithAct = analyzeNextState(State, ChanList, ChanPidList, PGPidList, SetPid),
    genEdge2NewState(State, NewStateListwithAct, SetPid),
    NewStateList = lists:map(fun(X) -> {_, NewState} = X, NewState end, NewStateListwithAct),
    genCSprime(NewStateList, Numproc, ChanList, SetPid, ChanPidList, PGPidList),
    genCSprime(States, Numproc, ChanList, SetPid, ChanPidList, PGPidList).

analyzeNextState(State, ChanList, ChanPidList, PGPidList, SetPid) ->          %2
    {LocList, ChanValueList} = State,
    NextEdgeList = analyzeLocList(LocList, [], 0, PGPidList),
    NewStateListwithAct = genNextStateList(LocList, NextEdgeList, 0, ChanList, ChanPidList, [], ChanValueList, [], SetPid),
    NewStateListwithAct.

genEdge2NewState(_, [], _) ->             %3
    fin;
genEdge2NewState(Source, [NewStatewithAct|NewStatewithActs], SetPid) ->
    {ActLabel, Target} = NewStatewithAct,
    my_utility:addloc(Target, SetPid),
    my_utility:addact(ActLabel, SetPid),
    my_utility:genedge(Source, ActLabel, Target, SetPid),
    genEdge2NewState(Source, NewStatewithActs, SetPid).

analyzeLocList([], List, _, _) ->
    List;
analyzeLocList([Loc|Locs], List, I, PGPidList) ->          %4
    NextInfoList = analyzeNextLoc(Loc, I, PGPidList),  %to6
    NewNextLocList = List ++ NextInfoList,
    analyzeLocList(Locs, NewNextLocList, I+1, PGPidList).

genNextStateList([], _, _, _, _, _, _, StateListwithAct, _) ->
    StateListwithAct;
genNextStateList([Loc|Locs], NextEdgeList, I, ChanList, ChanPidList, PreLocs, ChanValueList, StateListwithAct, SetPid) ->       %5
    IthPGNextEdgeList = lists:filter(fun(X) -> {NumPG,_} = X, I == NumPG end, NextEdgeList),   % I番目のPGの遷移を抽出
    IthPGNextEdgeList2 = lists:map(fun(X) -> {_,Edge} = X, Edge end, IthPGNextEdgeList),
    NextLocList = judgeNextStay(IthPGNextEdgeList2, [], ChanPidList),  %このリストの通りに遷移していいかどうかを判断して遷移していいやつとstayするやつはstayに置き換えたリスト
    TmpList = genNextState(PreLocs, Locs, NextLocList, ChanValueList, [], SetPid),
    NewStateListwithAct = StateListwithAct ++ TmpList,
    NewPreLocs = PreLocs ++ [Loc],
    genNextStateList(Locs, NextEdgeList, I+1, ChanList, ChanPidList, NewPreLocs, ChanValueList, NewStateListwithAct, SetPid).

genNextState(_, _, [], _, StateListwithAct, _) ->
    StateListwithAct;
genNextState(PreLocs, Locs, [NextLoc|NextLocs], ChanValueList ,StateListwithAct, SetPid) ->
    {ActLabel, Target, NewChanValueList} = NextLoc,
    case ActLabel of
        stay ->
            genNextState(PreLocs, Locs, NextLocs, ChanValueList, StateListwithAct, SetPid);
        X ->
            NewLocs = PreLocs ++ [Target] ++ Locs,
            case NewChanValueList of
                nochange ->
                    NewState = {NewLocs, ChanValueList};
                Other ->
                    NewState = {NewLocs, [Other]}
            end,
            Judge = my_utility:checkstate(NewState, SetPid),
            case Judge of
                false ->
                    NewStateListwithAct = StateListwithAct ++ [{X, NewState}],
                    genNextState(PreLocs, Locs, NextLocs, ChanValueList, NewStateListwithAct, SetPid);
                true ->
                    genNextState(PreLocs, Locs, NextLocs, ChanValueList, StateListwithAct, SetPid)
            end
    end.


analyzeNextLoc(Loc, NumPG, PGPidList) ->                  %6
    PGPid = lists:nth(NumPG+1, PGPidList),
    PGPid ! {self(), Loc, NumPG},
    receive
        {PGPid, Edges} ->
            Edges
    end.

judgeNextStay([], NextLocList, _) ->           %7
    NextLocList;
judgeNextStay([Edge|Edges], NextLocList, ChanPidList) ->           %7
    {_, ActLabel, Target} = Edge,
    case ActLabel of
        {true, Act} ->
            NewNextLocList = judgeAct(true, Act, Target, NextLocList, ChanPidList, nochange),
            judgeNextStay(Edges, NewNextLocList, ChanPidList);
        {Guard,Act} ->
            GuardChild = Guard#tree.children,
            case Guard#tree.value of
                stmnt4guard1 ->
                    NewNextLocList = judgeAct(Guard, Act, Target, NextLocList, ChanPidList, nochange),
                    judgeNextStay(Edges, NewNextLocList, ChanPidList);
                stmnt4guard2 when GuardChild#tree.value == send1 ->
                    SendArgs = GuardChild#tree.children,
                    Result = chanjudge(SendArgs, ChanPidList, senddesu),
                    case Result of
                        stay ->
                            NewNextLocList = NextLocList ++ [{stay, stay, nochange}],
                            judgeNextStay(Edges, NewNextLocList, ChanPidList);
                        {next, NewChanValueList} ->
                            NewNextLocList = judgeAct(Guard, Act, Target, NextLocList, ChanPidList, NewChanValueList),
                            judgeNextStay(Edges, NewNextLocList, ChanPidList)
                    end;
                stmnt4guard2 when GuardChild#tree.value == receive1 ->
                    ReceiveArgs = GuardChild#tree.children,
                    Result = chanjudge(ReceiveArgs, ChanPidList, receivedesu),
                    case Result of
                        stay ->
                            NewNextLocList = NextLocList ++ [{stay, stay, nochange}],
                            judgeNextStay(Edges, NewNextLocList, ChanPidList);
                        {next, NewChanValueList} ->
                            NewNextLocList = judgeAct(Guard, Act, Target, NextLocList, ChanPidList, NewChanValueList),
                            judgeNextStay(Edges, NewNextLocList, ChanPidList)
                    end;
                stmnt4guard2 ->
                    NewNextLocList = judgeAct(Guard, Act, Target, NextLocList, ChanPidList, nochange),
                    judgeNextStay(Edges, NewNextLocList, ChanPidList)
            end
    end.
judgeAct(Guard, Act, Target, NextLocList, ChanPidList, ChanValueList) when Act == skip ->
    NewNextLocList = NextLocList ++ [{{Guard, Act}, Target, nochange}],
    NewNextLocList;
judgeAct(Guard, Act, Target, NextLocList, ChanPidList, ChanValueList) ->
    case Act#tree.value of
        stmnt8 ->
            ActChild = Act#tree.children,
            case ActChild#tree.value of
                send1 ->
                    SendArgs = ActChild#tree.children,
                    Result = chanjudge(SendArgs, ChanPidList, senddesu),
                    case Result of
                        stay when Guard == true ->
                            NewNextLocList = NextLocList ++ [{stay, stay, nochange}],
                            NewNextLocList;
                        stay ->
                            NewNextLocList = NextLocList ++ [{stay, stay, ChanValueList}],
                            NewNextLocList;
                        {next, NewChanValueList} ->
                            NewNextLocList = NextLocList ++ [{{Guard, Act}, Target, NewChanValueList}],
                            NewNextLocList
                    end;
                receive1 ->
                    ReceiveArgs = ActChild#tree.children,
                    Result = chanjudge(ReceiveArgs, ChanPidList, receivedesu),
                    case Result of
                        stay when Guard == true ->
                            NewNextLocList = NextLocList ++ [{stay, stay, nochange}],
                            NewNextLocList;
                        stay ->
                            NewNextLocList = NextLocList ++ [{stay, stay, ChanValueList}],
                            NewNextLocList;
                        {next, NewChanValueList} ->
                            NewNextLocList = NextLocList ++ [{{Guard, Act}, Target, NewChanValueList}],
                            NewNextLocList
                    end;
                Other ->
                    NewNextLocList = NextLocList ++ [{{Guard, Act}, Target, nochange}],
                    NewNextLocList
            end;
        stmnt10 ->
            NewNextLocList = NextLocList ++ [{{Guard, Act}, Target, nochange}],
            NewNextLocList;
        stmnt13 ->
            NewNextLocList = NextLocList ++ [{{Guard, Act}, Target, nochange}],
            NewNextLocList;
        stmnt14 ->
            NewNextLocList = NextLocList ++ [{{Guard, Act}, Target, nochange}],
            NewNextLocList;
        one_decl ->
            NewNextLocList = NextLocList ++ [{{Guard, Act}, Target, nochange}],
            NewNextLocList
    end.

chanjudge(SendChildList, ChanPidList, SRFlag) ->
    Varref = hd(SendChildList),
    Tmp = hd(Varref),                   % varref:[name] (any_expr) (varref)
    Tmp2 = hd(Tmp#tree.children),       % [name]
    ChannameTree = hd(Tmp2),            % name
    Channame = ChannameTree#tree.children,
    Tmp3 = tl(SendChildList),
    ChanPid = convertCSprime_utility:searchChanPid(Channame, ChanPidList),
    case SRFlag of
        senddesu ->
            SendArgsTree = hd(Tmp3),
            SendArg = convertCSprime_utility:searchSendarg(SendArgsTree),
            ChanPid ! {self(), {write, SendArg}},
            receive
                {ChanPid, stay} ->
                    stay;
                {ChanPid, NewChanValueList} ->
                    {next, NewChanValueList}
            end;
        receivedesu ->
            % ReceiveArgsTree = hd(Tmp3),
            % ReceiveArg = convertCSprime_utility:searchReceivearg(ReceiveArgsTree),
            ChanPid ! {self(), read},
            receive
                {ChanPid, stay} ->
                    stay;
                {ChanPid, NewChanValueList} ->
                    {next, NewChanValueList}
            end
    end.