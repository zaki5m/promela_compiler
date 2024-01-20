-module(sendReceive).
-export([start/2, sender/2, receiver/0]).

start([], PidList) ->
    pidPool(PidList, []),
    fin;

start([ModuleName|ModuleNameList], PidList) ->
    ManagerPid = self(),
    Pid = spawn(fun () -> apply(ModuleName,start,[ManagerPid]) end),
    io:format("Pid: ~p~n", [Pid]),
    start(ModuleNameList, [Pid|PidList]).

pidPool([], _) ->
    fin;

pidPool(PidList, GlobalVarList) ->
    io:format("PidList: ~p~n", [PidList]),
    receive 
        {Pid , kill} ->
            NewPidList = lists:delete(Pid, PidList),
            pidPool(NewPidList, GlobalVarList);
        {Pid, {send, Send}} ->
            io:format("PidPool: ~p ~p~n", [Pid, Send]),
            lists:foldl(fun (TmpPid, _) -> TmpPid ! {Send, Pid} end, fin, PidList),
            pidPool(PidList, GlobalVarList);
        {Pid, {globalVarGet, VarName}} ->
            VarValueList = lists:filter(fun ({Var,_}) -> Var == VarName end, GlobalVarList),
            case VarValueList of
                [] ->
                    Pid ! {VarName, undefined},
                    pidPool(PidList, GlobalVarList);
                _ ->
                    {_, VarValue} = hd(VarValueList),
                    Pid ! {VarName, VarValue},
                    pidPool(PidList, GlobalVarList)
            end;
        {_, {globalVarPut, VarName, VarValue}} ->
            TmpGlobalVarList = lists:filter(fun ({Var,_}) -> Var /= VarName end, GlobalVarList),
            NewGlobalVarList = [{VarName, VarValue}|TmpGlobalVarList],
            pidPool(PidList, NewGlobalVarList);
        {_, {globalVarPutList, TmpGlobalVarList}} ->
            case GlobalVarList of
                [] ->
                    pidPool(PidList, TmpGlobalVarList);
                _ ->
                    pidPool(PidList, GlobalVarList)
            end;
        _ -> 
            pidPool(PidList, GlobalVarList)
    end.
sender([], _) ->
    receive 
        {From, _} ->
            From ! {self(), accept}
    end,
    fin;

sender([Send|SendList], ManagerPid) ->
    io:format("Send: ~p~n", [Send]),
    ManagerPid ! {self(), {send, Send}},
    sender(SendList, ManagerPid).
    

receiver() ->
    receive
        { _, accept } ->
            true;
        {_, _} ->
            false
    after 
        1000 ->
            false
    end.
