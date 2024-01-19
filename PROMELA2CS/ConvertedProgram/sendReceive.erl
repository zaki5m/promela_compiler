-module(sendReceive).
-export([start/2, sender/2, receiver/0]).

start([], PidList) ->
    pidPool(PidList),
    fin;

start([ModuleName|ModuleNameList], PidList) ->
    ManagerPid = self(),
    Pid = spawn(fun () -> apply(ModuleName,start,[ManagerPid]) end),
    io:format("Pid: ~p~n", [Pid]),
    start(ModuleNameList, [Pid|PidList]).

pidPool([]) ->
    fin;

pidPool(PidList) ->
    io:format("PidList: ~p~n", [PidList]),
    receive 
        {Pid , kill} ->
            NewPidList = lists:delete(Pid, PidList),
            pidPool(NewPidList);
        {Pid, Send} ->
            io:format("PidPool: ~p ~p~n", [Pid, Send]),
            lists:foldl(fun (TmpPid, _) -> TmpPid ! {Send, Pid} end, fin, PidList),
            pidPool(PidList);
        _ -> 
            pidPool(PidList)
    end.
sender([], _) ->
    receive 
        {From, _} ->
            From ! {self(), accept}
    end,
    fin;

sender([Send|SendList], ManagerPid) ->
    io:format("Send: ~p~n", [Send]),
    ManagerPid ! {self(), Send},
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
