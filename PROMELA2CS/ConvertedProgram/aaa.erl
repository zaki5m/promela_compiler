-module(aaa).
-export([start/1]).
start(ManagerPid) ->
   f0([],ManagerPid).
f0(VarList,ManagerPid) ->
   SendList = [      {c , true  } ],
   _ = spawn(fun () -> sendReceive:sender(SendList,ManagerPid) end),
   f1(VarList,ManagerPid).
f1(VarList,ManagerPid) ->
   receive
      { {d , false } , Pid } ->
         Pid ! { self(), ok },
         IsReceive = sendReceive:receiver(),
         case IsReceive of
           true -> fin;
           false -> f1(VarList,ManagerPid)
         end;
      {_, Pid} ->
         Pid ! { self() , no},
          f1(VarList,ManagerPid)
   end,
   f2(VarList,ManagerPid).
f2(VarList,ManagerPid) ->
   exitFun(ManagerPid).
exitFun(ManagerPid) ->
   ManagerPid ! { self(), kill },
   fin.
