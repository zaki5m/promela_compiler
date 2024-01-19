-module(bbb).
-export([start/1]).
start(ManagerPid) ->
   f0([],ManagerPid).
f0(VarList,ManagerPid) ->
   receive
      { {c , true } , Pid } ->
         Pid ! { self(), ok },
         IsReceive = sendReceive:receiver(),
         case IsReceive of
           true -> fin;
           false -> f0(VarList,ManagerPid)
         end;
      {_, Pid} ->
         Pid ! { self() , no},
          f0(VarList,ManagerPid)
   end,
   f1(VarList,ManagerPid).
f1(VarList,ManagerPid) ->
   SendList = [      {d , false  } ],
   _ = spawn(fun () -> sendReceive:sender(SendList,ManagerPid) end),
   f2(VarList,ManagerPid).
f2(VarList,ManagerPid) ->
   exitFun(ManagerPid).
exitFun(ManagerPid) ->
   ManagerPid ! { self(), kill },
   fin.
