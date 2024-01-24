-module(funmanager4cs2erl).
-export([start/1]).

start(FunList) ->
    funmanager(FunList).

funmanager(FunList) ->
    receive
        {From, fin} ->
            io:format("~p~n", [get()]),
            From ! {self(), ok};
        {From, {deffun, Loc}} ->
            case lists:member(Loc, FunList) of
                false ->
                    NewFunList  = [Loc|FunList],
                    From ! {self(), Loc},
                    funmanager(NewFunList);
                true ->
                    From ! {self(), Loc},
                    funmanager(FunList)
            end
    end.