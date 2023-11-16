-module(setmanager).
-export([start/0]).
start() ->
    addel([], [], []).

addel(Loc, Act, Arrow) ->
    receive
        {From, {loc, Value}} ->
            io:format("aaa~n"),
            NewLoc = Loc ++ [Value],
            From ! {self(), ok},
            addel(NewLoc, Act, Arrow);
        {From, {act, Value}} ->
            io:format("bbb~n"),
            NewAct = Act ++ [Value],
            From ! {self(), ok},
            addel(Loc, NewAct, Arrow);
        {From, {arrow, Value}} ->
            io:format("ccc~n"),
            NewArrow = Arrow ++ [Value],
            From ! {self(), ok},
            addel(Loc, Act, NewArrow);
        {From, fin} ->
            io:format("ddd~n"),
            io:format("~p~p~p~n", [Loc, Act, Arrow]),
            {Loc, Act, Arrow}
    end.