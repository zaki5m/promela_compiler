-module(setmanager).
-export([start/0, addchan/1]).

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

addel(Loc, Act, Arrow) ->
    receive
        {From, {check, Value}} ->
            Result = lists:member(Value, Loc),
            From ! {self(), Result},
            addel(Loc, Act, Arrow);
        {From, {loc, Value}} ->
            % io:format("aaa~n"),
            NewLoc = Loc ++ [Value],
            From ! {self(), ok},
            addel(NewLoc, Act, Arrow);
        {From, {act, Value}} ->
            % io:format("bbb~n"),
            NewAct = Act ++ [Value],
            From ! {self(), ok},
            addel(Loc, NewAct, Arrow);
        {From, {arrow, Value}} ->
            % io:format("ccc~n"),
            NewArrow = Arrow ++ [Value],
            From ! {self(), ok},
            addel(Loc, Act, NewArrow);
        {From, fin} ->
            % io:format("ddd~n"),
            NewLoc = remove_duplicates(Loc),
            % io:format("~p~p~p~n", [NewLoc, Act, Arrow]),
            From ! {self(), {NewLoc, Act, Arrow}}
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