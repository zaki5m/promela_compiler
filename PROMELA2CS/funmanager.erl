-module(funmanager).
-export([start/1]).

start(I) ->
    funmanager(I).

funmanager(I) ->
    receive
        {From, fin} ->
            io:format("~p~n", [get()]),
            From ! {self(), ok};
        {From, {deffun, State}} ->
            case get(State) of
                undefined ->
                    put(State, I),
                    % io:format("I:~p~n", [I]),
                    From ! {self(), I},
                    funmanager(I+1);
                Value ->
                    From ! {self(), Value},
                    funmanager(I)
            end
    end.