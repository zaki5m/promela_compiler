-module(my_utility).
-export([listloop/2, listloop4seq/3, genedge/4, addloc/2, addact/2]).
-include("record.hrl").


listloop([], Pid) ->
    ok;
listloop([Tuple|T], Pid) ->
    Value = Tuple#tree.value,
    case Value of
        module  ->
            io:format("aa:~p~n", [Value]),
            pml2cs:module(Tuple, Pid),
            listloop(T, Pid);
        % sequence ->
        %     io:format("bb:~p~n", [Value]),
        %     pml2cs:sequence(Tuple, Pid),
        %     listloop(T, Pid);
        _       ->
            io:format("cc:~p~n", [Value]),
            listloop(T, Pid)
    end.

listloop4seq([], Pid, I) ->
    ok;
listloop4seq([Tuple|T], Pid, I) ->
    Value = Tuple#tree.value,
    case Value of
        sequence ->
            io:format("bb:~p~n", [Value]),
            pml2cs:sequence(Tuple, Pid, I),
            listloop4seq(T, Pid, I);
        _        ->
            io:format("cc:~p~n", [Value]),
            listloop4seq(T, Pid, I)
    end.

genedge(Source, Act, Target, Pid) ->
    Pid ! {self(), {arrow, {Source, Act, Target}}},
    receive
        {Pid, ok} ->
            ok
    end.

addloc(I, Pid) ->
    Pid ! {self(), {loc, I}},
    receive
        {Pid, ok} ->
            ok
    end.

addact(Act, Pid) ->
    Pid ! {self(), {act, Act}},
    receive
        {Pid, ok} ->
            ok
    end.