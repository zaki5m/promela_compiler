-module(globalvarmanager).
-export([loop/0]).

loop() ->
    receive
        {From, fin} ->
            From ! {self(), get()};
        {From, {reg, Varname, Value}} ->
            put(Varname, Value),
            From ! {self(), fin},
            loop();
        {From, {get, Varname}} ->
            Return = get(Varname),
            From ! {self(), Return},
            loop()
    end.

