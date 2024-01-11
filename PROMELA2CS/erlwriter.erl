-module(erlwriter).
-export([openfile/0, closefile/1, filewrite/1]).

openfile() ->
    {ok, File} = file:open("tmp.erl", [write, append]),
    File.

closefile(File) ->
    file:close(File).

filewrite(File) ->
    receive
        {From, fin} ->
            From ! {self(), ok};
        {From, {append, Msg}} when is_list(Msg) ->
            NewMsg = remove_quotes(Msg),
            file:write(File, io_lib:format("~s", [NewMsg])),
            From ! {self(), fin},
            filewrite(File);
        {From, {nl, Msg}} when is_list(Msg) ->
            NewMsg = remove_quotes(Msg),
            file:write(File, io_lib:format("~s~n", [NewMsg])),
            From ! {self(), fin},
            filewrite(File);
        {From, {append, Msg}} ->
            file:write(File, io_lib:format("~p", [Msg])),
            From ! {self(), fin},
            filewrite(File);
        {From, {nl, Msg}} ->
            file:write(File, io_lib:format("~p~n", [Msg])),
            From ! {self(), fin},
            filewrite(File)
    end.

remove_quotes(Msg) ->
    lists:sublist(Msg, 1, length(Msg)).