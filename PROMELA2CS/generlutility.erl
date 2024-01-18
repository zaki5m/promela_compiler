-module(generlutility).
-export([getguardvar/2, any_expr/2, write/2, valuelistWrite/2, expr/3, writeListOperationForList/2, genexit/1, moduleSetup/2, declGuardVar/2, declVar/2, startfun/3, endfun/4, varref/1, recfun/3, genSend/2, genReceive/4, send_args/1]).
-include("record.hrl").

%% ガード部分で用いる変数を取得する
getguardvar([], VarList) ->
    NewVarList = lists:usort(VarList),
    NewVarList;
getguardvar([Edge|Edges], VarList) ->
    {_, {Guard, _}, _} = Edge,
    case Guard of
        true ->
            getguardvar(Edges, VarList);
        _ ->
            GuardChild = Guard#tree.children,
            case Guard#tree.value of
                stmnt4guard1 ->
                    getguardvar(Edges, VarList);
                stmnt4guard2 when GuardChild#tree.value == expr1 ->
                    Expr1Child = GuardChild#tree.children,
                    {_, NewVarList} = any_expr(Expr1Child, VarList),
                    getguardvar(Edges, NewVarList);
                stmnt4guard2 when GuardChild#tree.value == expr2 ->
                    Expr2Child  = GuardChild#tree.children,
                    NewVarList = getguardvar(Expr2Child, VarList),
                    getguardvar(Edges, NewVarList)
            end
    end.

expr(ExprTree, FPid, Flag) when ExprTree#tree.value == expr1 ->
    Expr1Child = ExprTree#tree.children,
    {ValueList, TmpVarList} = any_expr(Expr1Child, []),
    VarList = lists:usort(TmpVarList),
    NewValueList = lists:map(fun(X) -> case is_atom(X) of true -> atom_to_list(X); false -> X end end, ValueList),
    case Flag of
        act ->
            declVar(VarList, FPid),
            write({self(), {append, "   "}}, FPid),
            valuelistWrite(NewValueList, FPid),
            write({self(), {nl, ","}}, FPid);
        guard ->
            write({self(), {append, "   "}}, FPid),
            guardvaluelistWrite(NewValueList, FPid),
            write({self(), {nl, " ->"}}, FPid)
    end;
expr(ExprTree, FPid, Flag) when ExprTree#tree.value == expr2 ->
    Expr2Child = ExprTree#tree.children,
    write({self(), {nl, "   ("}}, FPid),
    expr(Expr2Child, FPid, Flag),
    write({self(), {nl, "   )"}}, FPid).


%% expr内での値のリストと変数のリストを返す
any_expr(Any_exprTree, VarList) ->
    io:format("AnyExpr: ~p~n", [Any_exprTree]),
    ValueList = [],
    Tmp = hd(Any_exprTree),
    case Tmp#tree.value of
        any_expr1 ->
            Any_exprChild = Tmp#tree.children,
            io:format("Any1:~p~n", [Any_exprChild]),
            {TmpList, NewVarList} = any_expr(Any_exprChild, VarList),
            NewValueList = ['('] ++ TmpList ++ [')'],
            {NewValueList, NewVarList};
        any_expr2 ->
            Any_exprChild = Tmp#tree.children,
            {NewValueList, NewVarList} = calc(Any_exprChild, VarList),
            {NewValueList, NewVarList};
        any_expr6 ->
            Any_exprChild = Tmp#tree.children,
            case is_list(Any_exprChild) of
                true ->
                    Name = varref(Any_exprChild),
                    NewValueList = ValueList ++ [Name],
                    NewVarList = [Name|VarList],
                    {NewValueList, NewVarList};
                false ->
                    Const = Any_exprChild#tree.children,
                    NewValueList = ValueList ++ [Const],
                    {NewValueList, VarList}
            end;
        _ ->
            skip
    end.

varref(VarrefTreeList) ->
    Tmp = hd(VarrefTreeList),
    VarrefChild = Tmp#tree.children,
    Tmp2 = hd(VarrefChild),
    NameTree = hd(Tmp2),
    Name = NameTree#tree.children,
    Name.


calc(Any_exprChild, VarList) ->
    {LH, OpTree, RH} = list_to_tuple(Any_exprChild),
    % LAny_exprTree = hd(LH),
    % RAny_exprTree = hd(RH),
    {LHVlueList, LHVarList} = generlutility:any_expr(LH, VarList),
    TmpOp = OpTree#tree.children,
    Op = convbinarop(TmpOp),
    {RHValueList, RHVarList} = generlutility:any_expr(RH, LHVarList),
    ValueList =  LHVlueList ++ [Op] ++ RHValueList,
    {ValueList, RHVarList}.

%% 二項演算子をPromeraの演算子からErlangの演算子に変換する
convbinarop(BinOpAtom) when is_tuple(BinOpAtom) ->
    NewBinOpAtom = BinOpAtom#tree.children,
    case NewBinOpAtom of
        '&&' ->
            'and';
        '||' ->
            'or'
    end;
convbinarop(BinOpAtom) ->
    case BinOpAtom of
        '/' ->
            'div';
        '!=' ->
            '/=';
        '%' ->
            'rem';
        '&' ->
            'band';
        '|' ->
            'bor';
        _ ->
            BinOpAtom
    end.


write(Msg, FPid) ->
    FPid ! Msg,
    receive
        {FPid, fin} ->
            fin
    end.

valuelistWrite([], _) ->
    fin;
valuelistWrite([Value|Values], FPid) ->
    write({self(), {append, Value}}, FPid),
    write({self(), {append, " "}}, FPid),
    valuelistWrite(Values, FPid).

guardvaluelistWrite([], _) ->
    fin;
guardvaluelistWrite([Value|Values], FPid) ->
    write({self(), {append, "Tmp"}}, FPid),
    write({self(), {append, Value}}, FPid),
    write({self(), {append, " "}}, FPid),
    valuelistWrite(Values, FPid).

startfun(FMPid, FPid, Source) ->
    FMPid ! {self(), {deffun, Source}},
    receive
        {FMPid, Funname} ->
            % io:format("~p~n", [Funname]),
            write({self(), {append, f}}, FPid),
            write({self(), {append, Funname}}, FPid),
            write({self(), {nl, "(VarList) ->"}}, FPid)
    end.

endfun(FMPid, FPid, Target, VarListChangeFlag) ->
    {Loc, _} = Target,
    case Loc of
        exit ->
            write({self(), {append, "   "}}, FPid),
            write({self(), {append, exit}}, FPid),
            write({self(), {append, "()"}}, FPid);
        _ ->
            FMPid ! {self(), {deffun, Target}},
            receive
                {FMPid, Funname} ->
                    write({self(), {append, "   "}}, FPid),
                    write({self(), {append, f}}, FPid),
                    write({self(), {append, Funname}}, FPid),
                    case VarListChangeFlag of
                        nochange ->
                            write({self(), {append, "(VarList)"}}, FPid);
                        changed ->
                            write({self(), {append, "(NewVarList)"}}, FPid)
                    end
            end
    end.

% 関数を再帰するときに呼び出す関数
recfun(FMPid, FPid, Source) ->
    FMPid ! {self(), {deffun, Source}},
    receive
        {FMPid, Funname} ->
            % io:format("~p~n", [Funname]),
            write({self(), {append, f}}, FPid),
            write({self(), {append, Funname}}, FPid),
            write({self(), {append, "(VarList)"}}, FPid)
    end.

genexit(FPid) ->
    write({self(), {append, exit}}, FPid),
    % write({self(), {nl, "(GPid) ->"}}, FPid),
    write({self(), {nl, "() ->"}}, FPid),
    write({self(), {append, "   fin."}}, FPid).

moduleSetup(ModuleName, FPid) ->
    write({self(), {append, "-module("}}, FPid),
    write({self(), {append, ModuleName}}, FPid),
    write({self(), {nl, ")."}}, FPid),
    write({self(), {nl, "-export([start/0])."}}, FPid),
    write({self(), {nl, "start() ->"}}, FPid),
    % write({self(), {nl, "   GPid = spawn(fun() -> globalvarmanager:loop() end),"}}, FPid),
    % write({self(), {nl, "   f0(GPid)."}}, FPid).
    write({self(), {nl, "   f0([])."}}, FPid).

declVar([], _) ->
    fin;
declVar([Var|Vars], FPid) ->
    write({self(), {append, "   {_, "}}, FPid),
    TmpVar = atom_to_list(Var),
    write({self(), {append, TmpVar}}, FPid),
    write({self(), {append, "} = lists:filter(fun(X) -> {Tmpname, _} = X, Tmpname == "}}, FPid),
    write({self(), {append, Var}}, FPid),
    write({self(), {nl, " end, VarList),"}}, FPid),
    declVar(Vars, FPid).

declGuardVar([], _) ->
    fin;
declGuardVar([Var|Vars], FPid) ->
    write({self(), {append, "   {_, Tmp"}}, FPid),
    TmpVar = atom_to_list(Var),
    write({self(), {append, TmpVar}}, FPid),
    write({self(), {append, "} = lists:filter(fun(X) -> {Tmpname, _} = X, Tmpname == "}}, FPid),
    write({self(), {append, Var}}, FPid),
    write({self(), {nl, " end, VarList),"}}, FPid),
    declVar(Vars, FPid).

writeListOperationForList(VarName, FPid) ->
    write({self(), {append, "   TmpVarList = lists:filter(fun(X) -> {TmpVarname, _} = X, TmpVarname /= "}}, FPid),
    write({self(), {append, VarName}}, FPid),
    write({self(), {nl, " end, VarList),"}}, FPid),
    write({self(), {append, "   NewVarList = [{"}}, FPid),
    write({self(), {append, VarName}}, FPid),
    write({self(), {append, ","}}, FPid),
    fin.

writeReceivePatern([], Source, FPid, FMPid) ->
    write({self(), {nl, "      {_, Pid} ->"}}, FPid),
    write({self(), {nl, "         Pid ! { self() , no},"}}, FPid),
    write({self(), {append, "          "}}, FPid),
    write({self(), {nl, recfun(FMPid, FPid, Source)}}, FPid),% パターンにマッチしなかった場合はloopする
    fin;

writeReceivePatern([Patern | PaternList], Source, FPid, FMPid) ->
    {Buffer, Value} = Patern,
    write({self(), {append, "      { {"}}, FPid),
    write({self(), {append, Buffer}}, FPid),
    write({self(), {append, " , "}}, FPid),
    write({self(), {append, Value}}, FPid),
    write({self(), {nl, " } , Pid } ->"}}, FPid),
    write({self(), {nl, "         Pid ! { self(), ok }"}}, FPid),
    write({self(), {nl, "         fin;"}}, FPid),
    writeReceivePatern(PaternList, Source, FPid, FMPid).


% PaternListはreceiveのパターンのリスト
% {buffer, 値}の形式である
% 例: [{c, 1}, {d, 2}, {c, 3}]
genReceive(PaternList, Source, FPid, FMPid) -> 
    write({self(), {append, "   receive"}}, FPid),
    writeReceivePatern(PaternList, Source, FPid, FMPid),
    write({self(), {nl, "   end,"}}, FPid).

writeSendPatern([], FPid) ->
    fin;

writeSendPatern([Patern | PaternList], FPid) ->
    {Buffer, ValueList} = Patern,
    write({self(), {append, "      {"}}, FPid),
    write({self(), {append, Buffer}}, FPid),
    write({self(), {append, " , "}}, FPid),
    valuelistWrite(ValueList, FPid),
    write({self(), {append, " }"}}, FPid),
    case PaternList of 
        [] ->
            write({self(), {nl, " ],"}}, FPid),
            fin;
        _ ->
            write({self(), {append, ","}}, FPid)
    end,
    writeSendPatern(PaternList, FPid).

genSend(PaternList, FPid) ->
    %まずはsender用の関数をspawnする
    write({self(), {append, "   SendList = ["}}, FPid),
    writeSendPatern(PaternList, FPid),
    write({self(), {nl, "   _ = spawn(fun () -> sendReceive:sender(SendList) end),"}}, FPid),
    fin.

send_args(SendArgs) ->
    io:format("SendArgs: ~p~n", [SendArgs]),
    Value = SendArgs#tree.value,
    SendArgsChild = SendArgs#tree.children,
    case Value of
        send_args1 -> 
            {ValueList, TmpVarList} = any_expr(SendArgsChild,[]),
            VarList = lists:usort(TmpVarList),
            NewValueList = lists:map(fun(X) -> case is_atom(X) of true -> atom_to_list(X); false -> X end end, ValueList),
            {NewValueList, VarList};
        send_args2 -> 
            %tba
            {[], []}
    end.
    
    