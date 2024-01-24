-module(generlutility4cs2erl).
-export([getguardvar/2, any_expr/2, write/2, valuelistWrite/2, expr/5, writeListOperationForList/2, genexit/1, moduleSetup/4, declGuardVar/3, declVar/2, startfun/3, endfun/4, varref/1, send_args/3, ivars/2, writeAddVarList/2, recv_args/1, writeGetGlobalVar/2]).
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

expr(ExprTree, MtypeList, GlobalVarList, FPid, Flag) when ExprTree#tree.value == expr1 ->
    Expr1Child = ExprTree#tree.children,
    {ValueList, TmpVarList} = any_expr(Expr1Child, []),
    io:format("TmpVarList:~p~n", [TmpVarList]),
    io:format("MtypeList:~p~n", [MtypeList]),
    VarList = lists:filter(fun(X) -> (MtypeList == []) or not(lists:any(fun(Y) -> Y == X end, MtypeList)) end, lists:usort(TmpVarList)),
    io:format("VarList:~p~n", [VarList]),
    LocalVarList = lists:filter(fun(X) -> not(lists:any(fun({First, _}) -> First == X end, GlobalVarList)) end, VarList),
    UseGlobalVarList = lists:filter(fun(X) -> lists:any(fun({First, _}) -> First == X end, GlobalVarList) end, VarList),
    NewValueList = lists:map(fun(X) -> case is_atom(X) of true -> atom_to_list(X); false -> X end end, ValueList),
    % io:format("NewValueList:~p~n", [NewValueList]),
    case Flag of
        act ->
            writeGetGlobalVar(UseGlobalVarList, FPid),
            declVar(LocalVarList, FPid),
            write({self(), {append, "   "}}, FPid),
            valuelistWrite(NewValueList, FPid),
            write({self(), {nl, ","}}, FPid);
        guard ->
            write({self(), {append, "   "}}, FPid),
            guardvaluelistWrite(NewValueList, VarList, FPid),
            write({self(), {nl, " ->"}}, FPid)
    end;
expr(ExprTree, MtypeList, GlobalVarList, FPid, Flag) when ExprTree#tree.value == expr2 ->
    Expr2Child = ExprTree#tree.children,
    write({self(), {nl, "   ("}}, FPid),
    expr(Expr2Child, MtypeList, GlobalVarList, FPid, Flag),
    write({self(), {nl, "   )"}}, FPid).


%% expr内での値のリストと変数のリストを返す
any_expr(Any_exprTree, VarList) ->
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

ivars([], VarList) ->
    VarList;
ivars([Ivar|Ivars], VarList) ->
    IvarChild = Ivar#tree.children,
    Tmp = lists:nth(1, IvarChild),
    NameTree = hd(Tmp),
    Tmp2 = lists:nth(3, IvarChild),
    case is_list(Tmp2) of
        false ->
            ivars(Ivars, VarList);
        true ->
            ValueTree = hd(Tmp2),   %any_exprのtree
            Tmp3 = ValueTree#tree.children,     %const(tuple) or varref(list)
            case is_list(Tmp3) of
                false ->       %const
                    NewVarList = [{NameTree#tree.children, Tmp3#tree.children}|VarList],
                    ivars(Ivars, NewVarList);
                true ->     %varref
                    NewVarList = [{NameTree#tree.children, varref(Tmp3)}|VarList],
                    ivars(Ivars, NewVarList)
            end
    end.

writeAddVarList([], FPid) ->
    write({self(), {nl, ","}}, FPid);
writeAddVarList([Var|Vars], FPid) ->
    write({self(), {append, "++ ["}}, FPid),
    write({self(), {append, Var}}, FPid),
    write({self(), {append, "]"}}, FPid),
    writeAddVarList(Vars, FPid).

varref(VarrefTreeList) ->   %[VarrefTree]
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
        '<=' ->
            '=<';
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


guardvaluelistWrite([], _ ,_) ->
    fin;
guardvaluelistWrite([Value|Values], VarList, FPid) ->
    case lists:any(fun(X) -> atom_to_list(X) == Value end, VarList) of
        true ->
            write({self(), {append, "Tmp"}}, FPid),
            write({self(), {append, Value}}, FPid),
            write({self(), {append, " "}}, FPid),
            guardvaluelistWrite(Values, VarList, FPid);
        false ->
            write({self(), {append, Value}}, FPid),
            write({self(), {append, " "}}, FPid),
            guardvaluelistWrite(Values, VarList, FPid)
    end.

startfun(FMPid, FPid, Source) ->
    FMPid ! {self(), {deffun, Source}},
    receive
        {FMPid, Funname} ->
            % io:format("~p~n", [Funname]),
            write({self(), {append, f}}, FPid),
            write({self(), {append, Funname}}, FPid),
            % write({self(), {nl, "(VarList, ChanPidList) ->"}}, FPid)
            write({self(), {nl, "(VarList, ChanPidList, GPid) ->"}}, FPid)
    end.

endfun(FMPid, FPid, Target, VarListChangeFlag) ->
    case Target of
        exit ->
            write({self(), {append, "   f"}}, FPid),
            write({self(), {append, exit}}, FPid),
            write({self(), {append, "(GPid)"}}, FPid);
        _ ->
            FMPid ! {self(), {deffun, Target}},
            receive
                {FMPid, Funname} ->
                    write({self(), {append, "   "}}, FPid),
                    write({self(), {append, f}}, FPid),
                    write({self(), {append, Funname}}, FPid),
                    case VarListChangeFlag of
                        nochange ->
                            write({self(), {append, "(VarList, ChanPidList, GPid)"}}, FPid);
                        changed ->
                            write({self(), {append, "(NewVarList, ChanPidList, GPid)"}}, FPid)
                        % nochange ->
                        %     write({self(), {append, "(VarList, ChanPidList)"}}, FPid);
                        % changed ->
                        %     write({self(), {append, "(NewVarList, ChanPidList)"}}, FPid)
                    end
            end
    end.

genexit(FPid) ->
    write({self(), {append, "f"}}, FPid),
    write({self(), {append, exit}}, FPid),
    write({self(), {nl, "(GPid) ->"}}, FPid),
    % write({self(), {nl, "() ->"}}, FPid),
    write({self(), {nl, "   GPid ! {self(), fin},"}}, FPid),
    write({self(), {append, "   fin."}}, FPid).

moduleSetup(ModuleName, ChanList, GlobalVarList, FPid) ->
    write({self(), {append, "-module("}}, FPid),
    write({self(), {append, ModuleName}}, FPid),
    write({self(), {nl, ")."}}, FPid),
    write({self(), {nl, "-export([start/1])."}}, FPid),
    write({self(), {nl, "start(ChanPidList) ->"}}, FPid),
    write({self(), {nl, "   GPid = spawn(fun() -> globalvarmanager:loop() end),"}}, FPid),
    writeInitGlobalVar(GlobalVarList, FPid),
    write({self(), {nl, "   receive"}}, FPid),
    write({self(), {nl, "      start ->"}}, FPid),
    write({self(), {nl, "           f0([], ChanPidList, GPid)"}}, FPid),
% write({self(), {nl, "              f0([], ChanPidList)"}}, FPid),
    write({self(), {nl, "   end."}}, FPid).
    % write({self(), {nl, "   GPid = spawn(fun() -> globalvarmanager:loop() end),"}}, FPid),
    % write({self(), {nl, "   f0(GPid)."}}, FPid).
    % write({self(), {nl, "   f0([], ChanPidList)."}}, FPid).
writeInitGlobalVar([], _) ->
    fin;
writeInitGlobalVar([GlobalVar|GlobalVars], FPid) ->
    {Varname, Value} = GlobalVar,
    write({self(), {append, "   GPid ! {self(), {reg, "}}, FPid),
    write({self(), {append, Varname}}, FPid),
    write({self(), {append, ", "}}, FPid),
    write({self(), {append, Value}}, FPid),
    write({self(), {nl, "}},"}}, FPid),
    write({self(), {nl, "   receive"}}, FPid),
    write({self(), {nl, "       {GPid, fin} ->"}}, FPid),
    write({self(), {nl, "           skip"}}, FPid),
    write({self(), {nl, "   end,"}}, FPid),
    writeInitGlobalVar(GlobalVars, FPid).

declVar([], _) ->
    fin;
declVar([Var|Vars], FPid) ->
    write({self(), {append, "   {_, "}}, FPid),
    TmpVar = atom_to_list(Var),
    write({self(), {append, TmpVar}}, FPid),
    write({self(), {append, "} = hd(lists:filter(fun(X) -> {Tmpname, _} = X, Tmpname == "}}, FPid),
    write({self(), {append, Var}}, FPid),
    write({self(), {nl, " end, VarList)),"}}, FPid),
    declVar(Vars, FPid).

declGuardVar([], _, _) ->
    fin;
declGuardVar([Var|Vars], GlobalVarList, FPid) ->
    case lists:any(fun({First, _}) -> First == Var end, GlobalVarList) of
        false ->
            write({self(), {append, "   {_, Tmp"}}, FPid),
            TmpVar = atom_to_list(Var),
            write({self(), {append, TmpVar}}, FPid),
            write({self(), {append, "} = hd(lists:filter(fun(X) -> {Tmpname, _} = X, Tmpname == "}}, FPid),
            write({self(), {append, Var}}, FPid),
            write({self(), {nl, " end, VarList)),"}}, FPid),
            declGuardVar(Vars, GlobalVarList, FPid);
        true ->
            write({self(), {append, "   GPid ! {self(), {get, "}}, FPid),
            write({self(), {append, Var}}, FPid),
            write({self(), {nl, "}},"}}, FPid),
            write({self(), {nl, "   receive"}}, FPid),
            write({self(), {append, "       {GPid, Tmp"}}, FPid),
            GlobalVarName = atom_to_list(Var),
            write({self(), {append, GlobalVarName}}, FPid),
            write({self(), {nl, "} ->"}}, FPid),
            write({self(), {nl, "           skip"}}, FPid),
            write({self(), {nl, "   end,"}}, FPid),
            declGuardVar(Vars, GlobalVarList, FPid)
    end.

writeListOperationForList(VarName, FPid) ->
    write({self(), {append, "   TmpVarList = lists:filter(fun(X) -> {TmpVarname, _} = X, TmpVarname /= "}}, FPid),
    write({self(), {append, VarName}}, FPid),
    write({self(), {nl, " end, VarList),"}}, FPid),
    write({self(), {append, "   NewVarList = [{"}}, FPid),
    write({self(), {append, VarName}}, FPid),
    write({self(), {append, ","}}, FPid),
    fin.

writeGetGlobalVar([], _) ->
    fin;
writeGetGlobalVar([GlobalVar|GlobalVars], FPid) ->
    write({self(), {append, "   GPid ! {self(), {get, "}}, FPid),
    write({self(), {append, GlobalVar}}, FPid),
    write({self(), {nl, "}},"}}, FPid),
    write({self(), {nl, "   receive"}}, FPid),
    write({self(), {append, "       {GPid, "}}, FPid),
    GlobalVarName = atom_to_list(GlobalVar),
    write({self(), {append, GlobalVarName}}, FPid),
    write({self(), {nl, "} ->"}}, FPid),
    write({self(), {nl, "           skip"}}, FPid),
    write({self(), {nl, "   end,"}}, FPid),
    writeGetGlobalVar(GlobalVars, FPid).

% writeRegGlobalVar(GlobalVar, FPid) ->
%     write({self(), {append, }}, FPid)

send_args(SendArgs, MtypeList, GlobalVarList) ->
    Value = SendArgs#tree.value,
    SendArgsChild = SendArgs#tree.children,
    case Value of
        send_args1 ->
            {ValueList, TmpVarList} = any_expr(SendArgsChild,[]),
            TmpVarList2 = lists:filter(fun(X) -> (MtypeList == []) or not(lists:any(fun(Y) -> Y == X end, MtypeList)) end, TmpVarList),
            VarList = lists:usort(TmpVarList2),
            % io:format("----------------VarList:~p~n", [VarList]),
            % LocalVarList = lists:filter(fun(X) -> not(lists:member(X, GlobalVarList)) end, VarList),
            LocalVarList = lists:filter(fun(X) -> not(lists:any(fun({First, _}) -> First == X end, GlobalVarList)) end, VarList),
            % UseGlobalVarList = lists:filter(fun(X) -> lists:member(X, GlobalVarList) end, VarList),
            UseGlobalVarList = lists:filter(fun(X) -> lists:any(fun({First, _}) -> First == X end, GlobalVarList) end, VarList),
            % io:format("UseGlobalVarList:~p~n", [UseGlobalVarList]),
            NewValueList = lists:map(fun(X) -> case is_atom(X) of true -> atom_to_list(X); false -> X end end, ValueList),
            {NewValueList, LocalVarList, UseGlobalVarList};
        send_args2 ->
            %tba
            {[], []}
    end.

recv_args(RecvArg) ->    %recv_arg
    RecvArgValue = RecvArg#tree.value,
    RecvArgChild = RecvArg#tree.children,
    case RecvArgValue of
        recv_arg1 ->
            % Tmp = hd(RecvArgChild),
            % Varname = atom_to_list(varref(RecvArgChild)),
            Varname = varref(RecvArgChild),
            {var, Varname};
        recv_arg3 ->
            ConstTree = lists:nth(2, RecvArgChild),
            io:format("====:~n~p~n", [ConstTree#tree.children]),
            {const, ConstTree#tree.children}
    end.
