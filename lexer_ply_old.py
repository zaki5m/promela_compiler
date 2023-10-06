import ply.lex as lex
import ply.yacc as yacc
import queue

############################
# lexer
############################
reserved = {
    "proctype":"PROCTYPE",
    "init":"INIT",
    "never":"NEVER",
    "trace":"TRACE",
    "typedef":"TYPEDEF",
    "mtype":"MTYPE",
    # "unsigned":"UNSIGNED", not use yet
    "bit":"BIT",
    "bool":"BOOL",
    "byte":"BYTE",
    "short":"SHORT",
    "int":"INT",
    "chan":"CHAN",
    "active":"ACTIVE",
    "priority":"PRIORITY",
    "provided":"PROVIDED",
    "hidden":"HIDDEN",
    "show":"SHOW",
    "unless":"UNLESS",
    "xr":"XR",
    "xs":"XS",
    "of":"OF",
    "eval":"EVAL",
    "if":"IF",
    "fi":"FI",
    "do":"DO",
    "od":"OD",
    "for":"FOR",
    "atomic":"ATOMIC",
    "d_step":"D_STEP",
    "select":"SELECT",
    "else":"ELSE",
    "break":"BREAK",
    "goto":"GOTO",
    "printf":"PRINTF",
    "printm":"PRINTM",
    "assert":"ASSERT",
    "in":"IN",
    "len":"LEN",
    "timeout":"TIMEOUT",
    "np_":"NP_",
    "enabled":"ENABLED",
    "pc_value":"PC_VALUE",
    "run":"RUN",
    "full":"FULL",
    "empty":"EMPTY",
    "nfull":"NFULL",
    "nempty":"NEMPTY",
    "true":"TRUE",
    "false":"FALSE",
    "skip":"SKIP",
    "get_priority": "GET_PRIORITY",
    "set_priority": "SET_PRIORITY"
}


# List of token names
tokens = [
    "ARROW",
    "NUMBER",
    "LPAREN", #(
    "RPAREN", #)
    "LBRACKET", #[
    "RBRACKET", #]
    "LBRACE", #{
    "RBRACE", #}
    "SEMI", #;
    "COLON", #:
    "COLONS", #::
    "EQUAL", #=
    "COMMA", #,
    "PERIOD",
    "PERIODS",
    "EOF",
    "NAME",
    'PLUS',
    'INCR',
    'MINUS',
    'DECR',
    'TIMES',
    'DIVIDE',
    'MOD',
    'AND',
    'XOR',
    'OR',
    'GT',
    'LT',
    'GE',
    'LE',
    'EQ',
    'NE',
    'LSHIFT',
    'RSHIFT',
    'LAND',
    'LOR',
    'RCV',
    'R_RCV',
    'LNOT',
    'TX2',
    'TILDE',
    'AT',
    'DQUO',
    "COMMENT",
    "STRING"
]

tokens += list(reserved.values())

t_NUMBER = r'[0-9]+[.]?[0-9]*'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMI = r';'
t_COLON = r':'
t_COLONS = r'\::'
t_EQUAL = r'='
t_ARROW = r'->'
t_PLUS = r'\+'
t_INCR = r'\+\+'
t_MINUS = r'-'
t_DECR = r'--'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_MOD = r'%'
t_AND = r'&'
t_XOR = r'\^'
t_OR = r'\|'
t_GT = r'>'
t_LT = r'<'
t_GE = r'>='
t_LE = r'<='
t_EQ = r'=='
t_NE = r'!='
t_LSHIFT = r'<<'
t_RSHIFT = r'>>'
t_LAND = r'&&'
t_LOR = r'\|\|'
t_RCV = r'\?'
t_R_RCV = r'\?\?'
t_LNOT = r'!'
t_TX2 = r'!!'
t_COMMA = r','
t_PERIOD = r'\.'
t_PERIODS = r'\.\.'
t_TILDE = r'~'
t_AT = r'@'
t_DQUO = r'"'
t_ignore = ' \t'

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    #t.typeはdefaultではt_以下の文字列に設定されている
    t.type = reserved.get(t.value, "NAME") #reservedに登録されているかのチェック登録されていなければ第二引数が代入される
    return t

def t_STRING(t):
    r'"([^"]|\")*"'
    #t.typeはdefaultではt_以下の文字列に設定されている
    t.type = reserved.get(t.value, "STRING") #reservedに登録されているかのチェック登録されていなければ第二引数が代入される
    return t

# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_COMMENT(t):
    r'/\*(.|\n)*?\*/|//.*'
    pass

# Compute column.
#     input is the input text string
#     token is a token instance
# def find_column(input, token):
#     line_start = input.rfind('\n', 0, token.lexpos) + 1
#     return (token.lexpos - line_start) + 1

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# EOF handling rule
# def t_eof(t):
#     # Get more input (Example)
#     more = input('... ')
#     if more:
#         t.self.lexer.input(more)
#         return t.self.lexer.token()
#     return None


############################
# parser
############################

def print_indent(tab_num):
    while tab_num > 0:
        print("   ", end="")
        tab_num -= 1

def nesttuple_putqueue(p):
    last_elem = len(p) - 1
    if type(p[last_elem]) is tuple:
        q.put(p[:-1], block = False)
        nesttuple_putqueue(p[last_elem])
    else:
        q.put(p, block=False)

# def nesttuple_putqueue(p):
#     i = 0
#     tupleindex_lst = []
#     while i > len(p):
#         if type(p[i]) is tuple:
#             i.append(tupleindex_lst)
#         else:
#             pass

def set_p(p):
    # if type(p[1]) == tuple:
    #     t = p[1]
    # else:
    #     t = (p[1],)
    # t = (p[1],)
    # i = 2
    # while i < len(p):
    #     t = t + (p[i],)
    #     i += 1


    t = []
    i = 1
    while i < len(p):
        if type(p[i]) == str:
            print("aaaa")
            t1 = (p[i],)
            t.append(t1)
        else:
            print("bbbb")
            t.append(p[i])
        i += 1
    return t


def print_p(p):
    i = 1
    while i < len(p):
        if type(p[i]) is str:
            print(p[i])
        elif type(p[i]) is tuple:
            tab_num = 0
            nesttuple_putqueue(p[i])
            while not q.empty():
                tmp = q.get(block=False)
                print_indent(tab_num)
                print(tmp)
                tab_num += 1
        i += 1

def printany(aaa):
    print(aaa)

precedence = (
    ("right", "EQUAL"),
    ("right", "ARROW", "SEMI"),
    ("left", "LOR"),
    ("left", "LAND"),
    ("left", "OR"),
    ("left", "XOR"),
    ("left", "AND"),
    ("left", "EQ", "NE"),
    ("left", "LT", "GT", "GE", "LE"),
    ("left", "LSHIFT", "RSHIFT"),
    ("left", "PLUS", "MINUS"),
    ("left", "TIMES", "DIVIDE", "MOD"),
    ("right", "INCR", "DECR"),
    ("right", "TILDE"),
    ("right", "LNOT")
)

start = "spec"

q = queue.Queue()

def p_spec(p):
    """spec : module
            | module  spec"""
    if len(p) == 2:
        p[0] = ("spec", p[1])
    else:
        p[0] = ("spec", set_p(p))

    # print_p(p)

def p_module(p):
    """module   : proctype
                | mtype
                | init
                | never
                | trace
                | utype
                | decl_lst
    """
    p[0] = ("module", p[1])

def p_proctype(p):
    """proctype : PROCTYPE name LPAREN RPAREN LBRACE sequence RBRACE
                | active PROCTYPE name LPAREN RPAREN LBRACE sequence RBRACE
                | PROCTYPE name LPAREN decl_lst RPAREN LBRACE sequence RBRACE
                | PROCTYPE name LPAREN RPAREN priority LBRACE sequence RBRACE
                | PROCTYPE name LPAREN RPAREN enabler LBRACE sequence RBRACE
                | active PROCTYPE name LPAREN decl_lst RPAREN LBRACE sequence RBRACE
                | active PROCTYPE name LPAREN RPAREN priority LBRACE sequence RBRACE
                | active PROCTYPE name LPAREN RPAREN enabler LBRACE sequence RBRACE
                | PROCTYPE name LPAREN RPAREN priority enabler LBRACE sequence RBRACE
                | PROCTYPE name LPAREN decl_lst RPAREN priority LBRACE sequence RBRACE
                | PROCTYPE name LPAREN decl_lst RPAREN enabler LBRACE sequence RBRACE
                | active PROCTYPE name LPAREN decl_lst RPAREN priority LBRACE sequence RBRACE
                | active PROCTYPE name LPAREN decl_lst RPAREN enabler LBRACE sequence RBRACE
                | active PROCTYPE name LPAREN RPAREN priority enabler LBRACE sequence RBRACE
                | PROCTYPE name LPAREN decl_lst RPAREN priority enabler LBRACE sequence RBRACE
                | active PROCTYPE name LPAREN decl_lst RPAREN priority enabler LBRACE sequence RBRACE"""
    # p[0] = "proctype"
    # print_p(p)
    p[0] = ("proctype", set_p(p))


def p_init(p):
    """init : INIT LBRACE sequence RBRACE
            | INIT priority LBRACE sequence RBRACE"""
    # p[0] = "init"
    # print_p(p)
    p[0] = ("init", set_p(p))

def p_never(p):
    "never    : NEVER LBRACE sequence RBRACE"
    # p[0] = "never"
    # print_p(p)
    p[0] = ("never", set_p(p))

def p_trace(p):
    "trace    : TRACE LBRACE sequence RBRACE"
    # p[0] = "trace"
    # print_p(p)
    p[0] = ("trace", set_p(p))

def p_utype(p):
    "utype    : TYPEDEF name LBRACE decl_lst RBRACE "
    # p[0] = "utype"
    # print_p(p)
    p[0] = ("utype", set_p(p))

def p_mtype(p):
    """mtype    : MTYPE LBRACE names RBRACE
                | MTYPE EQUAL LBRACE names RBRACE"""
    # p[0] = "mtype"
    # print_p(p)
    p[0] = ("mtype", set_p(p))

# def p_decl_lst(p):
#     """decl_lst     : one_decl
#                     | one_decl SEMI decl_lst"""
#     if len(p) == 2:
#         p[0] = "decl_lst"
#     else:
#         p[0] = "decl_lst"
#     print_p(p)

def p_decl_lst(p):
    """decl_lst     : one_decl
                    | one_decl one_decls"""
    if len(p) == 2:
        p[0] = "decl_lst", p[1]
    else:
        p[0] = ("decl_lst", set_p(p))
    # print_p(p)

def p_one_decls(p):
    """one_decls    : SEMI one_decl
                    | SEMI one_decl one_decls"""
    # if len(p) == 3:
    #     p[0] = "one_decls", p[2]
    # else:
    #     p[0] = "one_decls", p[2], p[3]
    p[0] = ("one_decls", set_p(p))

def p_one_decl(p):
    """one_decl : typename ivar
                | visible typename ivar
                | typename ivar ivars
                | visible typename ivar ivars"""
                # | visible unsigned_decl
                # | unsigned_decl
    # if len(p) == 2:
    #     p[0] = "one_decl", p[1]

    # if len(p) == 3:
    #     p[0] = "one_decl", p[1], p[2]
    # elif len(p) == 4:
    #     p[0] = "one_decl", p[1], p[2], p[3]
    # else:
    #     p[0] = "one_decl", p[1], p[2], p[3], p[4]
    p[0] = ("one_decl", set_p(p))

def p_ivars(p):
    """ivars    : COMMA ivar
                | COMMA ivar ivars"""
    # if len(p) == 3:
    #     p[0] = "ivars", p[2]
    # else:
    #     p[0] = "ivars", p[2], p[3]
    p[0] = ("ivars", set_p(p))

def p_typename(p):
    """typename : BIT
                | BOOL
                | BYTE
                | SHORT
                | INT
                | MTYPE
                | CHAN
                | uname"""
    if type(p[1]) == str:
        p[0] = "typename", (p[1],)
    else:
        p[0] = "typename", p[1]

def p_active(p):
    """active   : ACTIVE
                | ACTIVE LBRACKET const RBRACKET"""
    if len(p) == 2:
        p[0] = "active", (p[1],)
    # elif len(p) == 5:
    #     p[0] = "active", p[3]
    else:
        p[0] = ("active", set_p(p))

def p_priority(p):
    "priority : PRIORITY const "
    # p[0] = "priotity", p[2]
    p[0] = ("priority", set_p(p))

def p_enabler(p):
    "enabler : PROVIDED LPAREN expr RPAREN"
    # p[0] = "enabler", p[1], p[3]
    p[0] = ("enabler", set_p(p))

def p_visible(p):
    """visible  : HIDDEN
                | SHOW"""
    p[0] = "visible", (p[1],)

# def p_sequence(p):
#     """sequence     : step
#                     | step SEMI sequence
#                     | step ARROW sequence"""
#     if len(p) == 2:
#         p[0] = "sequence", p[1]
#     elif len(p) == 4:
#         p[0] = "sequence", p[1], p[3]

def p_sequence(p):
    """sequence     : step
                    | step steps"""
    if len(p) == 2:
        p[0] = "sequence", p[1]
    # elif len(p) == 3:
    #     p[0] = "sequence", p[1], p[2]
    else:
        p[0] = ("sequence", set_p(p))

def p_steps(p):
    """steps    : SEMI step
                | SEMI step steps
                | ARROW step
                | ARROW step steps"""
    # if len(p) == 3:
    #     p[0] = "steps", p[2]
    # else:
    #     p[0] = "steps", p[2], p[3]
    p[0] = ("steps", set_p(p))

def p_step(p):
    """step : stmnt
            | one_decl
            | XR varref
            | XR varref varrefs
            | XS varref
            | XS varref varrefs"""
    if len(p) == 2:
        p[0] = "step", p[1]
    # elif len(p) == 3:
    #     p[0] = "step", p[1], p[2]
    else:
    #     p[0] = "step", p[1], p[2], p[3]
        p[0] = ("step", set_p(p))

def p_varrefs(p):
    """varrefs  : COMMA varref
                | COMMA varref varrefs"""
    # if len(p) == 3:
    #     p[0] = "varrefs", p[2]
    # else:
    #     p[0] = "varrefs", p[2], p[3]
    p[0] = ("varrefs", set_p(p))

def p_ivar(p):
    """ivar : name
            | name LBRACKET const RBRACKET
            | name EQUAL any_expr
            | name EQUAL ch_init
            | name LBRACKET const RBRACKET EQUAL any_expr
            | name LBRACKET const RBRACKET EQUAL ch_init"""
    if len(p) == 2:
        p[0] = "ivar", p[1]
    # elif len(p) == 4:
    #     p[0] = "ivar", p[1], p[3]
    # elif len(p) == 5:
    #     p[0] = "ivar", p[1], p[3]
    # elif len(p) == 7:
    #     p[0] = "ivar", p[1], p[3], p[6]
    else:
        p[0] = ("ivar", set_p(p))

def p_ch_init(p):
    """ch_init  : LBRACKET const RBRACKET OF LBRACE typename RBRACE
                | LBRACKET const RBRACKET OF LBRACE typename typenames RBRACE"""
    # if len(p) == 8:
    #     p[0] = "ch_init", p[2], p[6]
    # else:
    #     p[0] = "ch_init", p[2], p[6], p[7]
    p[0] = ("ch_init", set_p(p))

def p_typenames(p):
    """typenames    : COMMA typename
                    | COMMA typename typenames"""
    # if len(p) == 3:
    #     p[0] = "typenames", p[2]
    # else:
    #     p[0] = "typenames", p[2], p[3]
    p[0] = ("typenames", set_p(p))

def p_varref(p):
    """varref   : name
                | name LBRACKET any_expr RBRACKET
                | name PERIOD varref
                | name LBRACKET any_expr RBRACKET PERIOD varref"""
    if len(p) == 2:
        p[0] = "varref", p[1]
    # elif len(p) == 7:
    #     p[0] = "varref", p[1], p[3], p[6]
    else:
    #     p[0] = "varref", p[1], p[3]
        p[0] = ("varref", set_p(p))

def p_send(p):
    """send : varref LNOT send_args
            | varref TX2 send_args"""
    # p[0] = "send", p[1], p[3]
    p[0] = ("send", set_p(p))

def p_receive(p):
    """receive  : varref RCV recv_args
                | varref R_RCV recv_args
                | varref RCV LT recv_args GT
                | varref R_RCV LT recv_args GT"""
    # if len(p) == 4:
    #     p[0] = "receive", p[1], p[3]
    # else:
    #     p[0] = "receive", p[1], p[4]
    p[0] = ("receive", set_p(p))

def p_poll(p):
    """poll : varref RCV LBRACKET recv_args RBRACKET
            | varref R_RCV LBRACKET recv_args RBRACKET"""
    # p[0] = "poll", p[1], p[4]
    p[0] = ("poll", set_p(p))

def p_send_args(p):
    """send_args    : arg_lst
                    | any_expr LPAREN arg_lst RPAREN"""
    if len(p) == 2:
        p[0] = "send_args", p[1]
    else:
    #     p[0] = "send_args", p[1], p[3]
        p[0] = ("send_args", set_p(p))

def p_arg_lst(p):
    """arg_lst  : any_expr
                | any_expr any_exprs"""
    if len(p) == 2:
        p[0] = "arg_lst", p[1]
    else:
    #     p[0] = "arg_lst", p[1], p[2]
        p[0] = ("arg_lst", set_p(p))

def p_anyexprs(p):
    """any_exprs    : COMMA any_expr
                    | COMMA any_expr any_exprs"""
    # if len(p) == 3:
    #     p[0] = "any_exprs", p[2]
    # else:
    #     p[0] = "any_exprs", p[2], p[3]
    p[0] = ("any_exprs", set_p(p))

def p_recv_args(p):
    """recv_args    : recv_arg
                    | recv_arg recv_argss
                    | recv_arg LPAREN recv_args RPAREN"""
    if len(p) == 2:
        p[0] = "recv_args", p[1]
    # elif len(p) == 3:
    #     p[0] = "recv_args", p[1], p[2]
    else:
    #     p[0] = "recv_args", p[1], p[3]
        p[0] = ("recv_args", set_p(p))

def p_recv_argss(p):
    """recv_argss   : COMMA recv_arg
                    | COMMA recv_arg recv_argss"""
    # if len(p) == 3:
    #     p[0] = "recv_argss", p[2]
    # else:
    #     p[0] = "recv_argss", p[2], p[3]
    p[0] = ("recv_argss", set_p(p))

def p_recv_arg(p):
    """recv_arg : varref
                | EVAL LPAREN varref RPAREN
                | const
                | MINUS const"""
    if len(p) == 2:
        p[0] = "recv_arg", p[1]
    # elif len(p) == 3:
    #     p[0] = "recv_arg", p[2]
    else:
    #     p[0] = "recv_arg", p[3]
        p[0] = ("recv_arg", set_p(p))

def p_assign(p):
    """assign   : varref EQUAL any_expr
                | varref INCR
                | varref DECR"""
    # if len(p) == 3:
    #     p[0] = "assign", p[1], p[2]
    # else:
    #     p[0] = "assign", p[1], p[3]
    p[0] = ("assign", set_p(p))

def p_stmnt(p):
    """stmnt    : IF options FI
                | DO options OD
                | FOR LPAREN range RPAREN LBRACE sequence RBRACE
                | ATOMIC LBRACE sequence RBRACE
                | D_STEP LBRACE sequence RBRACE
                | SELECT LPAREN range RPAREN
                | LBRACE sequence RBRACE
                | send
                | receive
                | assign
                | ELSE
                | BREAK
                | GOTO name
                | name COLON stmnt
                | PRINTM LPAREN name RPAREN
                | PRINTF LPAREN STRING RPAREN
                | PRINTF LPAREN STRING COMMA arg_lst RPAREN
                | ASSERT expr
                | expr"""
    if len(p) == 2:
        if type(p[1]) == str:
            p[0] = "stmnt", (p[1],)
        else:
            p[0] = "stmnt", p[1]
    # elif len(p) == 3:
    #     p[0] = "stmnt", p[1], p[2]
    # elif len(p) == 4:
    #     if p[1] == "if" or "do":
    #         p[0] = "stmnt", p[1], p[2]
    #     elif p[1] == "(":
    #         p[0] = "stmnt", p[2]
    #     else:
    #         p[0] = "stmnt", p[1], p[3]
    # elif len(p)== 5:
    #     p[0] = "stmnt", p[1], p[3]
    # elif len(p) == 6:
    #     p[0] = "stmnt", p[1], p[3], p[4]
    # elif len(p) == 7:
    #     p[0] = "stmnt", p[1], p[3], p[5]
    # elif len(p) == 8:
    #     p[0] = "stmnt", p[1], p[3], p[6]
    else:
        p[0] = ("stmnt", set_p(p))

def p_range(p):
    """range    : name COLON any_expr PERIODS any_expr
                | name IN name"""
    # if len(p) == 4:
    #     p[0] = "range", p[1], p[3]
    # else:
    #     p[0] = "range", p[1], p[3], p[5]
    p[0] = ("range", set_p(p))

def p_options(p):
    """options  : COLONS sequence
                | COLONS sequence options"""
    # if len(p) == 3:
    #     p[0] = "options", p[2]
    # elif len(p) == 4:
    #     p[0] = "options", p[2], p[3]
    p[0] = ("options", set_p(p))

def p_andor(p):
    """andor    : LAND
                | LOR"""
    p[0] = "andor", (p[1],)

def p_binarop(p):
    """binarop  : PLUS
                | MINUS
                | TIMES
                | DIVIDE
                | MOD
                | AND
                | XOR
                | OR
                | GT
                | LT
                | GE
                | LE
                | EQ
                | NE
                | LSHIFT
                | RSHIFT
                | andor"""

    if type(p[1]) == str:
        p[0] = "binarop", (p[1],)
    else:
        p[0] = "binarop", p[1]

def p_unarop(p):
    """unarop   : TILDE
                | MINUS
                | LNOT"""
    p[0] = "unarop", (p[1],)

def p_any_expr(p):
    """any_expr : LPAREN any_expr RPAREN
                | any_expr binarop any_expr
                | unarop any_expr
                | LPAREN any_expr ARROW any_expr COLON any_expr RPAREN
                | LEN LPAREN varref RPAREN
                | poll
                | varref
                | const
                | TIMEOUT
                | NP_
                | ENABLED LPAREN any_expr RPAREN
                | PC_VALUE LPAREN any_expr RPAREN
                | name LBRACKET any_expr RBRACKET AT name
                | RUN name LPAREN RPAREN
                | RUN name LPAREN arg_lst RPAREN
                | RUN name LPAREN RPAREN priority
                | RUN name LPAREN arg_lst RPAREN priority
                | GET_PRIORITY LPAREN expr RPAREN
                | SET_PRIORITY LPAREN expr COMMA expr RPAREN"""
    if len(p) == 2:
        if type(p[1]) == str:
            p[0] = "any_expr", (p[1],)
        else:
            p[0] = "any_expr", p[1]
    # elif len(p) == 3:
    #     p[0] = "any_expr", p[1], p[2]
    # elif len(p) == 4:
    #     if p[1] == "(":
    #         p[0] = "anyexpr", p[2]
    #     elif p[2][0] == "binarop":
    #         p[0] = "any_expr", p[1], p[2], p[3]
    # elif len(p) == 5:
    #     if p[1] == "run":
    #         p[0] = "any_expr", p[1], p[2]
    #     elif p[1] == "get_priority":
    #         p[0] = "get_priority", p[1], p[3]
    #     else:
    #         p[0] = "any_expr", p[1], p[3]
    # elif len(p) == 6:
    #     if p[4] == ")":
    #         p[0] = "any_expr", p[1], p[2], p[5]
    #     else:
    #         p[0] = "any_expr", p[1], p[2], p[4]
    else:
    #     if p[1] == "run":
    #         p[0] = "any_expr", p[1], p[2], p[4], p[6]
    #     elif p[1] == "get_priority":
    #         p[0] = "any_expr", p[1], p[3], p[5]
    #     else:
    #         p[0] = "any_expr", p[1], p[3], p[6]
        p[0] = ("any_expr", set_p(p))

def p_expr(p):
    """expr : any_expr
            | chanpoll LPAREN varref RPAREN"""
            # | LPAREN expr RPAREN
            # | expr andor expr
    if len(p) == 2:
        p[0] = "expr", p[1]
    # # elif len(p) == 4:
    # #     if p[1] == "(":
    # #         p[0] = "expr2", p[2]
    # #     else:
    # #         p[0] = "expr3", p[1], p[2], p[3]
    else:
    #     p[0] = "expr4", p[1], p[3]
        p[0] = ("expr", set_p(p))

def p_chanpoll(p):
    """chanpoll : FULL
                | EMPTY
                | NFULL
                | NEMPTY"""
    p[0] = "chanpoll", (p[1],)

# def p_string(p):
#     """string   : DQUO any_ascii_char DQUO
#                 | DQUO any_ascii_char any_ascii_chars DQUO"""

# def p_any_ascii_chars(p):
#     """any_ascii_chars  : any_ascii_char
#                         | any_ascii_char any_ascii_chars"""

def p_uname(p):
    "uname : name"
    p[0] = "uname", p[1]

def p_name(p):
    "name : NAME"
    p[0] = "name", (p[1],)

def p_names(p):
    """names    : name
                | name COMMA names"""
    if len(p) == 2:
        p[0] = "name", p[1]
    # elif len(p) == 4:
    #     p[0] = p[1], p[3]
    else:
        p[0] = ("names", set_p(p))

def p_const(p):
    """const    : TRUE
                | FALSE
                | SKIP
                | NUMBER"""
    p[0] = "const", (p[1],)

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")
# def p_error(p):
#     print("Whoa. You are seriously hosed.")
#     if not p:
#         print("End of File!")
#         return

#     # Read ahead looking for a closing '}'
#     while True:
#         tok = parser.token()             # Get the next token
#         print (tok)
#         if not tok or tok.type == 'RBRACE':
#             break
#     parser.restart()

lexer = lex.lex()   #build
# lexer = lex.lex(debug = 1)  #debugg


# ############# lex_test
def lex_test():
    test_file = input()
    f = open(test_file, 'r')
    data = f.read()
    f.close()

    lexer.input(data)

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
# #############




parser = yacc.yacc()

# while True:
#     try:
#         s = input('calc > ')
#     except EOFError:
#         break
#     if not s: continue
#     result = parser.parse(s)
#     print(result)

# Build the parser
# parser = yacc.yacc()

def yacc_test():
    test_file = input()
    f = open(test_file, 'r')
    # f = open("test2.pml", 'r')
    data = f.read()
    f.close()

    # parser = yacc.yacc()
    result = parser.parse(data)
    print('result: ', result)
    result2 = f"{result}"
    wresultfile = open("result_analyze.txt", "w")
    wresultfile.write(result2)
    wresultfile.close


if __name__ == '__main__':
    print("lex test ? or yacc test?")
    lory = input()
    if lory == "l":
        lex_test()
    elif lory == "y":
        yacc_test()