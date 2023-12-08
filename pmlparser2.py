import ply.lex as lex
import ply.yacc as yacc
import queue
import sys

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


def set_p(p):
    t = []
    i = 1
    while i < len(p):
        if type(p[i]) == str:
            # print("aaaa")
            t1 = (p[i],)
            t.append(t1)
        else:
            # print("bbbb")
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
    """spec : module  modules"""
    p[0] = ("spec", p[1] + p[2])

def p_modules(p):
    """modules  :
                | module modules"""
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[1] + p[2]

def p_module(p):
    """module   : proctype
                | mtype
                | init
                | never
                | trace
                | utype
                | decl_lst
    """
    p[0] = [("module", p[1])]

def p_proctype(p):
    """proctype : activeO PROCTYPE name LPAREN decl_lstO RPAREN priorityO enablerO LBRACE sequence RBRACE"""
    # p[0] = ("proctype", set_p(p))
    p[0] = ("proctype", [p[1], p[3], p[5], p[7], p[8], p[10]])

def p_activeO(p):
    """activeO  :
                | active"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[1]

def p_decl_lstO(p):
    """decl_lstO :
                | decl_lst"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[1]
def p_priorityO(p):
    """priorityO    :
                    | priority"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[1]

def p_enablerO(p):
    """enablerO :
                | enabler"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[1]


def p_init(p):
    """init : INIT priorityO LBRACE sequence RBRACE"""
    p[0] = ("init", [p[2], p[4]])

def p_never(p):
    "never    : NEVER LBRACE sequence RBRACE"
    p[0] = ("never",  p[3])

def p_trace(p):
    "trace    : TRACE LBRACE sequence RBRACE"
    p[0] = ("trace",p[3])

def p_utype(p):
    "utype    : TYPEDEF name LBRACE decl_lst RBRACE "
    p[0] = ("utype", [(p[1],), p[2], p[4]])

def p_mtype(p):
    """mtype    : MTYPE equalO LBRACE name names RBRACE"""
    p[0] = ("mtype", [p[4] + p[5]])

def p_equalO(p):
    """equalO    :
                | EQUAL"""
    if len(p) == 1:
        p[0] = ("none",)
    if len(p) == 2:
        p[0] = p[1]

def p_decl_lst(p):
    """decl_lst : one_decl one_decls"""
    p[0] = p[1] + p[2]

def p_one_decls(p):
    """one_decls    :
                    | SEMI one_decl one_decls"""
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[2] + p[3]

def p_one_decl(p):
    """one_decl : visibleO typename ivar ivars"""
                # | visible unsigned_decl
                # | unsigned_decl
    p[0] = [("one_decl", [p[1], p[2], p[3] + p[4]])]

def p_visibleO(p):
    """visibleO  :
                | visible"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[1]

def p_ivars(p):
    """ivars    :
                | COMMA ivar ivars"""
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[2] + p[3]

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
        p[0] = [("typename", (p[1],))]
    else:
        p[0] = [("typename", p[1])]

def p_active(p):
    """active   : ACTIVE activeopt"""
    p[0] = ("active", p[2])

def p_activeopt(p):
    """activeopt    :
                    | LBRACKET const RBRACKET"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[2]

def p_priority(p):
    """priority : PRIORITY const """
    p[0] = ("priority", p[2])

def p_enabler(p):
    """enabler  : PROVIDED LPAREN expr RPAREN"""
    p[0] = ("enabler", [(p[1],), p[3]])

def p_visible(p):
    """visible  : HIDDEN
                | SHOW"""
    p[0] = ("visible", (p[1],))

def p_sequence(p):
    """sequence     : step steps"""
    p[0] = p[1] + p[2]

def p_optionseq(p):
    """optionseq    : guard ARROW step steps"""
    p[0] = [("optionseq", [p[1], p[3] + p[4]])]


def p_steps(p):
    """steps    :
                | SEMI step steps
                | ARROW step steps"""
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[2] + p[3]

def p_step(p):
    """step : stmnt stepopt
            | one_decl
            | XR varref varrefs
            | XS varref varrefs"""
    if len(p) == 2:                 # decl_lst
        p[0] = [("step2", p[1])]
    elif len(p) == 3:               # stmnt stepopt
        p[0] = [("step1", [p[1], p[2]])]
    elif len(p) == 4:               # XR/XS varref varrefs
        p[0] = [("step3", [(p[1],), p[2] + p[3]])]

def p_stepopt(p):
    """stepopt  :
                | UNLESS stmnt"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[2]

def p_guard(p):
    """guard    : stmnt4guard guardopt"""
    p[0] = ("guard", set_p(p))

def p_guardopt(p):
    """guardopt :
                | UNLESS stmnt"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[2]

def p_varrefs(p):
    """varrefs  :
                | COMMA varref varrefs"""
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[2] + p[3]

def p_varref(p):
    """varref   : name varrefopt1 varrefopt2"""
    p[0] = [("varref", [p[1], p[2], p[3]])]

def p_varrefopt1(p):
    """varrefopt1   :
                    | LBRACKET any_expr RBRACKET"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = (p[2])
def p_varrefopt2(p):
    """varrefopt2   :
                    | PERIOD varref"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[2]

def p_ivar(p):
    """ivar : name ivaropt1 ivaropt2"""
    p[0] = [("ivar", [p[1], p[2], p[3]])]

def p_ivaropt1(p):
    """ivaropt1  :
                | LBRACKET const RBRACKET"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[2]

def p_ivaropt2(p):
    """ivaropt2 :
                | EQUAL any_expr
                | EQUAL ch_init"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[2]

def p_ch_init(p):
    """ch_init  : LBRACKET const RBRACKET OF LBRACE typename typenames RBRACE"""
    p[0] = ("ch_init", [p[2], p[6] + p[7]])

def p_typenames(p):
    """typenames    :
                    | COMMA typename typenames"""
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[2] + p[3]

def p_send(p):
    """send : varref LNOT send_args
            | varref TX2 send_args"""
    # p[0] = ("send", set_p(p))
    if p[2] == "!":
        p[0] = ("send1", [p[1], p[3]])
    elif p[2] == "!!":
        p[0] = ("send2", [p[1], p[3]])

def p_receive(p):
    """receive  : varref RCV recv_args
                | varref R_RCV recv_args
                | varref RCV LT recv_args GT
                | varref R_RCV LT recv_args GT"""
    # p[0] = ("receive", set_p(p))
    if len(p) == 4:
        if p[2] == "?":
            p[0] = ("receive1", [p[1], p[3]])
        elif p[2] == "??":
            p[0] = ("receive2", [p[1], p[3]])
    elif len(p) == 6:
        if p[2] == "?":
            p[0] = ("receive3", [p[1], p[4]])
        elif p[2] == "??":
            p[0] = ("receive4", [p[1], p[4]])

def p_poll(p):
    """poll : varref RCV LBRACKET recv_args RBRACKET
            | varref R_RCV LBRACKET recv_args RBRACKET"""
    # p[0] = ("poll", [p[1], p[2], p[4]])
    if p[2] == "?":
        p[0] = ("poll", [p[1], p[4]])
    elif p[2] == "??":
        p[0] = ("poll2", [p[1], p[4]])

def p_send_args(p):
    """send_args    : arg_lst
                    | any_expr LPAREN arg_lst RPAREN"""
    if len(p) == 2:
        p[0] = "send_args1", p[1]
    else:
        p[0] = ("send_args2", [p[1], p[3]])

def p_arg_lst(p):
    """arg_lst  : any_expr any_exprs"""
    p[0] = p[1] + p[2]

def p_anyexprs(p):
    """any_exprs    :
                    | COMMA any_expr any_exprs"""
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[2] + p[3]

def p_recv_args(p):
    """recv_args    : recv_arg recv_argss
                    | recv_arg LPAREN recv_args RPAREN"""
    if len(p) == 3:
        p[0] = ("recv_args1",[p[1] + p[2]])
    else:
        p[0] = ("recv_args2", [p[1], p[3]])

def p_recv_argss(p):
    """recv_argss   :
                    | COMMA recv_arg recv_argss"""
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[2] + p[3]

def p_recv_arg(p):
    """recv_arg : varref
                | EVAL LPAREN varref RPAREN
                | minusO const"""
    if len(p) == 2:
        p[0] = [("recv_arg1", p[1])]
    elif len(p) == 3:
        p[0] = [("recv_arg3", [p[1], p[2]])]
    else:
        p[0] = [("recv_arg2", [(p[1],), p[3]])]

def p_minusO(p):
    """minusO   :
                | MINUS"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = (p[1],)

def p_assign(p):
    """assign   : varref EQUAL any_expr
                | varref INCR
                | varref DECR"""
    # if len(p) == 3:
    #     p[0] = "assign", p[1], p[2]
    # else:
    #     p[0] = "assign", p[1], p[3]
    # p[0] = ("assign", set_p(p))
    if p[2] == "=":
        p[0] = ("assign1", [p[1], p[3]])
    elif p[2] == "++":
        p[0] = ("assign2", [p[1]])
    elif p[2] == "--":
        p[0] = ("assign3", [p[1]])

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
                | PRINTF LPAREN STRING stmntopt RPAREN
                | ASSERT expr
                | expr"""
    if len(p) == 2:                     # ELSE or BREAK
        if p[1] == "else":
            p[0] = "stmnt9", (p[1],)
        elif p[1] == "break":
            p[0] = "stmnt10", (p[1],)
        else:
            p[0] = "stmnt8", p[1]               #send or receive or assign or expr
    elif len(p) == 3:    #GOTO ASSERT
        if p[1] == "goto":
            p[0] = ("stmnt11", p[2])
        elif p[1] == "assert":
            p[0] = ("stmnt15", p[2])
    elif len(p) == 4:                       # IF DO ,name COLON stmnt, LBRACE sequence RBRACE
        if p[1] == "if":
            p[0] = ("stmnt1", p[2])
        elif p[1] == "do":
            p[0] = ("stmnt2", p[2])
        elif p[1] == "(":
            p[0] = ("stmnt7", p[2])
        else:
            p[0] = ("stmnt12", [p[1], p[3]])
    elif len(p)== 5:                    # ATOMIC D_STEP SELECT PRINTM
        if p[1] == "atomic":
            p[0] = ("stmnt4", p[3])
        elif p[1] == "d_step":
            p[0] = ("stmnt5", p[3])
        elif p[1] == "select":
            p[0] = ("stmnt6", p[3])
        elif p[1] == "printm":
            p[0] = ("stmnt13", p[3])
    elif len(p) == 6:                   #PRINTF
        p[0] = ("stmnt14", [(p[3],), p[4]])
    # elif len(p) == 7:
        # p[0] = ("stmnt", [(p[1],), (p[3],), p[5]])
    elif len(p) == 8:                                   # FOR
        p[0] = ("stmnt3", [p[3], p[6]])

def p_stmntopt(p):
    """stmntopt :
                | COMMA arg_lst"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[2]

def p_stmnt4guard(p):
    """stmnt4guard : send
                | receive
                | ELSE
                | expr"""
    # if len(p) == 2:
    #     if type(p[1]) == str:
    #         p[0] = "stmnt", (p[1],)
    #     else:
    #         p[0] = "stmnt", p[1]
    if p[1] == "else":
        p[0] = ("stmnt4guard1", (p[1],))
    else:
        p[0] = ("stmnt4guard2", p[1])


def p_range(p):
    """range    : name COLON any_expr PERIODS any_expr
                | name IN name"""
    if len(p) == 4:
        p[0] = ("range2", [p[1], p[3]])
    else:
        p[0] = ("range1", [p[1], p[3], p[5]])
    # p[0] = ("range", set_p(p)

def p_options(p):
    """options  : COLONS optionseq optionseqs"""
    p[0] = p[2] + p[3]

def p_optionseqs(p):
    """optionseqs   :
                    | COLONS optionseq optionseqs"""
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[2] + p[3]

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
                | RUN name LPAREN arg_lstO RPAREN priorityO
                | GET_PRIORITY LPAREN expr RPAREN
                | SET_PRIORITY LPAREN expr COMMA expr RPAREN"""
    if len(p) == 2:                             # varref or const or TIMEOUT or NP_ or poll
        if p[1] == "timeout":
            p[0] = [("any_expr7", (p[1],))]
        elif(p[1] == "np_"):
            p[0] = [("any_expr8", (p[1],))]
        else:
            p[0] = [("any_expr6", p[1])]
    elif len(p) == 3:                           # unarop any_expr
        p[0] = [("any_expr3", [p[1], p[2]])]
    elif len(p) == 4:                           # LPAREN any_expr RPAREN, any_expr binarop any_expr
        if p[1] == "(":
            p[0] = [("any_expr1", p[2])]
        else:
            p[0] = [("any_expr2", [p[1], p[2], p[3]])]
    elif len(p) == 5:                           # LEN LPAREN varref RPAREN, ENABLED LPAREN any_expr RPAREN, PC_VALUE LPAREN any_expr RPAREN, GET_PRIORITY LPAREN expr RPAREN
        if p[1] == "len":
            p[0] = [("any_expr5", p[3])]
        elif p[1] == "enabled":
            p[0] = [("any_expr9", p[3])]
        elif p[1] == "pc_value":
            p[0] = [("any_expr10", p[3])]
        elif p[1] == "get_priority":
            p[0] = [("any_expr13", p[3])]
    elif len(p) == 8:                           # LPAREN any_expr ARROW any_expr COLON any_expr RPAREN
        p[0] = [("any_expr4", [p[2], p[4], p[6]])]
    else:
        if p[1] == "run":
            p[0] = [("any_expr12", [p[2], p[4], p[6]])]
        elif p[1] == "set_priority":
            p[0] = [("any_expr14", [p[3], p[5]])]
        else:
            p[0] = [("any_expr11", [p[1], p[3], p[6]])]

def p_arg_lstO(p):
    """arg_lstO :
                | arg_lst"""
    if len(p) == 1:
        p[0] = ("none",)
    else:
        p[0] = p[1]

def p_expr(p):
    """expr : any_expr
            | LPAREN expr RPAREN
            | expr andor expr
            | chanpoll LPAREN varref RPAREN"""
    if len(p) == 2:
        p[0] = ("expr1", p[1])
    elif len(p) == 4:
        if p[1] == "(":
            p[0] = ("expr2", p[2])
        else:
            p[0] = ("expr3", [p[1], p[2], p[3]])
    else:
        p[0] = ("expr4", [p[1], p[3]])
        # p[0] = ("expr", set_p(p))

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
    "name    : NAME"
    p[0] = [("name", (p[1],))]

def p_names(p):
    """names    :
                | COMMA name names"""
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[2] + p[3]

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
    # test_file = input()
    test_file = sys.argv[2]
    f = open(test_file, 'r')
    # f = open("test2.pml", 'r')
    data = f.read()
    f.close()

    # parser = yacc.yacc()
    result = parser.parse(data)
    # print('result: ', result)
    print(result)
    # result2 = f"{result}"
    # wresultfile = open("result_analyze.txt", "w")
    # wresultfile.write(result2)
    # wresultfile.close


if __name__ == '__main__':
    # args = sys.argv
    # print("lex test ? or yacc test?")
    # lory = input()
    lory = sys.argv[1]
    if lory == "l":
        lex_test()
    elif lory == "y":
        yacc_test()