import ply.lex as lex
import ply.yacc as yacc
#List of reserved words
reserved = {
    'proctype': 'PROCTYPE',
    'active':'ACTIVE',
    'true':'TRUE',
    'false':'FALSE',
    'skip':'SKIP',
    'proctype':'PROCTYPE',
    'if':'IF',
    'fi':'FI',
    'do':'DO',
    'od':'OD',
    'atomic':'ATOMIC',
    'd_step':'D_STEP',
    'int':'INT',
    'printm':'PRINTM',
    'mtype' : 'MTYPE'
}


# List of token names
tokens = [
    # "ALPHA",
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
    "EOF",
    "NAME",
    'PLUS',
    'MINUS',
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
    "COMMENT",
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
t_MINUS = r'-'
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
t_COMMA = r','
t_ignore = ' \t'

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    #t.typeはdefaultではt_以下の文字列に設定されている
    t.type = reserved.get(t.value, "NAME") #reservedに登録されているかのチェック登録されていなければ第二引数が代入される
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


precedence = (
    ("left", "PLUS", "MINUS"),
    ("left", "TIMES", "DIVIDE", "MOD")
)

start = "module"

def p_module(p):
    """module   : proctype
                | mtype
    """
    p[0] = p[1]

def p_proctype(p):
    "proctype : active PROCTYPE name LPAREN RPAREN LBRACE sequence RBRACE"
    p[0] = p[2]

def p_active(p):
    """active   : ACTIVE
                | ACTIVE LBRACKET const RBRACKET"""
    p[0] = p[1]

def p_active_error(p):
    "active : ACTIVE LBRACKET error RBRACKET"
    print("Syntax error in active. Bad expression")

def p_const(p):
    """const    : TRUE
                | FALSE
                | SKIP
                | NUMBER"""
    p[0] = p[1]

def p_name(p):
    "name : NAME"
    p[0] = p[1]

def p_sequence(p):
    """sequence     : step
                    | step SEMI sequence"""

def p_step(p):
    "step : stmnt"

def p_stmnt(p):
    """stmnt    : IF options FI
                | DO options OD
                | ATOMIC LBRACE sequence RBRACE
                | D_STEP LBRACE sequence RBRACE
                | LBRACE sequence RBRACE
                | PRINTM LPAREN name RPAREN
                | expr"""
    if p[1] == "LBRACE":
        p[0] = "sequence"
    else:
        p[0] = p[1]

def p_options(p):
    """options  : COLONS sequence
                | COLONS sequence options"""


def p_mtype(p):
    "mtype : MTYPE LBRACE names RBRACE"
    p[0] = p[1]

def p_names(p):
    """names    : name
                | name COMMA names"""

def p_any_expr(p):
    """any_expr : LPAREN any_expr RPAREN
                | const
                | any_expr binarop any_expr"""
    if len(p) == 4:
        p[0] = p[2]
    elif len(p) == 2:
        p[0] = p[1]
    elif p[2] == "binarop":
        match p[2]:
                case "PLUS":
                    p[0] = p[1] + p[3]
                case "MINUS":
                    p[0] = p[1] - p[3]
                case "TIMES":
                    p[0] = p[1] * p[3]
                case "DIVIDE":
                    p[0] = p[1] // p[3]
                case "MOD":
                    p[0] = p[1] % p[3]
        print(p[0])
#                 case "AND":
#                     p[0] = p[1] & p[3]
#                 case "XOR":
#                     p[0] = p[1] ^ p[3]
#                 case "OR":
#                     p[0] = p[1] | p[3]
#                 case "GT":
#                     p[0] = p[1] > p[3]
#                 case "LT":
#                     p[0] = p[1] < p[3]
#                 case "GE":
#                     p[0] = p[1] >= p[3]
#                 case "LE":
#                     p[0] = p[1] <= p[3]
#                 case "EQ":
#                     p[0] = p[1] == p[3]
#                 case "NE":
#                     p[0] = p[1] != p[3]
#                 case "LSHIFT":
#                     p[0] = p[1] << p[3]
#                 case "LE":
#                     p[0] = p[1] >> p[3]
#                 case "andor":
#                     if p[2] == "LAND":
#                         p[0] = p[1] and p[3]
#                     elif p[2] == "LOR":
#                         p[0] = p[1] or p[3]

def p_binarop(p):
        """binarop  : PLUS
                    | MINUS
                    | TIMES
                    | DIVIDE
                    | MOD"""
#                     | AND
#                     | XOR
#                     | OR
#                     | GT
#                     | LT
#                     | GE
#                     | LE
#                     | EQ
#                     | NE
#                     | LSHIFT
#                     | RSHIFT
#                     | andor"""

def p_expr(p):
    """expr : any_expr
            | LPAREN expr RPAREN
    """
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        p[0] = p[2]

# def p_andor(p):
#     """andor    : LAND
#                 | LOR
#     """

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


# ############# test
def lex_test():
    f = open('test2.pml', 'r')
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
    f = open('test2.pml', 'r')
    data = f.read()
    f.close()

    # parser = yacc.yacc()
    result = parser.parse(data)
    print('result: ', result)


if __name__ == '__main__':
    # lex_test()
    yacc_test()