import keyword_dict
import re

# f = open("test.pml", "r")¥

class Tok(enum):
    tok_eof = -1
    tok_keyword = -2
    tok_identifier = -3
    tok_number = -4

def gettok(token):
    tokalpha_mo = re.match(r"[a-zA-Z]", token[0])
    tokdigit_mo = re.match(r"[0-9]", token[0])
    if tokalpha_mo != None:
        if token in keyword_dict.keyword_dict:
            return keyword_dict.keyword_dict[token]
        else:
            print("bbbb")
    elif tokdigit_mo != None:
        return tok_num

print(gettok("aactive"))

# f_line = f.readline()
