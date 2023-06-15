keyword_dict = {"proctype":-1, "init":-2, "never":-3, "trace":-4, "typedef":-5, "mtype":-6, "unsigned":-7, 
              "bit":-8, "bool":-9, "byte":-10, "short":-11, "int":-12, "chan":-13, "active":-14, "priority":-15, 
              "provide":-16, "hidden":-17, "show":-18, "unless":-19, "xr":-20, "xs":-21, "of":-22, "eval":-23, 
              "if":-24, "fi":-25, "do":-26, "od":-27, "for":-28, "atomic":-29, "d_step":-30, "select":-31, 
              "else":-32, "break":-33, "goto":-34, "print":-35, "assert":-36, "in":-37, "len":-38, "timeout":-39, 
              "np_":-40, "enabled":-41, "pc_value":-42, "run":-43, "full":-44, "empty":-45, "nfull":-46, "nempty":-47, 
              "true":-48, "false":-49, "skip":-50}


aaa = keyword_dict.get("show", "NAME")
bbb = keyword_dict.get("wohasdklhj", "NAME")

print(aaa)
print(bbb)