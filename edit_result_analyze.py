import re

# with open("result_analyze.txt", encoding="cp932") as f:
    # data_lines = f.read()
data_lines = input()
# pattern = r"\('([\"a-zA-Z0-9!-/:-@[-`{-~=\s]*)',\)"
pattern = r"\('([^,]*)',\)"
replacement = r"('\1')"
pattern2 = r"'([0-9])+'"
replacement2 = r"\1"
newdatalines = re.sub(pattern, replacement, data_lines)
newdatalines = re.sub(pattern2, replacement2, newdatalines)
newdatalines = re.sub("\(", "{", newdatalines)
newdatalines = re.sub("\)", "}", newdatalines)
newdatalines = re.sub("\"", "", newdatalines)
newdatalines = re.sub("None", "none", newdatalines)


# with open("/Users/yamakawamanobe/Master/promela_compiler/PROMELA2CS/result_analyze_forerl.txt", mode="w", encoding="cp932") as f2:
    # f2.write(newdatalines)

# f.close()
# f2.close()

print(newdatalines)