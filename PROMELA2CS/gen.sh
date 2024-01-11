PARSER=/Users/yamakawamanobe/Master/promela_compiler/pmlparser2.py
EDIT=/Users/yamakawamanobe/Master/promela_compiler/edit_result_analyze.py
DIR=/Users/yamakawamanobe/Master/promela_compiler
TESTDIR=/Users/yamakawamanobe/Master/promela_compiler/PROMELA2CS/testpml4gencsprime

python3 $PARSER y $DIR/$1 | python3 $EDIT > hoge.txt
echo "convartTree:start2(" `cat hoge.txt` ")."\
 |erl
cat result_tree.txt\
 | perl -e 'while (<>) { chop; print " $_"; } printf("\n");' > hoge2.txt
# echo "pml2cs:start(" `cat hoge2.txt` ")."\
#  |erl
#  | perl -e 'while (<>) { chop; print " $_"; }' > hoge2.txt