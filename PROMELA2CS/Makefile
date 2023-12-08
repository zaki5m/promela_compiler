TESTFILE = test2.pml
.SUFFIXES: .erl .beam

go: pr cs

pr: convartTree.beam
	sh gen.sh $(TESTFILE)

cs: pml2cs.beam convartTree.beam my_utility.beam setmanager.beam
	echo "pml2cs:start(" `cat hoge2.txt` ")."\
	|erl

.erl.beam:
	erlc $<