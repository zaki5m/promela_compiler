TESTFILE = test2.pml
OUT = CSedges.out
LOOPNUM = 6
.SUFFIXES: .erl .beam


go: pr cs 

debug:pr cs2

pr: convartTree.beam genTree.beam
	sh gen.sh $(TESTFILE)

cs: pml2cs.beam convartTree.beam my_utility.beam setmanager.beam cs2csprime.beam convertCSprime_utility.beam csprime2erl.beam funmanager.beam erlwriter.beam generlutility.beam separateCSprime.beam
	echo "pml2cs:start(" `cat hoge2.txt` ")."\
	|erl

cs2: pml2cs.beam convartTree.beam my_utility.beam setmanager.beam cs2csprime.beam convertCSprime_utility.beam separateCSprime.beam
	echo "pml2cs:start(" `cat hoge2.txt` ")."\
	|erl

convdata:
		sh converl2cs.sh $(OUT)

csprimetest: cs convdata

test:
	sh csprimetest.sh $(LOOPNUM)

.erl.beam:
	erlc $<