CONVERTER=/Users/yamakawamanobe/Master/promela_compiler/PROMELA2CS/draw_graph/converterl2pydata.py
PUGEN=/Users/yamakawamanobe/Master/promela_compiler/PROMELA2CS/draw_graph/genCSprimepu.py
DIR=/Users/yamakawamanobe/Master/promela_compiler/PROMELA2CS
PU=/Users/yamakawamanobe/Master/promela_compiler/PROMELA2CS/draw_graph/csprime.pu

# python3 $CONVERTER $DIR/$1 | $PUGEN | plantuml $PU
python3 $CONVERTER $DIR/$1