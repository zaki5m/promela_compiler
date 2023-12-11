# 参考サイト
* PROMELA文法
    * https://spinroot.com/spin/Man/grammar.html#ivar
* PLYリファレンス
    * https://www.dabeaz.com/ply/ply.html#ply_nn23

# 必要ファイル
* promela_compiler
    * 入力となるpmlファイル(makeファイルのTESTFILEでファイル名指定)
    * pmlparser2.py
    * edit_result_analyze.py
* PROMELA2CS内
    * gen.sh
    * Makefile
    * convartTree.erl
    * pml2cs.erl
* testpmlはparserのテストファイルが入ってます


# スクリプト系色々
* parserのテスト
    *  csh testscript.csh
* make
    * PROMELA2CSで `make`
    * ``make go`` ``make pr`` ``make cs`` で色々できる

