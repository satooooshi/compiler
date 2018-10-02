#!/bin/bash
BIN=lextest
REFOUTDIR=refs
TESTCASEDIR=./testcases

rm -f _tmp.txt .tmp.txt	
make clean >& /dev/null
make > /dev/null
if [[ $? != 0 ]]; then
	echo "[-_-]$ite: Compile Error"		
	echo "TOTAL SCORE: 0"
	exit 123
fi	

for tcase in `ls $TESTCASEDIR`
do		
	if [ ${tcase##*.} = "tig" ]; then
		tfileName=${tcase##*/}

		./$BIN $TESTCASEDIR/$tfileName >& _tmp.txt #./lextest testcases/...tig >& //2>&1 が標準エラー出力(2)を標準出力(1)に混ぜて入れることを意味しています
		diff $DIFFOPTION _tmp.txt $REFOUTDIR/${tfileName%.*}.out >& .tmp.txt #diff .tig .out
		if [ -s .tmp.txt ]; then	# exists difference
			cat .tmp.txt
			echo "[*_*]$ite: Output Mismatch [$tfileName]"				
			rm -f _tmp.txt .tmp.txt	
			echo "TOTAL SCORE: 0"
			exit 234
		fi
	fi	
done

rm -f _tmp.txt .tmp.txt	
echo "[^_^]$ite: Pass"
echo "TOTAL SCORE: 100"

