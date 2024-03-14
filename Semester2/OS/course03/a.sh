#/bin/bash
A=7
B=9
expr 7 + 9
expr $A + $B

echo $* 
echo $@
echo $0 $1 $2 $3 $4 $5 $6 $7 $8 $9
shift 2
echo $*
echo $@
echo $0 $1 $2 $3 $4 $5 $6 $7 $8 $9
echo $#
echo $?
