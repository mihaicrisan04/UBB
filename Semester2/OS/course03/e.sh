#!/bin/bash

D = $1
S = $2

if ! test -d $D
then
    echo "Directory $D does not exist"
    exit 1
fi

# ` backticks are used to execute the command and return the output of the command
#for F in `find $D -type f 2 > /dev/null`; do
#    N = `ls -l $F | awk '{print $5}'`
#    if [ $N -gt $S ]; then
#         echo $F
#    fi 
#done


for F in `find $D -type f 2 > /dev/null`; do
    N = `ls -l $F | awk '{print $5}'`
    if [ $N -gt $S ]; then
         echo $F
    fi 
done
