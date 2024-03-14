#!/bin/bash

FILE=$1
N=0

while [ $N -lt 200 ]; do
    K=`cat $FILE`
    K=`expr $K + 1`
    echo $K > $FILE
    N=`expr $N + 1`
done    
