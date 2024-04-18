#!/bin/bash


while [ $# -gt 0 ]; do
    file=$1
    word=$2
    appear=`grep -o $word $file | wc -l`
    if [ $appear -gt 1 ]; then
        echo "$appear $word in $file"
    fi
    shift 2
done 





