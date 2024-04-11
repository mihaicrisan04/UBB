#!/bin/bash


if [ $# -lt 1 ]; then
    echo "Nooooo"
    exit 1
fi

S=$1
shift 1

for f in $@; do
    if [ -f $f ];then 
        if grep -E -q "$S" $f; then
            echo "File $f contains string $S"
        else
            echo "File $f does not contain string $S"
        fi
    elif [ -d $f ]; then
        if `find $f -name $S | wc -l` -gt 0; then
            echo "Directory $f contains file $S"
        else
            echo "Directory $f does not contain file $S"
        fi
    fi
done
