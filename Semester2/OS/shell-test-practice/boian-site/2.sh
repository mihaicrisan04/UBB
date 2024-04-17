#!/bin/bash

# Find recursively in a directory all ".c" files having more than 500 lines. Stop after finding 2 such files.


files=`find dir -name "*.c"`
k=0

for file in $files; do 
    if [ `wc -l < $file` -gt 500 ]; then
        ((k++)) 
        echo $file
    fi

    if [ $k -gt 1 ]; then
        break
    fi
done 


