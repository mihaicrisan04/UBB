#!/bin/bash

# find and print recursively all the C source files from a given directory

if [ $# -ne 1 ]; then
    echo "wrong number of argumets"
    exit 1
fi

dir=$1

for file in $(find $dir); do
    if file $file | grep -E -q "C source"; then
        echo $file
    fi
done

