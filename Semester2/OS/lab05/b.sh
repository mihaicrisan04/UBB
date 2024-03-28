#!/bin/bash

# find and print recursively all the C source files from a given directory
# and creates a file containing all the includes used across the found files
# without duplicates 

if [ $# -ne 1 ]; then
    echo "wrong number of argumets"
    exit 1
fi

dir=$1

if [ -f result ]; then
    rm result
fi

for file in $(find $dir); do
    if file $file | grep -E -q "C source"; then
        grep -E "^[ ]*#include" $file >> out
    fi
done | sort | uniq > result
