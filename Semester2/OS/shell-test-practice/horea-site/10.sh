#!/bin/bash


for file in $@; do 
    if [ ! -f $file ]; then
        echo "$file is not a file"
        exit1
    fi
done

for file in $@; do
    du $file
done | sort -r 
