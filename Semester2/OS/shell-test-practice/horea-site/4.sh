#!/bin/bash


for file in $@; do
    if [ -f $file ]; then
        echo "$file is a regular fle"
    elif [ -d $file ]; then
        echo "$file is a directory"
    elif echo $file | grep -E -q "^[0-9]+$"; then
        echo "$file is an integer number"
    else
        echo "$file is something else"
    fi
done

