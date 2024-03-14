#!/bin/bash

# write a shell script that printst all the names of the c source files from
# a given directory

if [ -d $1 ]; then
    for f in $(ls $1); do
        f_path="$1/$f"
        if file ${f_path} | grep -E -q "C source"; then
            echo ${f_path} 
        fi
        echo $f
    done    
fi


