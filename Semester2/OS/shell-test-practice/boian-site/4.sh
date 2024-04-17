#!/bin/bash

# Find recursively in a given directory all the symbolic links, and report those that point to files/directories that no longer exist. Use operator -L to test if a path is a symbolic link, and operator -e to test if it exists (will return false if the target to which the link points does not exist)



files=`find dir`

for file in $files; do
    if [ -L $file ]; then
        if [ ! -e $file ]; then
            echo $file" points to an inexistent file"
        fi
    fi
done


