#!/bin/bash

# write a bash script that recieves as a command line arugments file names
# the script will output the unique lines obtained by removing all letters and
# digits from the files content


for file in $@; do
    if [ -f $file ]; then
        sed "s/[a-zA-Z0-9 ]//g" $file 
    fi
done | sort | uniq




