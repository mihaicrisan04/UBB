#!/bin/bash

# write a bash script that recieves at the command line pairs containing
# filename and word
# for every pair print an approppriate message if the word
# appears in the file at least 3 times


if [ $# -eq 0 ]; then
    echo "No arguments provided"
    exit 1
fi

while [ $# -gt 1 ]; do
    file=$1
    word=$2
    
    if [ ! -f $file ]; then
        echo "File $file does not exist"
        shift 2
        continue
    fi
    
    count=$(grep -E -o "\<$word\>" $file | grep -E -c ".*") 
    if [ $count -ge 3 ]; then
        echo "The word $word appears at least 3 times in $file"
    fi

    shift 2
done

if [ $# -eq 1 ]; then
    echo "The last argument is not a pair"
fi
