#!/bin/bash


file=""

while read file; do 
    if [[ $file == "stop" ]]; then
        break
    fi

    if file $file | grep -q "ASCII text"; then
        # words on the first line
        words=$(head -n 1 $file | wc -w)
        echo $words
    fi
done


