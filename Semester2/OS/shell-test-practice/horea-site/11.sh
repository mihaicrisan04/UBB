#!/bin/bash


if [ ! -f "11.out" ]; then
    touch "11.out"
fi

files=`find dir -type f`

for file in $files; do
    if file $file | grep -q -E "c program"; then
        cat $file | grep -E "^#include " | awk '{print $2}' >> 11.out
    fi
done
