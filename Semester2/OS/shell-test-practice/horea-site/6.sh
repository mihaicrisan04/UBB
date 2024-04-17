#!/bin/bash



files=`find dir -type f`

for file in $files; do
    du $file | awk '{print $1" "$2}'
done | sort -n



