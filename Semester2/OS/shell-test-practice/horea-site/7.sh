#!/bin/bash



files=`find dir -type f`
total=0

for file in $files; do
    size=`du $file | awk '{print $1}'`
    total=$((total + size))
done

echo $total

