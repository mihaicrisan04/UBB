#!/bin/bash


# Find recursively in a directory, all the files with the extension ".log" and sort their lines (replace the original file with the sorted content).



files=`find dir -name "*.log"`

touch tmp
for file in $files; do
    sort $file > tmp
    cp tmp $file
done

rm tmp
