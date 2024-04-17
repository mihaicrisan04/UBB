#!/bin/bash


# write a bash script that counts all the lines of code in the C files from the
# directory as given as command-line argumen, excluding lines that are empty or
# contain spaces


files=`find dir -type f -name "*.c"`
total=0
for file in $files; do
    nr_lines=`grep -E -c -v "^[ \t]*$" $file`
    echo "$file: $nr_lines"
    total=$((total + nr_lines))
done

echo $total
