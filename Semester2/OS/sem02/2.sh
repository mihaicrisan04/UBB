#!/bin/bash

# wirte a bash script that takes as cmd line arguments pairs consisting of
# (filename, integer) and chceks if the file size is less than the specified
# integer
# ./script.sh file1 100 file2 200 file3 300


total=0
while [ $# -gt 1 ]; do
    echo "file $1 - int: $2"
    if [ -n $1 ] && [ -f $1 ]; then 
        size=$(du -b $1 | awk '{print $1}')
        if [ $size -lt $2 ]; then
            echo "filesize of $1 is less than $2"
        else
            echo "filesize of $1 is greater than $2"
        fi
        total=$((total+size))
    else
        echo "argument $1 is not a file"
    fi
    shift 2
done

echo "total size of all files: $total"
echo "value of size: $size"
