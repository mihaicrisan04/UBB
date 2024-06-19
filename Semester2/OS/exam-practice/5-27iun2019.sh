#!/bin/bash


dirs=`find . -type d`

for dir in $dirs; do
    txt_files_count=0
    other_files_count=0

    files=`ls $dir`

    for file in $files; do
        if [ -f $file ]; then
            if [ "$file" == "*.txt" ]; then
                txt_files_count=$((txt_files_count+1))
            else
                other_files_count=$((other_files_count+1))
            fi
        fi
    done

    #median=$((txt_files_count/other_files_count))
    echo $dir $txt_files_count $other_files_count

done

