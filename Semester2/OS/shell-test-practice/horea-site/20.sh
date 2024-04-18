#!/bin/bash


words=($@)
for word in $words; do
    echo $word" "
done

echo "Enter file names"
file=""
while [ ${#words[@]} -ne 0 ]; do
    read -r file
    if [ ! -f "$file" ]; then
        echo "File does not exist"
        continue
    fi
    new_words=()
    for word in $words; do 
        if grep -q "$word" "$file"; then
            echo "$word found in $file"
        else 
            new_words+=($word)
        fi 
    done
    words=("${new_words[@]}")
done
