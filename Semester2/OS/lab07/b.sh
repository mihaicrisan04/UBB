#!/bin/bash

# Check if the correct number of arguments are provided
if [ $# -lt 2 ]; then
    echo "Usage: $0 <character_sequence> <file_or_folder1> [<file_or_folder2> ...]"
    exit 1
fi

# Extract the character sequence S
S="$1"
shift 1

# Function to check if a file contains the sequence S
check_file_contains_sequence() {
    if [ -f "$1" ]; then
        if grep -q "$S" "$1"; then
            echo "$1 contains the sequence '$S'"
        else
            echo "$1 does not contain the sequence '$S'"
        fi
    else
        echo "$1 is not a regular file"
    fi
}

# Function to check if a folder contains an item with the name equal to the sequence S (recursively)
check_folder_contains_item() {
    if [ -d "$1" ]; then
        if find "$1" -name "$S" | grep -q .; then
            echo "$1 contains an item with the name '$S'"
        else
            echo "$1 does not contain an item with the name '$S'"
        fi
    else
        echo "$1 is not a folder"
    fi
}

# Iterate through the provided arguments
for item in "$@"; do
    check_file_contains_sequence "$item"
    check_folder_contains_item "$item"
done



