#!/bin/bash

# Function to reverse the operation and update the database file
reverse_operation() {
    local db_file="$1"
    local id="$2"
    local operation="$3"
    local sign="$4"
    local number="$5"
    
    case "$operation" in
        "ad")
            # Delete the row with the specified ID
            sed -i "/^$id /d" "$db_file"
            ;;
        "del")
            # Add a new row with the specified ID and value 0
            echo "$id 0" >> "$db_file"
            ;;
        "ch")
            # Reverse the modification based on the argument
            case "$sign" in
                "+") sed -i "s/^$id \([0-9]\+\)/$id \$((\1 - $number))/e" "$db_file";;
                "-") sed -i "s/^$id \([0-9]\+\)/$id \$((\1 + $number))/e" "$db_file";;
                "*") sed -i "s/^$id \([0-9]\+\)/$id \$((\1 / $number))/e" "$db_file";;
                "/") sed -i "s/^$id \([0-9]\+\)/$id \$((\1 * $number))/e" "$db_file";;
            esac
            ;;
    esac
}

# Main script

# Check if the correct number of arguments is provided
if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <database_files...> <log_files...>"
    exit 1
fi

# Extract the database filenames from the command-line arguments
log_files=()
database_files=()

for file in "$@"; do
    if [[ "$file" == *.db ]]; then
        database_files+=("$file")
    else
        log_files+=("$file")
    fi
done

# Process each log file
for log_file in "${log_files[@]}"; do
    # Read each line of the log file
    while IFS= read -r line; do
        # Extract the fields from the log entry
        IFS=' ' read -r -a fields <<< "$line"
        db_file="${fields[0]}"
        id="${fields[1]}"
        operation="${fields[2]}"
        arguments="${fields[3]}"
        sign="${arguments:0:1}"
        number="${arguments:1}"
        
        # Reverse the operation and update the database file
        reverse_operation "$db_file" "$id" "$operation" "$sign" "$number"
    done < "$log_file"
done

