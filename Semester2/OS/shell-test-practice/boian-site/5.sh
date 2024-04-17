#!/bin/bash

# work in progress

# Write a script that receives dangerous program names as command line arguments. The script will monitor all the processes in the system, and whenever a program known to be dangerous is run, the script will kill it and display a message.


is_process_running() {
    local process_name="$1"
    ps -C "$process_name" > /dev/null 2>&1
}

kill_process() {
    local process_id="$1"
    kill -9 "$process_id" > /dev/null 2>&1
}

dangerous_programs=("$@")

while true; do
    for program_name in "${dangerous_programs[@]}"; do
        if is_process_running "$program_name"; then
            process_id=$(pidof "$program_name")
            kill_process "$process_id"
            echo "Killed dangerous program: $program_name"
        fi
    done
    sleep 1
done

