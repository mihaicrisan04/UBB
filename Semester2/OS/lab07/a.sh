#!/bin/bash

if [ $# -eq 0 ]; then
    echo "No arguments given"
    exit 1
fi

while true; do
    clear
    for username in "$@"; do
        if grep -q -E "^${username}:" /etc/passwd; then
            processes=`ps aux | grep -c -E "^${username}"`
            echo "User ${username} has ${processes} processes"
        fi    
    done
    sleep 1
done

