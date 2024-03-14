#!/bin/bash
# Write a bash script that prints al the processes belonging to a specified
# user


u=$1
#ps -ef | grep -E "^$u"

tmp="$IFS"
IFS=$'\n'

for line in $(ps -ef); do
    user=$(echo $line | awk '{print $1}')
    if [ "$user" = "$u" ]; then
        echo $line
    fi
done
IFS="$tmp"
