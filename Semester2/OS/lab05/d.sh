#!/bin/bash

# write a bash script that calculates the sum of all the pids running on the
# system

# ps -ef | awk '{print $2}' | awk 'BEGIN {sum=0;c=0;} {sum += $1; c+=1} END {print sum/c}'

for x in $(ps -ef | grep -E "^yz[0-9]+" | awk '{print $2}'); do
    c=$((c + 1))
    sum=$((sum + x))
done 

if [ $c -ne 0]; then
    echo $((sum / c))
fi

