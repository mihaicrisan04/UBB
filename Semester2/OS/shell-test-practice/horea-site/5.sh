#!/bin/bash


file=""
while true; do
    read file
    if [ -f $file ]; then
        break
    fi
done


