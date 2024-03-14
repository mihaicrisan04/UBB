#!/bin/bash

for A in $@; do
    if echo $A | grep -q -E "^[0-9]*[02468]$" && \
        ( test -f $A || test -d $A ); then 
        echo "Behold the amazing even file name $A"
    elif test -f $A; then
        echo "$A is a file"
    elif test -d $A; then
        echo "$A is a directory"
    elif echo $A | grep -q -E "^[0-9]+$"; then
       echo "$A is a number" 
    else 
        echo "$A what?"
    fi
done

#if test -f $A; then
#if [ -f $A ]; then must have space after [ and before ]
