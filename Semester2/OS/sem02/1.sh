#!/bin/bash


a="value 1"
aa="value 2"

echo "arg 1: $1"
echo "arg 2: $2"
echo "arh 10: ${10}"
echo var a: ${a}
echo var aa: ${aa}

echo "all args: $@"

for i in $@; do
    echo "arg: $i"
done
