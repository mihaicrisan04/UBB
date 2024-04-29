#!/bin/bash

echo "20" > numbers.txt
for i in {1..20}; do
   echo "$i" >> numbers.txt
done
