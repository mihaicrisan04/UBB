#!/bin/bash 

echo 0 > z

./f.sh z &
./f.sh z & 
./f.sh z &

