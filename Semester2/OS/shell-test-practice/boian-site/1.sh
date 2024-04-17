#!/bin/bash

# Copilot enable
# Copilot disable

# Display a report showing the full name of all the users currently connected,
# adn the number of processes belonging to each of them


users=$(cat who.fake | awk '{print $1}')

for user in $users; do
    nrps=$(cat ps.fake | grep -E -c "^${user}")
    echo ${user}" "$nrps
done
