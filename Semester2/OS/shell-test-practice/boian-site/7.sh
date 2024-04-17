#!/bin/bash



at="@scs.ubbcluj.ro"
string=""


usernames=`cat input`

for username in $usernames; do
    string="$string$username$at,"
done

# remove last character
string=${string%?}
echo $string

# remove last n characters
# string=${string%${n}*}
