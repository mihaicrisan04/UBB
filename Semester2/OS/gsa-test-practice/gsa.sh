#!/bin/bash

# print the first 4 columns of the file last.fake
#awk '{print $1" "$2" "$3" "$4}' last.fake


# print the lines that contain only non-alphanumeric characters
#grep -E "^[^a-zA-Z0-9]*$" file.txt


# duplicate each number in the file
#sed -E "s/([0-9]+)/\1\1/g" file.txt


# remove all the words after the last space
#sed -E "s/(\s)[^[:space:]]*$/\1/" file.txt


# print the line number and the middle column of the file
#awk 'NF % 2 == 1 {i=int(NF/2); print NR" "$i}' file.txt


# swap the second and third columns of the file
# separated by :
#sed -E "s/([^:]+):([^:]+):([^:]+):(.*)$/\1:\3:\2:\4/" passwd.fake


# print the line if it has at most 5 vowels between ^ ^ characters
#grep -E "\^([^aeiou]*[aeiou][^aeiou]*){0,5}\^" file.txt


# remove the first word of each line containing only lowercase letters
#sed -E "s/^([a-z]+)([:space:]+)(.*)$/\2\3/" last.fake



# 9.
# format: num_proccesses user ->  from ps -ef
#ps -ef | grep -E -v "^UID" | awk '{print $1}' | sort | uniq -c


# 10.
# display only the name of the file and the permissions for the user
#ls -l | awk 'NF == 9 {print $9" "$1}' | sed -E "s/(-[rwx-]{3}).*/\1/"


# 11.
# 
