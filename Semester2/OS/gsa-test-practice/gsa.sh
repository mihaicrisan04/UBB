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
# display the number of lines that end in a vowel and the number of lines that
# end in a consonant from a flie
#awk 'BEGIN{vowels=0;consonants=0}{if ($0 ~ /[aeiou]$/) {vowels++} else {consonants++}} END {print "Vowels: "vowels"\nConsonants: "consonants}' file.txt


# 12.
# calculate the sum of all the PID from ps aux that use the vim
#ps aux | grep -E "(vim|joe|nano|pico|emacs)" | awk '{sum+=$2} END {print sum}'


# 13.
# display all the users in the system which name starts with a vowel from the
# passwd file
#awk -F: '$5 ~ /^[aeiouAEIOU]/ {print $1}' passwd.fake



# 14.
# display the files that have persmisions for read
#ls -l | grep -E "^-(r..){3}"




# 15.
# using the last command display how many users have logged into the system
#last | awk 'NF == 9 {print $1}' | sort | uniq -c | sort -rn



# 16.
# using only grep, display the nuumber of lines in a file
#grep -E -c "" last.fake


# 17.
#sort last.fake | uniq -c | awk '$1 > 1 {c++} END {print c}'
# sort -> sorts the lines in a file
# uniq -c -> counts the number of occurences of each line


# 18.
#ls -l | awk '$1 ~ /^-/ {print $9}'


# 19.
#cat last.fake | awk '{print $1}' | sed "y/0123456789/1234567890/" | sort | uniq -c | sort -n 






