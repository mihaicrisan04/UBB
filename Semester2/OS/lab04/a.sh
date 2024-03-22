#!/bin/bash


# EASY
# AWK: print the first 4 fields of every even line in the file last.fake
# awk 'NR%2==0 {print $1, $2, $3, $4}' file.txt

# GREP: print all the lines that contain only non-alphanumeric characters from
# a file
#grep -E '^[^a-zA-Z0-9]*$' file.txt

# SED: Duplicate each occurrence of an integer number in a file. We will
# consider that an integer is a sequence of neighboting difits in base 10
#sed -E 's/[0-9]+/&&/g' file.txt

# SED: swap field number 2 with field number 3 from a file where the fields are
# separated by the ':' character
#sed -E 's/([^:]*):([^:]*):([^:]*)/\1:\3:\2/' file.txt


# MEDIUM
# GREP: display all the lines from a file that contain between 2 and
# 4 occurrences of the letter 'i', not necessarily consecutive
#grep -E '^([^i]*i[^i]*){2,4}$' passwd.fake

# SED: delete all characters after the last wihitespace from each line from
# a file
#sed 's/ [^ ]*$//' passwd.fake

# AWK:  print the number and the field from the middle of the line from each line
# that contains an odd number of fields from a file
#awk 'NF%2==1 {print NR, $(int(NF/2)+1)}' file.txt

# SED: Remove the first word containing only only lowercase letters from each
# line of a file
#sed 's/\<[a-z]*\>//' last.fake
