#!/bin/bash 


# Find recursively in a directory, all the files that have write permissions for everyone. Display their names, and the permissions before and after removing the write permission for everybody. You will need to use chmod's symbolic permissions mode, instead of the octal mode we have used in class. The the chmod manual for details.


files=`find dir -type f`

for file in $files; do
    ls -l $file
    chmod o-w $file
    ls -l $file
done


