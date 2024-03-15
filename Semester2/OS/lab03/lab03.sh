#!/bin/bash

# 1. display all the lines from passwd file contating the string "dan"
#    (lowecase only)

grep -E "dan" passwd.fake

# 2. display the number of lines in a file

grep -E -c ".*" passwd.fake

# 3. display the lines from passwd file belonging to users with a surname
# (family name) at least 11 characters long

grep -E "^[^:]+:[^:]+:[^:]+:[^:]+:[^:]{11,} " passwd.fake

# 4. display the lines from passwd file that do not begin with the letters e or
# y

grep -E "^[^ey].*" passwd.fake
