#!/bin/bash





# awk
awk -F/ '$0 ~ /.txt$/ {print $4}' files.fake


# grep
grep -E "\.php" trafic.fake


# sed
cat trafic2.fake | sed -E 's/\[([0-9]+)\/([0-9]+)\/([0-9]+) (.+)\]/[\3\/\2\/\1\ \4]/g' | sed -E 's/\[(.+) (.+)\]/[\2 \1]/g' 

