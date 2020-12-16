#!/bin/bash

sudo find /var/log -name '*.log' > logs.tmp

LINES_COUNT='0'

while read LINE
do
    if [ -r $LINE ]
    then
        LINES_IN_FILE=$(wc -l < $LINE)
        LINES_COUNT=$(($LINES_COUNT+$LINES_IN_FILE))
    fi
done < logs.tmp
rm logs.tmp

echo $LINES_COUNT
