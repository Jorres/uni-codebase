#!/bin/bash

MAX=0
MAX_DATE=""

ls "/home/jorres/" > tmp.tmp
while read LINE; do
    if echo "$LINE" | grep -q "Backup"; then
        THIS_DATE_SECONDS=$(date -d ${LINE:7} +%s)
        if [[ $MAX -eq 0 ]] || [[ $THIS_DATE_SECONDS -ge $MAX ]]; then
            MAX=$THIS_DATE_SECONDS    
            MAX_DATE=${LINE:7}
        fi
    fi
done < tmp.tmp
rm tmp.tmp

if [[ $MAX -eq 0 ]]; then
    echo "No backup directory found"
else
    ls "/home/jorres/Backup-$MAX_DATE" |
    while read LINE; do
        if echo "$LINE" | grep -qEo "[0-9]{4}-[0-9]{2}-[0-9]{2}"; then
            cp "/home/jorres/Backup-$MAX_DATE/$LINE" "/home/user/restore/$LINE"
        fi
    done
fi
