#!/bin/bash

USER_PREF="/home/jorres/"

LAST_PERMITTED=$(date -d '7 days ago' +%s)
NEED_TO_CREATE=1

ls "$USER_PREF" > tmp.tmp
while read LINE; do
    if echo "$LINE" | grep -q "Backup"; then
        DATEPART=${LINE:7}
        THIS_DATE_SECONDS=$(date -d "${DATEPART}" +%s)
        if [[ $THIS_DATE_SECONDS -ge $LAST_PERMITTED ]]; then
            NEED_TO_CREATE=0
            BACKUP_NAME=$LINE
        fi
    fi
done < tmp.tmp
rm tmp.tmp

DATE_ADDITION=$(date -d 'now' "+%Y-%m-%d")

if [[ "$NEED_TO_CREATE" == "1" ]]; then
    mkdir "$USER_PREF/Backup-$DATE_ADDITION"
    echo "New backup folder with date $DATE_ADDITION created" >> "$USER_PREF/backup_report"

    ls "$USER_PREF/source/" |
    while read LINE; do
        CUR_PATH="$USER_PREF/source/$LINE"
        cp "$CUR_PATH" "$USER_PREF/Backup-$DATE_ADDITION/$LINE"
        echo "$LINE" >> "$USER_PREF/backup_report"
    done
else
    echo "Old backup directory is still fresh, copying into $BACKUP_NAME..."

    ls "$USER_PREF/source" |
    while read LINE; do
        CUR_PATH="$USER_PREF/source/$LINE"
        if [[ -f "$USER_PREF/$BACKUP_NAME/$LINE" ]]; then
            CUR_SIZE=$(wc -c < $CUR_PATH)
            BACKUP_SIZE=$(wc -c < "$USER_PREF/$BACKUP_NAME/$LINE")
            if [[ $CUR_SIZE -ne $BACKUP_SIZE ]]; then
                cp "$CUR_PATH" "$USER_PREF/$BACKUP_NAME/$LINE.$DATE_ADDITION"
                echo "$LINE.$DATE_ADDITION" >> "$USER_PREF/backup_report"
            fi
        else
            cp "$CUR_PATH" "$USER_PREF/$BACKUP_NAME/$LINE"
            echo "$LINE" >> "$USER_PREF/backup_report"
        fi
    done
fi
