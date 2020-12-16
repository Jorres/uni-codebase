#!/bin/bash

FILENAME=$1
TRASH_PATH="/home/jorres/.trash"
TRASH_LOG_PATH="/home/jorres/.trash/.trash.log"

echo "" >> tmp.tmp

           # <&10
while read -u 10 LINE; do
    if [[ "$LINE" == "" ]]; then
        continue
    fi

    OLD_PATH_LENGTH=$(echo "$LINE" | awk -F" " '{print length($0) - length($NF)}')
    OLD_CAT_LENGTH=$(echo "$LINE" | awk -F"/" '{print length($0) - length($NF)}')

    OLD_PATH=${LINE::OLD_PATH_LENGTH}

    OLD_CAT=${LINE::OLD_CAT_LENGTH}
    LOG_FILENAME=${LINE:OLD_PATH_LENGTH}

    if echo "$OLD_PATH" | grep -q "$FILENAME"; then
        echo "Do you want to restore following file [Y/N]: $OLD_PATH ?";
        read USR_RESP
        if [[ "$USR_RESP" == "Y" ]]; then
            #  echo "restoring" "$OLD_PATH"
            ln "$TRASH_PATH/$LOG_FILENAME" "$OLD_PATH"
            rm "$TRASH_PATH/$LOG_FILENAME"
        else
            echo "$LINE" >> tmp.tmp
        fi
    else
        echo "$LINE" >> tmp.tmp
    fi
done 10< "$TRASH_LOG_PATH"

rm "$TRASH_LOG_PATH"
mv tmp.tmp "$TRASH_LOG_PATH"

