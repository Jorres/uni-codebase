#!/bin/bash

FILENAME=$1

FULLPATH="$(pwd)/$FILENAME"
RMTRASH_EXEC="/home/jorres/work/prg/os/lab5/rmtrash.sh"

if [[ -d "$FULLPATH" ]]; then
    ls "$FULLPATH" |
    while read LINE; do
        "$RMTRASH_EXEC" "$LINE"
    done
else
    echo "I'm a test subject" >> "${FILENAME}"

    TRASH_PATH="/home/jorres/.trash"

    if [[ ! -d "$TRASH_PATH" ]]; then
        mkdir "$TRASH_PATH"
    fi

    NUM="1"

    ls "$TRASH_PATH" > tmp.tmp
    while read LINE; do
        NUM=$(("$NUM"+"$LINE"))
    done < tmp.tmp
    rm tmp.tmp

    ln "./$FILENAME" "$TRASH_PATH/$NUM"

    rm "$FILENAME"

    echo "$(pwd)/$FILENAME $NUM" >> "$TRASH_PATH/.trash.log"
    echo "$(pwd)/$FILENAME $NUM"
fi
