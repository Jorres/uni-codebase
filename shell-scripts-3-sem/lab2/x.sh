#!/bin/bash

LINE_NUM=1

while read -r STR; do
    if [[ ${STR:0:2} == "#!" ]]; then
        if [ "$LINE_NUM" -eq "1" ]; then
            echo $STR
        fi
    else 
        if [[ "${STR:0:1}" == "#" ]]; then
            continue
        else 
            echo $STR
        fi
    fi

    LINE_NUM=$(($LINE_NUM+1))
done < $1
