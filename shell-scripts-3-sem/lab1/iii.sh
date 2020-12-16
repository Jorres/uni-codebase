#!/bin/bash

RESULT=""

while true
do
    read -r A
    if [[ "$A" == "q" ]]
    then
        break
    fi

    RESULT=$RESULT$A
done

echo $RESULT
