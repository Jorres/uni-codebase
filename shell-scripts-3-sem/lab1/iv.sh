#!/bin/bash

COUNT=0

while true
do
    read A
    if (( $A % 2 == 0 ))
    then
        break
    fi

    COUNT=$(( $COUNT + 1 ))
done

echo $COUNT
