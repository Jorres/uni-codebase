#!/bin/bash

# attempt to create test directory

mkdir ~/test &&
    echo "catalog test created successfully" >> ~/report &&
    {
        NAME=$(echo $(date) | awk '
        BEGIN {
            OFS="-"
        }
        {
            $1=$1
            print $0
        }')
        echo "$NAME"
    } &&
    echo "" >> ~/test/$NAME

ping www.net_nikogo.ru || echo "Server ping failed" >> ~/report
