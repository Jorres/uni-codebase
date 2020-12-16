#!/bin/bash

# schedule execution of script 1

for i in {0..11}; do
    for j in {0..11}; do
        MIN=$((j*5))
        if [[ "$MIN" -eq "5" ]] || [[ "$MIN" -eq "0" ]]; then
            MIN=0$MIN
        fi
        at $i:$MIN AM Fri -f ./i.sh
        # at $i:$MIN PM Fri -f ./i.sh
    done
done

# for i in `atq | awk '{print $1}'`;do atrm $i;done
