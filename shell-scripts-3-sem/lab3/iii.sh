#!/bin/bash

# print processes launched by other processes from /usr/bin

for i in {1..10000}; do
    if [[ -d "/proc/$i" ]]; then
        DIR=$(readlink /proc/$i/exe | grep "^/usr/bin/")
        if [[ -n "$DIR" ]]; then
          echo "/proc/$i" | awk -F "/" '{print $3}'
        fi
    fi
done

