#!/bin/bash

# operations under values from file

NUM="1"
CURNUM="NaN"
MODE="ADD"

(tail -n 0 -f data.txt) |
  while read -r LINE; do
    if [[ "$LINE" == "QUIT" ]]; then
        echo "exit"
        killall tail
        exit
    elif [[ "$LINE" == "+" ]]; then
        MODE="ADD"
        echo "set addition mode"
    elif [[ "$LINE" == "*" ]]; then
        MODE="MULT"
        echo "set multiplication mode"
    elif [[ "$LINE" -eq "$LINE" ]]; then
        CURNUM=$LINE
    else
        echo "non valid argument"
        killall tail
        exit
    fi

    if [[ "NaN" -ne "$CURNUM" ]]; then
      if [[ "$MODE" == "ADD" ]]; then
        NUM=$(($NUM+$CURNUM))
        echo "result $NUM"
        CURNUM="NaN"
      fi

      if [[ "$MODE" == "MULT" ]]; then
        NUM=$(($NUM*$CURNUM))
        echo "result $NUM"
        CURNUM="NaN"
      fi
    fi
  done
