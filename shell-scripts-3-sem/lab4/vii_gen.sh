#!/bin/bash

# send user signals

./vii_worker.sh &

while read -r LINE; do
  if [[ "$LINE" == "+" ]]; then
    kill -USR1 $(cat "vii_pid.log")
  elif [[ "$LINE" == "*" ]]; then
    kill -USR2 $(cat "vii_pid.log")
  elif [[ "$LINE" == "TERM" ]]; then
    kill -SIGTERM $(cat "vii_pid.log")
  fi
done
