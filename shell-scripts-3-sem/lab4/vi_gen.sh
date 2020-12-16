#!/bin/bash

# send sigterm

./vi_worker.sh &

while true; do
  read -r LINE
  if [[ "$LINE" == "SIGTERM" ]]; then
    kill -SIGTERM $(cat "vi_pid.log")
  fi
done
