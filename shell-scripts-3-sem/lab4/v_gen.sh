#!/bin/bash

# read from stdin and pass to worker through file

rm data.txt
echo "" > data.txt

./v_worker.sh &

while true; do
  read -r LINE
  echo "$LINE" >> data.txt
done

