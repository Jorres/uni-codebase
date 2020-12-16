#!/bin/bash

# print PID of the last launched process

ps u --sort=start_time | tail -1 | awk '{
  print $2
}'
