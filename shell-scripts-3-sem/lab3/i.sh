#!/bin/bash

# print PID's launched by user $1

USER=$1
ps u | awk -v user="$USER" '{
  if ($1 == user) {
    print $2,":",$11
  }
}'
