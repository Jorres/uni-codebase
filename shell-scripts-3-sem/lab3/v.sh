#!/bin/bash

# print PID-PPID-Average sleeping time values

function get_field {
  FILE_NAME=$1
  cat $FILE_NAME | awk -v req_name=$2 sec_field=$3' {
    if ($1 == req_name) {
      print $1,$sec_field
    }
  }'
}

ps -la | 
tail -n +2 |
awk '{
  print $4
}' |
while read PROCESS_PID; do
    if [[ -d /proc/$PROCESS_PID ]]; then
      a=$(get_field /proc/$PROCESS_PID/status "Pid:" 2)
      b=$(get_field /proc/$PROCESS_PID/status "PPid:" 2)
      c=$(get_field /proc/$PROCESS_PID/sched "se.sum_exec_runtime" 3)
      echo "$a $b $c"
    fi
done | sort -nk 4 > runtime_info.log
