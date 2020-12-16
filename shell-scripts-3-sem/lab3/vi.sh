#!/bin/bash

# calculate average sleeping time grouped by PPID

LINE_NUM=0
PREV_PPID=0
RUNTIME_SUM=0
PROCESS_UNDER_SAME_PPID=0

while read LINE; do
  CUR_PPID=$(echo "$LINE" | awk '{
    print $4
  }')
  CUR_RUNTIME=$(echo "$LINE" | awk '{
    print $6
  }')

  if [[ $LINE_NUM > 0 ]] && [[ $PREV_PPID != $CUR_PPID ]]; then
    echo "Average exec runtime  = " \
          $( echo "scale=9;$RUNTIME_SUM/$PROCESS_UNDER_SAME_PPID" | bc)
    RUNTIME_SUM=$CUR_RUNTIME
    PROCESS_UNDER_SAME_PPID=1
  else
    PROCESS_UNDER_SAME_PPID=$(($PROCESS_UNDER_SAME_PPID+1)) 
    RUNTIME_SUM=$(echo "scale=9;$RUNTIME_SUM+$CUR_RUNTIME" | bc)
  fi

  LINE_NUM=$(($LINE_NUM+1))
  PREV_PPID=$CUR_PPID
  echo $LINE
done < runtime_info.log > runtime_info_stat.log

 echo "Average exec runtime  = " \
   $( echo "scale=9;$RUNTIME_SUM/$PROCESS_UNDER_SAME_PPID" | bc) >> runtime_info_stat.log
