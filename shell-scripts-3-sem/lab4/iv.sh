#!/bin/bash

# restrict cpu for 2 processes

# nice -n 20 taskset -c 1 ./everlasting_calc.sh
# sudo nice -n -20 taskset -c 1 ./everlasting_calc.sh

./everlasting_calc.sh &
./everlasting_calc.sh &

CALC_PID=$(pgrep -f "everlasting_calc.sh")

FIRST=$(echo $CALC_PID | awk '{
    print $1
}')

SECOND=$(echo $CALC_PID | awk '{
    print $2
}')

cpulimit -l 10 --pid $FIRST &
# cpulimit -l 10 --pid $SECOND &

