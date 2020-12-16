#!/bin/bash

# listen to sigterm

echo $$ > "vi_pid.log"
A=1
MODE="PRINT"
sigterm(){
    MODE="STOP"
}

trap 'sigterm' SIGTERM
while true; do
    if [[ "$MODE" == "PRINT" ]]; then
        A=$(($A+1))
        echo $A
    else
        echo "Stopped by SIGTERM"
        exit
    fi
    sleep 5
done
