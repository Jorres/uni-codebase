#!/bin/bash

# calculate based on user signals

echo $$ > "vii_pid.log"
A=1
usr1(){
    A=$(($A+2))
    echo "$A"
}
usr2(){
    A=$(($A*2))
    echo "$A"
}
sigterm(){
    echo "Stopped by SIGTERM"
    exit
}

trap 'usr1' USR1
trap 'usr2' USR2
trap 'sigterm' SIGTERM

while true; do
    sleep 1
done
