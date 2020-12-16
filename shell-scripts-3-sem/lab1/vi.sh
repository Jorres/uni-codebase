#!/bin/bash

WD=$(eval pwd)                               
HOME_DIR=~

if [[ $WD == $HOME_DIR ]]
then
    pwd
    exit 0
else
    echo "Try launching from $HOME_DIR instead of $WD"
    exit 1
fi
