#!/bin/bash

XORG_PATH=$(sudo find ~ -name "Xorg.0.log")

awk '/WW/ { gsub("WW","Warning:");     print}
     /II/ { gsub("II","Information:"); print}' $XORG_PATH | sort -k 3 > full.log

cat full.log


