#!/bin/bash

awk 'BEGIN { FS=":" } { print $3,$1 }' /etc/passwd | sort -nk 1
