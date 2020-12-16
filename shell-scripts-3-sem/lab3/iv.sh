#!/bin/bash

# find process most using residual memory

top -bn 1 | tail -n +8 | awk '{
  print $1,$7
}' | sort -nk 2 | tail -1 | awk '{
  print $1,$2
}'
