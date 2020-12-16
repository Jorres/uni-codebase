#!/bin/bash

sudo grep -r -l '#!' /bin |

while read LINE
do
    cat $LINE | head -1 | grep -o -E '^#!.*'
done |

sort -n -k 1 | uniq -c | sort -rn | head -1 | awk 'BEGIN  { FS="!" } { print $2 }'
