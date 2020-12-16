#!/bin/bash

man bash | awk ' { for (i=1;i<=NF;i++) {if (length($i)>3) print $i} }' | sort | uniq -c | sort -r -nk 1 | head -3


