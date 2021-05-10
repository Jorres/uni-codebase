#!/bin/bash

g++ -O3 -o generator generator.cpp
g++ -O3 -o exec g.cpp

for ((i=1; i <= 100000; i++ ))
do
    ./generator > input.txt
    ./answer < input.txt > ans.txt
    ./exec < input.txt > my.txt
    if cmp -s ans.txt my.txt; then
        echo "res"
        cat ans.txt
    else
        echo "Not OK ""$i"
        break
    fi
done
