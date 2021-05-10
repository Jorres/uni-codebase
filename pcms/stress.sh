#!/bin/bash

cd results

g++ -O3 -o generator ../generator.cpp
g++ -O3 -o answer ../answer.cpp
g++ -O3 -o exec ../e.cpp
# g++ -o checker ../checker.cpp

for ((i=1; i <= 100000; i++ ))
do
    ./generator 
    ./answer > ans.txt
    ./exec > my.txt
    if cmp -s ans.txt my.txt; then
        echo "res"
        cat ans.txt
    else
        echo "Not OK ""$i"
        break
    fi
done
