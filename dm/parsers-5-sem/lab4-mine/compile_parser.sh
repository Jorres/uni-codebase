#!/bin/bash

if [ "$1" = "CALC" ] || [ "$1" = "KOTLIN" ]; then
    cd code
    g++ -std=c++1z -g -o exec"$1" -Wall \
        lexer.cpp \
        parser.cpp \
        nodes.cpp \
        parse_exception.cpp
    cd ..
    echo "Starting $1 executable..."
    ./code/exec"$1"
else
    echo 'usage: ./compile_parser.sh [CALC|KOTLIN]'
fi

