#!/bin/bash

if [ "$1" = "CALC" ] || [ "$1" = "KOTLIN" ]; then
    python3 generate_lexer.py "$1"
    python3 generate_parser.py "$1"

    cd code
    clang-format -i *.cpp *.h 
    g++ -std=c++1z -g -o exec"$1" -Wall \
        lexer.cpp \
        parser.cpp \
        nodes.cpp \
        parse_exception.cpp
    cd ..
    ./code/exec"$1"
else
    echo 'usage: ./assemble_parser.sh [CALC|KOTLIN]'
fi

