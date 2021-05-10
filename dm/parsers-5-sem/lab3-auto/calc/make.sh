#!/bin/bash
flex calc.lex
bison -d calc.y
g++ calc.tab.c lex.yy.c 

