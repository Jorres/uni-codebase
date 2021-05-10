#!/bin/bash
rm assembly/lex.yy.c
rm assembly/calc.tab.c
rm assembly/calc.tab.h
rm assembly/tests.h
rm assembly/tests.cpp

cp tests.cpp assembly/
cp tests.h assembly/

flex calc.lex
bison -d calc.y

mv lex.yy.c assembly/
mv calc.tab.h assembly/
mv calc.tab.c assembly/

cd assembly/

g++ -Wwrite-strings -o exec calc.tab.c lex.yy.c tests.cpp
./exec

cd ..

