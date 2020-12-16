#!/bin/bash

MODULE='ru.ifmo.rain.tarasov.bank'
MODULE_SLASHES='ru/ifmo/rain/tarasov/bank'

SCRIPT=`realpath $0`
SCRIPTPATH=`dirname $SCRIPT`
LIBPATH="$SCRIPTPATH"'/../../../../../../lib'

cd $SCRIPTPATH

mkdir -p _build

javac -cp ".:""$LIBPATH""/*" -d _build *.java
