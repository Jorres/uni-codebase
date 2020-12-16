#!/bin/bash

MODULE='ru.ifmo.rain.tarasov.bank'
JUNIT_CONSOLE='junit-platform-console-standalone-1.6.2.jar'

SCRIPT=`realpath $0`
SCRIPTPATH=`dirname $SCRIPT`
LIBPATH="$SCRIPTPATH"'/../../../../../../lib'

cd "$SCRIPTPATH"'/_build'

java -jar "$LIBPATH"'/'"$JUNIT_CONSOLE" \
    -cp "." \
    -c "$MODULE."BankTests \
    --fail-if-no-tests
