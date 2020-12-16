#!/bin/bash

MODULE='ru.ifmo.rain.tarasov.bank'

SCRIPT=`realpath $0`
SCRIPTPATH=`dirname $SCRIPT`
LIBPATH="$SCRIPTPATH"'/../../../../../../lib'

cd "$SCRIPTPATH"'/_build'

echo "$LIBPATH"

java -cp '.:'"$LIBPATH"'/*' "$MODULE"'.BankTests'


