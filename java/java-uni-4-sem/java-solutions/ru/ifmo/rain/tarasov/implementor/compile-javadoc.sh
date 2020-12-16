#!/bin/bash

MOD_NAME='ru.ifmo.rain.tarasov.implementor'

cd ../../../../../../
rm -rf modules
mkdir -p modules/ru.ifmo.rain.tarasov.implementor/ru/ifmo/rain/tarasov
cp -r java-solutions/ru/ifmo/rain/tarasov/implementor modules/ru.ifmo.rain.tarasov.implementor/ru/ifmo/rain/tarasov
cp java-solutions/module-info.java modules/ru.ifmo.rain.tarasov.implementor/module-info.java

PATH_TO_ROOT="../java-advanced-2020/"
REQUIRES="$PATH_TO_ROOT"'lib'
IMPLER_PATH="$PATH_TO_ROOT"'modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/'

rm -rf '_javadoc'

javadoc -d '_javadoc' \
-link 'https://docs.oracle.com/en/java/javase/11/docs/api/' -private \
-encoding UTF-8 -docencoding UTF-8 \
--module-path "$REQUIRES" \
--module-source-path "$PATH_TO_ROOT"'modules:modules' \
--module "$MOD_NAME" \
"$IMPLER_PATH"'Impler.java' \
"$IMPLER_PATH"'ImplerException.java' \
"$IMPLER_PATH"'JarImpler.java'

# google-chrome --new-window '/_javadoc/'"$MOD_NAME"'/ru/ifmo/rain/tarasov/implementor/package-summary.html'
