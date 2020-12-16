#!/bin/bash

cd ../../../../../../
rm -rf _build
rm -rf modules
mkdir -p _build
cd ./_build
mkdir -p ../modules/ru.ifmo.rain.tarasov.implementor/ru/ifmo/rain/tarasov
cp -r ../java-solutions/ru/ifmo/rain/tarasov/implementor ../modules/ru.ifmo.rain.tarasov.implementor/ru/ifmo/rain/tarasov
cp ../java-solutions/module-info.java ../modules/ru.ifmo.rain.tarasov.implementor/module-info.java

javac -d '.' \
--module-path '../../java-advanced-2020/lib' \
--module-source-path '../../java-advanced-2020/modules:../modules' \
--module 'ru.ifmo.rain.tarasov.implementor,info.kgeorgiy.java.advanced.implementor'

jar cf info.kgeorgiy.java.advanced.implementor.jar -C info.kgeorgiy.java.advanced.implementor .

touch 'Manifest.txt'

echo 'Main-Class: ru.ifmo.rain.tarasov.implementor.JarImplementor
Class-Path: info.kgeorgiy.java.advanced.implementor.jar
' > 'Manifest.txt'

jar cfm ru.ifmo.rain.tarasov.implementor.jar 'Manifest.txt' -C ru.ifmo.rain.tarasov.implementor .

rm 'Manifest.txt'
















# #location is /custom_out/ru...implementor/
#
#MOD_NAME='ru.ifmo.rain.tarasov.implementor'
#KGEO_MOD_NAME='info.kgeorgiy.java.advanced.implementor'
#WORKING_DIR='/home/jorres/work/java/advanced/java-3228/custom_out/'"$MOD_NAME"
#KGEO_DIR='/home/jorres/work/java/advanced/java-3228/custom_out/'"$KGEO_MOD_NAME"'/info/kgeorgiy/java/advanced/implementor'
#
#touch "$WORKING_DIR"'/Manifest.txt'
#
#echo 'Main-Class: ru.ifmo.rain.tarasov.implementor.JarImplementor
#' > "$WORKING_DIR"'/Manifest.txt'
#
#TARGET_DIR="$WORKING_DIR"'/info/kgeorgiy/java/advanced/implementor'
#
#mkdir -p "$TARGET_DIR"
#
#cp "$KGEO_DIR"'/Impler.class' "$TARGET_DIR"
#cp "$KGEO_DIR"'/ImplerException.class' "$TARGET_DIR"
#cp "$KGEO_DIR"'/JarImpler.class' "$TARGET_DIR"
#
#jar cfm ru.ifmo.rain.tarasov.implementor.jar "$WORKING_DIR"'/Manifest.txt' \
#        ru/ifmo/rain/tarasov/implementor/*.class \
#        info/kgeorgiy/java/advanced/implementor/*.class
#
#rm -rf "$WORKING_DIR"'/info'
#
#rm "$WORKING_DIR"'/Manifest.txt'

