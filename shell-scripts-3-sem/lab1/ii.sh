#!/bin/bash

max=${1}

if (( $max < ${2} ))
then
    max=${2}
fi

if (( $max < ${3} ))
then
    max=${3}
fi

echo $max
