#!/bin/bash

for (( i=1; i <= 1000000; i++ ))
do
	cp /dev/null input.in
	cp /dev/null output_correct.out
	cp /dev/null output_exec.out
	
	./generator > input.in
	./correct < input.in > output_correct.out
	./exec < input.in > output_exec.out

	echo $i
	if diff output_correct.out output_exec.out
	then
		echo OK
	else
		echo NOT OK
		break
	fi
done