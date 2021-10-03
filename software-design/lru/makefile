all:
	g++ -Wall -fmax-errors=5 -fsanitize=address,undefined -O0 -g -o exec_gdb spec.cpp
	g++ -Wall -fsanitize=address,undefined -o exec spec.cpp
