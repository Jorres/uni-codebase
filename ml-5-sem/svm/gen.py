import sys
import random
import os

for i in range(0, 1):
    was_m = False
    was_p = False
    f = open("input.txt", "w")

    n = random.randrange(90, 100)
    f.write(str(n) + '\n')

    for i in range(0, n):
        s = ""
        for j in range(0, n):
            s += str(random.randrange(1, 1000000000)) + " "

        cl = 0
        if not was_m:
            cl = 0
            was_m = True
        elif not was_p:
            cl = 1
            was_p = True
        else:
            cl = random.randrange(0, 2)
        if cl == 0:
            cl = -1
        s += str(cl)
        f.write(s + '\n')
    f.write('10\n')

    f.close()

    os.system('cat input.txt && ./exec')
