import sys
import os
import random

letters = ['A', 'B', 'C']
def generate(depth):
    if (depth == 0):
        return letters[random.randint(0, 2)]

    op = random.randint(0, 3)

    left = '(' + generate(depth - 1) + ')'
    right = '(' + generate(depth - 1) + ')'

    if op == 0:
        return left + ' -> ' + right
    elif op == 1:
        return left + ' & ' + right
    elif op == 2:
        return left + ' | ' + right
    else:
        return '!(' + left + ')'

    


tasks = []
tries = 10000
depth = 5
for _ in range(tries):
    task = generate(depth)
    tasks.append(task)

for i in range(tries):
    f = open("input.txt", "w")
    f.write(tasks[i])
    f.close()

    os.system("./a > out.txt")
    lines_proof = open("out.txt").readlines()
    print(tasks[i])
    if lines_proof[0].startswith(":("):
        print(":( ans")
        continue

    os.system("./tolya < out.txt > checker_out.txt")
    lines = open("checker_out.txt").readlines()

    error = False
    for line in lines:
        if "incorrect" in line:
            error = True
            break
    print(lines[-1])
    if error:
        print("not OK")
        break
    else:
        print("OK")
    

