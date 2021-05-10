import random
import math

def get_start(base, power, n):
    result = 1
    while power > 0:
        if power % 2 != 0:
            power = power - 1
            result = result * base % n

        power = power // 2
        base = base * base % n
    return result

def simple_prime(arg):
    for i in range(2, arg):
        if i * i > arg:
            break
        if arg % i == 0:
            return False

    return True

def prime(arg):
    if (arg < 10000):
        return simple_prime(arg)

    prev = arg - 1
    s = 0
    while prev % 2 == 0:
        prev = prev // 2
        s += 1
    t = prev

    for _ in range(20): # TEST_ITERATIONS
        a = random.randrange(2, arg - 1)

        x = get_start(a, t, arg)

        if x == 1 or x == arg - 1:
            continue

        not_this_time = False
        for _ in range(s - 1):
            x = x * x % arg
            if x == 1:
                return False
            if x == arg - 1:
                not_this_time = True
                break
        if not not_this_time:
            return False
    return True

n = int(input())

for _ in range(n):
    x = int(input())
    if x == 2:
        print("YES")
    elif x == 1 or x % 2 == 0:
        print("NO")
    else:
        if prime(x):
            print("YES")
        else:
            print("NO")
