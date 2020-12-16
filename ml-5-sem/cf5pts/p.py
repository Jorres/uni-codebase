from collections import defaultdict

(k1, k2) = map(int, input().split())

seen = [0] * k1
seeny = [0] * k2

n = int(input())

def zero():
    return 0

def bs():
    return defaultdict(zero)

table = defaultdict(bs)

for i in range(n):
    (x1, x2) = map(int, input().split())
    x1 -= 1
    x2 -= 1

    table[x1][x2] += 1
    seen[x1] += 1
    seeny[x2] += 1

ans = 0

for i in table:
    for j in table[i]:
        curexp = seen[i] * (seeny[j]) / n
        ans += (table[i][j] - curexp) ** 2 / curexp
        ans -= curexp

ans += n

print(ans)
