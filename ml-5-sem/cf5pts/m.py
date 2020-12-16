import math

n = int(input())

xs = []
ys = []
rxs = []
rys = []

for i in range(n):
    (x, y) = map(int, input().split())
    rxs.append(x)
    rys.append(y)
    xs.append(x)
    ys.append(y)

xs.sort()
ys.sort()

xtoi = dict()
ytoi = dict()

for i in range(len(xs)):
    xtoi[xs[i]] = i
    ytoi[ys[i]] = i

R = 0
for (x, y) in zip(rxs, rys):
    R += (xtoi[x] - ytoi[y]) ** 2

print(1 - 6 / (n * (n - 1) * (n + 1)) * R)
