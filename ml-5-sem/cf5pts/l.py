import math

n = int(input())

xs = []
ys = []

for _ in range(n):
    (x, y) = map(int, input().split())
    xs.append(x)
    ys.append(y)

xm = sum(xs) / len(xs)
ym = sum(ys) / len(ys)

nom = sum([(x - xm) * (y - ym) for (x, y) in zip(xs, ys)])
fst = sum([(x - xm) ** 2 for x in xs])
snd = sum([(y - ym) ** 2 for y in ys])
dnm = math.sqrt(fst * snd)

if dnm == 0:
    print(0)
else:
    print(nom / dnm)

