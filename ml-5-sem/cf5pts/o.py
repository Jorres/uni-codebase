k = int(input())
n = int(input())

sxs = [0] * k
qxs = [0] * k
nxs = [0] * k

for i in range(n):
    (x, y) = map(int, input().split())
    x -= 1
    sxs[x] += y
    qxs[x] += y ** 2
    nxs[x] += 1


ans = 0
for i in range(k):
    if (nxs[i] > 0):
        ans += nxs[i] * ((qxs[i] / nxs[i]) - (sxs[i] ** 2) / (nxs[i] ** 2))

print(ans / n)
