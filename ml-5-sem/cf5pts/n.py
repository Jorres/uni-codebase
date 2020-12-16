k = int(input())
n = int(input())


def accumulate(arr):
    ans = [0]
    for elem in arr:
        ans.append(ans[-1] + elem)
    return ans


classes = [[] for _ in range(k)]
pointers = [0] * k

rxs = []
xs = []

for i in range(n):
    (x, y) = map(int, input().split())
    y -= 1
    xs.append((x, y))
    rxs.append(x)
    classes[y].append(x)

for cls in classes:
    cls.sort()
xs.sort()
rxs.sort()

cums = [accumulate(cls) for cls in classes]
big_cums = accumulate(rxs)

inner = 0
outer = 0

i = 0
for (x, y) in xs:
    curlen = len(classes[y])
    p = pointers[y]
    cum = cums[y]

    left_cls = p * x - cum[p]
    pointers[y] += 1
    inner += left_cls

    outer += i * x - big_cums[i] - left_cls

    i += 1

print(2 * inner)
print(2 * outer)
