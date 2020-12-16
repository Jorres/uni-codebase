n, m, k = map(int, (input().split()))
ans = [[] for i in range(k)]

data = list(map(int, (input().split())))
final_data = []
for i in range(len(data)):
    final_data.append((data[i], i))
final_data.sort()

curlist = 0
for i in range(len(final_data)):
    ans[curlist].append(final_data[i][1] + 1)
    curlist = (curlist + 1) % k

for arr in ans:
    print(len(arr), end=' ')
    for elem in arr:
        print(elem, end=' ')
    print()
