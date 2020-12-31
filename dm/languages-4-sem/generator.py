#!/usr/bin/env python3

n = int(input("Enter a number:"))

def escape_prev(s):
    ans = "" + s[0]
    for pos in range(1, len(s)):
        if s[pos] == '\n':
            continue
        if s[pos] == '"':
            ans += '\\\"'
        elif s[pos] == '\\':
            ans += '\\\\'
        else:
            ans += s[pos]
    return ans

cur_prog = "main() {print(\"\");}"

for num in range(1, n):
    cur_prog = "main() { print(\"" + escape_prev(cur_prog) + "\"); }"

print(cur_prog)

f = open(str(n) + ".dart", "w")
f.write(cur_prog)
