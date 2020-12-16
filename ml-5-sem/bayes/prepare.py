import os
import sys

from pathlib import Path

GRAM_PARAMETER = 1

def gramify(lst):
    ans = []
    for i in range(0, len(lst) - GRAM_PARAMETER):
        local_s = ""
        for j in range(0, GRAM_PARAMETER):
            local_s += str(lst[i + j])
        ans.append(int(local_s))
    return ans

def encode(lines):
    ans = []
    [subj, empty, main] = lines
    nosubj = subj[len("Subject: "):]

    subj_words = list(map(int, nosubj.split()))
    subj_words = [x * 10000 for x in subj_words]
    subj_words = gramify(subj_words)
    main_words = gramify(list(map(int, main.split())))

    subj_words = list(map(str, subj_words))
    main_words = list(map(str, main_words))
    return " ".join(subj_words + main_words)

for root, dirs, files in os.walk("./messages"):
    path = root.split(os.sep)
    for filename in files:
        if "swp" in filename:
            continue
        write_path = os.sep.join(['.', 'messages_' + str(GRAM_PARAMETER), path[2]])
        write_file = os.sep.join([write_path, filename])
        Path(write_path).mkdir(parents=True, exist_ok=True)

        print(write_file)

        print(os.sep.join(path + [filename]))
        f = open(os.sep.join(path + [filename]), "r")
        lines = f.readlines()
        assert len(lines) == 3

        to = open(write_file, "w+")
        # tmp = encode(lines)
        # print(tmp)
        # sys.exit()
        to.write(encode(lines))
        to.close()

        f.close()
