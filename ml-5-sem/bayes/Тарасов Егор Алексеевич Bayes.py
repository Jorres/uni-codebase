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

# real solver

import os
import math

from matplotlib import pyplot as plt

GRAM_PARAMETER = 1
FOLDS_VALIDATE = 10

ALPHA = 0.01

tp = 0
tn = 0
fp = 0
fn = 0
spamcnt = 0

ratios = []
marks = []

def handle_metrics(prediction, real):
    global tp, tn, fp, fn
    if prediction == real:
        if prediction == 0:
            tp += 1
        else:
            tn += 1
    else:
        if prediction == 0:
            fp += 1
        else:
            fn += 1

def read_words_from(path, filename):
    f = open(os.sep.join(path + [filename]), "r")
    ans = list(map(int, f.readline().split()))
    f.close()
    return ans

def iterate_met(i, allwords, met, ids, which, part, data):
    for [words, tp] in data[i]:
        met.append(set())
        part[tp] += 1
        which.append(tp)
        for word in words:
            if not word in allwords:
                ids[word] = len(allwords)
                allwords.add(word)
            met[-1].add(ids[word])

def iterate_count(i, ids, msg_id_a, count, count_neg, met, which, data):
    print('it count')
    for _ in data[i]:
        msg_id = msg_id_a[0]
        w = which[msg_id]
        for word in ids:
            i = ids[word]
            if i in met[msg_id]:
                count[i][w] += 1
            else:
                count_neg[i][w] += 1
        msg_id_a[0] += 1
        
def validate(test_num, ids, count, count_neg, lambdas, part, data):
    global spamcnt
    print('validate')
    n = sum(part)
    preproc = [0, 0]
    for word in ids:
        word_id = ids[word]
        for j in range(2):
            pos = count[word_id][j]
            neg = count_neg[word_id][j]
            preproc[j] += math.log(neg + ALPHA) - math.log(pos + neg + 2 * ALPHA)
         
    for [words, tp] in data[test_num]:
        message = set()
        for word in words:
            if word in ids:
                message.add(ids[word])

        noms = [0, 0]
        for j in range(2):
            # assuming in lambda is a log already
            noms[j] = lambdas[j] + math.log(part[j] / n) + preproc[j]
            for word_id in message:
                pos = count[word_id][j]
                neg = count_neg[word_id][j]
                noms[j] += math.log(pos + ALPHA)
                noms[j] -= math.log(neg + ALPHA)
        
        ratio = noms[0] - noms[1]
        ratios.append(ratio)
        prediction = 0 if ratio > 0 else 1
        handle_metrics(prediction, tp)
        marks.append(tp == 0)

        # if prediction == 0 and tp == 1:
        #     print("not spam as spam", spamcnt)
        #     spamcnt += 1

def cross_val(test_num, lambdas, data):
    allwords = set()
    met = []
    ids = dict()
    which = []
    part = [0, 0] # SPAM LEGIT

    for i in range(0, FOLDS_VALIDATE):
        if i != test_num:
            iterate_met(i, allwords, met, ids, which, part, data)

    count     = [[0 for _ in range(2)] for _ in range(len(ids))]
    count_neg = [[0 for _ in range(2)] for _ in range(len(ids))]

    msg_id_a = [0]
    for i in range(0, FOLDS_VALIDATE):
        if i != test_num:
            iterate_count(i, ids, msg_id_a, count, count_neg, met, which, data)

    validate(test_num, ids, count, count_neg, lambdas, part, data) 

def get_data():
    ans = [[] for _ in range(FOLDS_VALIDATE)]
    for i in range(0, FOLDS_VALIDATE):
        part_path = "./messages_" + str(GRAM_PARAMETER) + "/part" + str(i + 1)
        for root, dirs, files in os.walk(part_path):
            path = root.split(os.sep)
            for filename in files:
                ans[i].append([read_words_from(path, filename), 
                              1 if "legit" in filename else 0])
    return ans

roc_curves = []
accuracies = []
def visualize(data):
    global roc_curves, accuracies
    ratios = [a[0] for a in data]
    marks  = [a[1] for a in data]

    tp = 0
    fp = 0
    tn = 0
    fn = 0

    last_false = len(data) - 1
    while ratios[last_false] > 0 and not marks[last_false]:
        last_false -= 1

    first_true = 0
    while ratios[first_true] < 0 and marks[first_true]:
        first_true += 1

    tn = first_true
    # True if spam
    # ratio neg if not spam
    for i in range(first_true, len(data)):
        if marks[i]:
            tp += 1
        if not marks[i]:
            fp += 1

    lambdas = []
    specs = []
    sens = []
    accuracy = []

    for i in range(first_true, last_false + 1):
        # change what's in i    
        accuracy.append((tp + tn) / (tp + tn + fp + fn))
        lambdas.append(ratios[i])
        sens.append(fn / (tp + fn))
        specs.append(tn / (tn + fp))
        if marks[i]:
            fn += 1
            tp -= 1
        else:
            tn += 1
            fp -= 1


    roc_curves.append((sens, specs))
    accuracies.append((lambdas, accuracy))

labels = []
def show():
    i = 0
    for (x, y) in roc_curves:
        plt.plot(x, y, label="gram = {}, alpha = {}".format(labels[i][0], labels[i][1]))
        i += 1
    plt.legend()
    plt.show()
    i = 0
    for (x, y) in accuracies:
        plt.plot(x, y, label="gram = {}, alpha = {}".format(labels[i][0], labels[i][1]))
        i += 1
    plt.legend()
    plt.show()

def main():
    global ALPHA, GRAM_PARAMETER
    alphas = [0.01, 0.5]
    grams = [1, 2]

    for alpha in alphas:
        for gram in grams:
            GRAM_PARAMETER = gram
            ALPHA = alpha
            labels.append((gram, alpha))

            lambdas = [0, 0]
            src = get_data()
            for i in range(0, FOLDS_VALIDATE):
                print(i)
                cross_val(i, lambdas, src)
            print("Accuracy is: ", (tp + tn) / (tp + tn + fp + fn))

            data = list(zip(ratios, marks))
            data.sort()

            visualize(data)
    show()

main()
