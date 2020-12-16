from sklearn import tree
import csv
import random
import math
import sys

import numpy as np

from matplotlib import pyplot as plt
from matplotlib import colors

WEAK_DEPTH = 5
BOOST_SIZE = 56
GRID_SIZE = 50

def read(src):
    reader = open(src, 'r')

    first = True
    raw = []
    for raw_row in reader:
        if first:
            first = False
            continue
        raw.append(raw_row)
    random.shuffle(raw)

    data = []
    ys = []
    for raw_row in raw:
        row = raw_row.split(',')
        ys.append(1 if row[len(row) - 1].strip() == 'P' else -1)
        data.append(row[:-1])

    return (data, ys)

def calc_error(predictions, W, ys):
    ans = 0
    for i in range(len(ys)):
        m = predictions[i] * ys[i]
        if (m < 0):
            ans += W[i]
    return ans

def get_pred(ansamble, bs, elem):
    ans = 0
    for (tree, b) in zip(ansamble, bs):
        ans += b * tree.predict([elem])[0]
    return ans

def scatter_dots(dots, ys, xmax, ymax):
    pxs = []
    pys = []
    nxs = []
    nys = []
    for (dot, y) in zip(dots, ys):
        if int(y) > 0:
            pys.append(float(dot[0]) / xmax * GRID_SIZE)
            pxs.append(float(dot[1]) / ymax * GRID_SIZE)
        else:
            nys.append(float(dot[0]) / xmax * GRID_SIZE)
            nxs.append(float(dot[1]) / ymax * GRID_SIZE)

    plt.scatter(pxs, pys, c = "green")
    plt.scatter(nxs, nys, c = "orange")

def visualize(ansamble, bs, num, train_dots, test_dots, train_ys, test_ys):
    raw_xs = [int(d[0]) for d in train_dots]
    raw_ys = [float(d[1]) for d in train_dots]


    xmax = max(raw_xs)
    ymax = max(raw_ys)
    data = [[get_pred(ansamble, bs, [i * xmax / GRID_SIZE, j * ymax / GRID_SIZE]) 
        for j in range(GRID_SIZE + 1)] 
            for i in range(GRID_SIZE + 1)]

    cmap = colors.ListedColormap(['red', 'blue'])
    bounds = [-100, 0, 100]
    
    fig, ax = plt.subplots()
    ax.imshow(data, cmap=cmap)
    scatter_dots(train_dots, train_ys, xmax, ymax)
    scatter_dots(test_dots, test_ys, xmax, ymax)

    plt.savefig("surface_" + str(num) + ".png")
        
def train_adaboost(data, ys, test_data, test_ys, boost_size):
    D = len(data)
    W = [1.0 / D] * D
    visualize_on_steps = [1, 2, 3, 5, 8, 13, 21, 34, 55]
    ansamble = []
    bs = []
    acc_xs = []
    acc_ys = []
    for i in range(boost_size):
        weak = tree.DecisionTreeClassifier(max_depth = WEAK_DEPTH)
        weak.fit(data, ys, sample_weight = W)
        ansamble.append(weak)
        predictions = weak.predict(data)

        error = calc_error(predictions, W, ys)
        b = 0.5 * math.log((1 - error) / error)

        bs.append(b)

        acc_xs.append(i)
        acc_ys.append(validate((ansamble, bs), test_data, test_ys))

        for j in range(len(ys)):
            W[j] = W[j] * math.exp(-b * ys[j] * predictions[j])

        norm = sum(W)
        W = [w / norm for w in W]
        if i in visualize_on_steps:
            visualize(ansamble, bs, i, data, test_data, ys, test_ys)

    return (ansamble, bs)

def validate(classifier, data, ys):
    (ansamble, bs) = classifier
    correct = 0
    total = 0

    for (elem, y) in zip(data, ys):
        total += 1
        ans = get_pred(ansamble, bs, elem);

        if ans * y > 0:
            correct += 1

    print("Accuracy", correct / total)
    return correct / total

def main():
    is_geyser = True
    src = "geyser.csv" if is_geyser else "chips.csv"
    (data, ys) = read(src)
    train_sz = int(len(data) * 0.8)

    train_set = data[:train_sz]
    train_ys = ys[:train_sz]

    test_set = data[train_sz:]
    test_ys = ys[train_sz:]

    train_adaboost(train_set, train_ys, test_set, test_ys, BOOST_SIZE)

main()
