import random
import collections
from sklearn import tree

from matplotlib import pyplot as plt

BOOTSTRAP_SIZE = 2000
BUILD_SINGLE = True
FOREST_SIZE = 10

def check(test, test_ys, clf, check_prediction):
    correct = 0
    total = 0
    for [obj, mark] in zip(test, test_ys):
        if check_prediction(obj, mark, clf):
            correct += 1
        total += 1
    return [correct, total]

def bootstrap(dataset, ys):
    ans =  ([], [])
    for _ in range(BOOTSTRAP_SIZE):
        num = random.randint(0, len(dataset) - 1)
        ans[0].append(dataset[num])
        ans[1].append(ys[num])
    return ans

def build_random_forest(train, train_ys):
    classifiers = []
    for i in range(FOREST_SIZE):
        [data, ys] = bootstrap(train, train_ys)
        clf = tree.DecisionTreeClassifier()
        clf.fit(data, ys)
        classifiers.append(clf)
    return classifiers

def read_one(filename):
    f = open(filename, "r")
    [_, _] = list(map(int, f.readline().split()))
    sz = int(f.readline())
    X = []
    Y = []
    for j in range(0, sz):
        obj = list(map(int, f.readline().split()))
        Y.append(obj[-1])
        X.append(obj[:-1])
    return [X, Y]

def read_sets(i):
    pref = str(i)
    if len(pref) == 1:
        pref = '0' + pref

    [train, train_ys] = read_one('data/' + pref + "_train.txt")
    [test, test_ys]   = read_one('data/' + pref + "_test.txt")

    return [train, train_ys, test, test_ys]

def check_single(obj, mark, clf):
    return mark == clf.predict([obj])

def zero():
    return 0

def check_forest(obj, mark, forest):
    marks = collections.defaultdict(zero)
    for clf in forest:
        marks[clf.predict([obj])[0]] += 1

    max_mark = -1
    for key in marks:
        if (max_mark == -1 or marks[key] > marks[max_mark]):
            max_mark = key
    return mark == max_mark

min_height = ([],[])
max_height = ([],[])

def visualize():
    plt.xlabel("$Height$")
    plt.ylabel("$Accuracy$")
    plt.plot(min_height[0], min_height[1], '-o')
    plt.plot(max_height[0], max_height[1], '-x')
    plt.show()

def main():
    min_height_ds = 1
    max_height_ds = 21
    for i in range(1, 22): # 22
        if i == min_height_ds or i == max_height_ds:
            [train, train_ys, test, test_ys] = read_sets(i)

            if BUILD_SINGLE:
                best_accuracy = -1
                best_h = -1
                for h in range(1, 20):
                    clf = tree.DecisionTreeClassifier(max_depth = h, criterion = "gini", splitter = "best")
                    clf = clf.fit(train, train_ys)
                    [correct, total] = check(test, test_ys, clf, check_single)
                    accuracy = correct / total
                    if accuracy > best_accuracy:
                        best_accuracy = accuracy
                        best_h = h

                    if i == min_height_ds:
                        min_height[0].append(h)
                        min_height[1].append(accuracy)
                    if i == max_height_ds:
                        max_height[0].append(h)
                        max_height[1].append(accuracy)

                print("Best height: ", best_h, ', best accuracy: ', best_accuracy)
            else:
                classifiers = build_random_forest(train, train_ys)
                [correct, total] = check(test, test_ys, classifiers, check_forest)
                print('Forest accuracy is ', correct / total)

    visualize()
main()
