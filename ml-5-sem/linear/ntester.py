import numpy
import math
from matplotlib import pyplot as plt
import random

def read_matrix(n, m, f):
    xs = []
    ys = []
    for _ in range(n):
        x = list(map(float, f.readline().strip().split()))
        ys.append([x[-1]])
        x[-1] = 1
        xs.append(x)
    return numpy.array(xs), numpy.array(ys)

def do_show(ans, xs, ys):
    n = len(xs)
    m = len(xs[0])
    ys1 = xs.dot(ans)
    ys_diff = ys1 - ys
    smape = sum([abs(y1 - y) / (abs(y1) + abs(y)) for [y1], [y] in zip(ys1, ys)]) / n
    print(smape)
    p_w = numpy.array([[random.randint(-100, 100) / 100] for i in range(m)])
    #plot_x = list(xs.dot(p_w)) * 2
    #plot_y = list(ys) + list(ys1)
    #plot_c = ['g'] * n + ['b'] * n
    #plot_x = [abs(y) for [y] in ys]
    #plot_y = [y1 if y > 0 else -y1 for [y1], [y] in zip(ys1, ys)]
    plot_x = [y for [y] in ys]
    plot_y = [y1 for [y1], [y] in zip(ys1, ys)]
    print(ans[-1])
    print(ans[0])
    
    plot_c = ['b'] * n
    plt.scatter(plot_x, plot_y, c=plot_c, s=10, edgecolors=plot_c)
    plt.plot([-100000, 100000], [-100000, 100000])
    plt.plot([-100000, 100000], [0, 0])
    plt.plot([0, 0], [-100000, 100000])
    plt.plot([-75000, 75000], [-150000, 150000]) 
    plt.plot([-100000, 100000], [-50000, 50000])
    plt.show()


ws = list(map(float, input().split()))
ans = numpy.array([[w] for w in ws])

with open('./LR/0.52_0.70.txt') as f:
    m = int(f.readline().strip())
    n_train = int(f.readline().strip())
    xs_train, ys_train = read_matrix(n_train, m, f)
    n_test = int(f.readline().strip())
    xs_test, ys_test = read_matrix(n_test, m, f)

do_show(ans, xs_train, ys_train)
do_show(ans, xs_test, ys_test)
