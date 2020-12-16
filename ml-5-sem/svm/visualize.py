from matplotlib import pyplot as plt
import csv
import math

grid_size = 100

xs = []
ys = []
clrs = []
plots = []

is_geyser = False

def plot_dots():
    if is_geyser:
        f = open('geyser.csv', 'r')
    else:
        f = open('chips.csv', 'r')
    reader = csv.reader(f)
    first = True
    for row in reader:
        if not first:
            if is_geyser:
                xs.append(float(row[0]) / 23)
                ys.append(float(row[1]) / 6)
            else:
                xs.append(float(row[0]))
                ys.append(float(row[1]))
            cur_color = 'b' if row[2] == 'N' else 'r'
            clrs.append(cur_color)
        first = False

    plt.scatter(xs, ys, c=clrs)

def get_core(a, b):
        # return sum([x * y for (x, y) in zip(a, b)])
        # return (sum([x * y for (x, y) in zip(a, b)])) ** 2
        return math.exp(-5 * sum([(x - y) * (x - y) for (x, y) in zip(a, b)]))


def make_prediction(x, y, lambdas, w0):
    val = 0
    for i in range(0, len(plots)):
        val += lambdas[i] * plots[i][2] * get_core([plots[i][0], plots[i][1]], [x, y])
    return val - w0

def plot_contour(lambdasW):
    w0 = lambdasW[-1]
    lambdasW.pop()
    print(lambdasW, w0)

    if is_geyser:
        xs = [i / grid_size for i in range(0, grid_size)]
        ys = [i / grid_size for i in range(0, grid_size)]
    else:
        xs = [2 * i / grid_size -1  for i in range(0, grid_size)]
        ys = [2 * i / grid_size - 1 for i in range(0, grid_size)]


    z = [[make_prediction(xs[i], ys[j], lambdasW, w0) for i in range(0, grid_size)] for j in range(0, grid_size)]
    plt.contour(xs, ys, z, levels = [-1, 0, 1], colors = ['g', 'b', 'r'])
    plt.show()

def main():
    f = open('trained_model.txt', 'r')
    line = f.readline()
    lambdasW = [float(i) for i in line.split()]
    sz = int(f.readline())
    for _ in range(0, sz):
        [x, y, ans] = map(float, f.readline().split())
        plots.append((x, y, ans))
    f.close()

    plot_dots()
    plot_contour(lambdasW)

main()
