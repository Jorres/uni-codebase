import math
import sys

def _uniform(arg):
    return 1 / 2

def _triangular(arg):
    return 1 - abs(arg)

def _epanechnikov(arg):
    return 3 / 4 * (1 - arg * arg)

def _quartic(arg):
    return 15 / 16 * ((1 - arg * arg) ** 2)

def _triweight(arg):
    return 35 / 32 * ((1 - arg * arg) ** 3)

def _tricube(arg):
    return 70 / 81 * ((1 - abs(arg) ** 3) ** 3)

def _cosine(arg):
    return math.pi / 4 * math.cos(math.pi / 2 * arg)

def basic(f):
    return lambda arg : f(arg) if -1 < arg < 1 else 0

def gaussian(arg):
    return 1 / (math.sqrt(2 * math.pi)) * math.exp((-1) / 2 * arg * arg)

def logistic(arg):
    return 1 / (math.exp(arg) + 2 + math.exp(-arg))

def sigmoid(arg):
    return 2 / math.pi / (math.exp(arg) + math.exp(-arg))

str_to_fun = {
    "uniform": basic(_uniform),
    "triangular": basic(_triangular),
    "epanechnikov": basic(_epanechnikov),
    "quartic": basic(_quartic),
    "triweight": basic(_triweight),
    "tricube": basic(_tricube),
    "gaussian": gaussian,
    "cosine": basic(_cosine),
    "logistic": logistic,
    "sigmoid": sigmoid
}

def detect(s):
    return str_to_fun[s]

def manhattan(a, b):
    return sum([abs(x - y) for x, y in zip(a, b)])

def euclidean(a, b):
    return math.sqrt(sum([(x - y) * (x - y) for x, y in zip(a, b)]))

def chebyshev(a, b):
    return max([abs(x - y) for x, y in zip(a, b)])

str_to_dist = {
    "manhattan": manhattan,
    "euclidean": euclidean,
    "chebyshev": chebyshev
}

def read_list():
    return list(map(int, input().split()))

# n, m = read_list()
#
# datas = []
# for i in range(n):
#     datas.append(read_list())
# q = read_list()
#
# distance = input()
# kernel = input()
# window = input()
#
# win_par = int(input())
#
# distanced_datas = []
# for elem in datas:
#     distanced_datas.append((str_to_dist[distance](q, elem[:-1]), elem[-1:][0]))
# distanced_datas.sort()
#
# distances = [x for x, _ in distanced_datas]
# targets = [y for _, y in distanced_datas]
#
# if window == 'variable':
#     win_par = distances[win_par]
#
# kerneled_distances = []
# if win_par == 0:
#     kerneled_distances = [1 if dist == 0 else 0 for dist in distances]
# else:
#     for i in range(len(distances)):
#         arg_for_kernel = distances[i] / win_par
#         kerneled_distances.append(str_to_fun[kernel](arg_for_kernel))
#
# nom = sum([dist * target for dist, target in zip(kerneled_distances, targets)])
#
# denom = sum(kerneled_distances)
# print(nom / denom if denom != 0 else sum(targets) / n)
