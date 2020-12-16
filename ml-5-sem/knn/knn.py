import math
import pandas as pd
import numpy as np
import sys
import random
import warnings

from matplotlib import pyplot as plt

if not sys.warnoptions:
    warnings.simplefilter("ignore")

def calculate_f_mera(matrix):
    m_len = len(matrix)
    elems_of = [0] * m_len
    col = [0] * m_len

    tp = [0] * m_len
    tn = [0] * m_len
    fp = [0] * m_len
    fn = [0] * m_len

    for i in range(m_len):
        line = matrix[i]
        elems_of[i] = sum(line)
        for j in range(len(line)):
            col[j] += line[j]
        tp[i] = line[i]

    all_elems = sum(elems_of)

    for i in range(m_len):
        fn[i] = elems_of[i] - tp[i]
        fp[i] = col[i] - tp[i]
        tn[i] = sum(elems_of) - tp[i] - fn[i] - fp[i]

    micro_fs = [0] * m_len

    prec_w = 0
    rec_w = 0
    for i in range(m_len):
        prec = 0
        if tp[i] + fp[i] != 0:
            prec = tp[i] / (tp[i] + fp[i])
        rec = 0
        if tp[i] + fn[i] != 0:
            rec = tp[i] / (tp[i] + fn[i])

        part = elems_of[i] / all_elems
        if prec + rec == 0:
            micro_fs[i] = 0
        else:
            part = elems_of[i] / all_elems
            micro_fs[i] = part * (2 * (prec * rec) / (prec + rec))

        prec_w += part * prec
        rec_w += part * rec

    return (2 * (prec_w * rec_w) / (prec_w + rec_w), sum(micro_fs))

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

def manhattan(a, b, cut_tail):
    return sum([abs(a[i] - b[i]) for i in range(len(a) - cut_tail)])

def chebyshev(a, b, cut_tail):
    return max([abs(a[i] - b[i]) for i in range(len(a) - cut_tail)])

def euclidean(a, b, cut_tail):
    return math.sqrt(sum([(a[i] - b[i]) ** 2 for i in range(len(a) - cut_tail)]))

def sqeuclidean(a, b, cut_tail):
    return sum([(a[i] - b[i]) ** 2 for i in range(len(a) - cut_tail)])

def calculate_kerneled_distances(distances, k_nearest, window_parameter, kernel_function):
    kerneled_distances = []             
    if window_parameter == 0:
        kerneled_distances = [1 if dist == 0 else 0 for dist in distances]
    else:
        for elem in k_nearest:
            kerneled_distances.append(
                    str_to_fun[kernel_function](elem[0] / window_parameter))
    return kerneled_distances

def get_k_near_labeled(dataset, target_pos, k, distance_function):
    target = dataset.values[target_pos]
    distances = [distance_function(row, target, 1) for row in dataset.values]

    max_dist = max(distances)
    distances[target_pos] = max_dist + 1

    labels = [elem[-1] for elem in dataset.values]
    labels[target_pos] = 100

    labeled_distances = list(zip(distances, labels))
    labeled_distances.sort()
    labeled_distances.pop()
    return labeled_distances[:k]

def predict_classification(dataset, target_pos, k, distance_function):
    k_nearest = get_k_near_labeled(dataset, target_pos, k, distance_function)

    labels = [0] * 3
    for elem in k_nearest:
        labels[int(elem[-1])] += 1
    return labels.index(max(labels))

def do_classification(dataset, datalen, window, distance_function):
    matrix = [[0] * 3 for _ in range(3)]
    for i in range(datalen):
        predicted = predict_classification(dataset, i, window, distance_function)
        real = dataset.values[i][-1]
        matrix[int(real)][predicted] += 1
    print("F-score for classification: ", calculate_f_mera(matrix))

def predict_naive_regression(dataset, target_pos, window, kernel_function, distance_function):
    k_nearest = get_k_near_labeled(dataset, target_pos, window + 1, distance_function) # (dist, label)
    window_parameter = k_nearest[-1][0]
    k_nearest.pop()
    distances, labels = np.transpose(k_nearest)
   
    kerneled_distances = calculate_kerneled_distances(distances, k_nearest, window_parameter, kernel_function)

    nomin = sum([dist * label for dist, label in zip(kerneled_distances, labels)])
    denom = sum(kerneled_distances)
    return round(nomin / denom if denom != 0 else sum(labels) / len(k_nearest))

def do_naive_regression(dataset, datalen, window, kernel, distance_function):
    matrix = [[0] * 3 for _ in range(3)]
    for i in range(datalen):
        predicted = predict_naive_regression(dataset, i, window, kernel, distance_function)
        real = dataset.values[i][-1]
        matrix[int(real)][predicted] += 1
    # print("F-score for naive regression: ", calculate_f_mera(matrix))
    return calculate_f_mera(matrix)

def get_k_near_labeled_with_multiple_labels(dataset, target_pos, k, distance_function):
    target = dataset.values[target_pos]
    distances = [distance_function(row, target, 3) for row in dataset.values]

    max_dist = max(distances)
    distances[target_pos] = max_dist + 1

    labels = [(elem[-3], elem[-2], elem[-1])  for elem in dataset.values]
    labeled_distances = list(zip(distances, labels))

    labeled_distances.sort()
    labeled_distances.pop()
    return labeled_distances[:k]

def normalize(dataset):
    result = dataset.copy()
    for feature_name in dataset.columns:
        if feature_name == 'class':
            continue
        max_value = dataset[feature_name].max()
        min_value = dataset[feature_name].min()
        result[feature_name] = (dataset[feature_name] - min_value) / (max_value - min_value)
    return result

def one_hot_encode(dataset): 
    one_hot = pd.get_dummies(dataset['class'], prefix='class')
    dataset = dataset.drop(columns=['class'])
    for i in range(3):
        dataset["class_" + str(i)] = one_hot["class_" + str(i)]
    return dataset

# returns id of the class
def predict_progressive_regression(dataset, target_pos, k, kernel_function, distance_function):
    k_nearest = get_k_near_labeled_with_multiple_labels(dataset, target_pos, k + 1, distance_function) # (dist, (l1, l2, l3))
    window_parameter = k_nearest[-1][0]
    k_nearest.pop()
    distances, labels = np.transpose(k_nearest)

    kerneled_distances = calculate_kerneled_distances(
                            distances, k_nearest, window_parameter, kernel_function)
    ans = -1
    best = -1 

    for i in range(3):
        # one dirty(smart) hack, our dots are actually twice as important
        m_kerneled_distances = [dist * (1 if label[i] == 1 else 1/2) 
                                    for dist, label in zip(kerneled_distances, labels)]
        nomin = sum([dist * label[i] for dist, label in zip(m_kerneled_distances, labels)])
        denom = sum(m_kerneled_distances)
        prediction = nomin / denom if denom != 0 else sum([labels[t][i] for t in range(len(labels))]) / k

        if prediction > best:
            best = prediction
            ans = i
    return ans

def do_progressive_regression(dataset, datalen, neighbours, kernel, distance, should_log=False):
    encoded = one_hot_encode(dataset)
    matrix = [[0] * 3 for _ in range(3)]
    for i in range(datalen):
        predicted = predict_progressive_regression(encoded, i, neighbours, kernel, distance)
        real = dataset.values[i][-1]

        matrix[int(real)][predicted] += 1
    if should_log:
        print("F-score for one-got-regression: ", calculate_f_mera(matrix))
    return calculate_f_mera(matrix)

def draw_window_dependency(dataset, datalen, window, kernel, distance_function):
    print("Drawing some cool plot, wait 10-20 seconds...")
    naive = []
    some = []
    best = []
    x_1 = []
    x_2 = []
    x_3 = []
    for i in range(20):
        window = i + 2
        k = window
        x_1.append(window)
        x_2.append(window)
        x_3.append(window)
        f_score = do_naive_regression(dataset, datalen, window, "quartic", chebyshev)[0]
        naive.append(f_score)
        f_score = do_progressive_regression(dataset, datalen, window, "quartic", euclidean)[0]
        best.append(f_score)
        f_score = do_progressive_regression(dataset, datalen, window, "triangular", chebyshev)[0]
        some.append(f_score)
        
    plt.xticks([i for i in range(20)])

    # subscript = "knn, " + str(kernel) + ", " + distance_function.__name__
    subscript = "green - best, blue - naive, orange - some random"

    plt.xlabel("$" + subscript + "$")
    plt.ylabel("$F-score$")
    plt.plot(x_1, naive, '-o')
    plt.plot(x_2, some, '-o')
    plt.plot(x_3, best, '-o')
    plt.show()

def find_best_combination(dataset, datalen):
    top = -1
    topfun = ''
    topwindow = -1
    topdistance = euclidean

    for fun in list(str_to_fun):
        for window in range(5, 15):
            for distance in [sqeuclidean, manhattan, euclidean, chebyshev]:
                cur = do_progressive_regression(dataset, datalen, window, fun, distance)[0]
                if cur > top:
                    top = cur
                    topfun = fun
                    topwindow = window
                    topdistance = distance
                print(fun, window, str(distance))
                print('---')
    print(top, topfun, topwindow, str(topdistance))

def main():
    dataset = pd.read_csv('dataset.csv')
    dataset = normalize(dataset)
    datalen = len(dataset.values)

    window = 5
    kernel = "quartic"
    distance = manhattan
      
    do_classification(dataset, datalen, window, distance)
    do_naive_regression(dataset, datalen, window, kernel, distance)
    do_progressive_regression(dataset, datalen, window, kernel, distance, should_log=True)

    # find_best_combination(dataset, datalen)

    draw_window_dependency(dataset, datalen, window, kernel, distance)

main()
