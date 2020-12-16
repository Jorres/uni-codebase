import numpy
import sys
import math

from matplotlib import pyplot as plt

FREE_MEMBER = 1

def read_data(filename):
    f = open(filename, 'r')
    lines = f.readlines()

    ntraits = int(lines[0])

    train_set_size = int(lines[1])
    # test_set_size = int(lines[train_set_size + 1])

    train_set_arr = [ list(map(float, x.strip().split(' '))) for x in lines[2:train_set_size + 2] ]
    train_ys = numpy.array([obj[ntraits] for obj in train_set_arr])
    for i in range(0, len(train_set_arr)):
        train_set_arr[i][ntraits] = FREE_MEMBER

    test_set_arr = [ list(map(float, x.strip().split(' '))) for x in lines[train_set_size + 3:] ]
    test_ys = numpy.array([obj[ntraits] for obj in test_set_arr])
    for i in range(0, len(test_set_arr)):
        test_set_arr[i][ntraits] = FREE_MEMBER

    train_set = numpy.array(train_set_arr)
    test_set = numpy.array(test_set_arr)

    return [ntraits, train_set, test_set, train_ys, test_ys]

def calculate_effectiveness(w, data, ys):
    nobj = len(data)
    supp_ans = data.dot(w)
    diff = supp_ans - ys
    nrmse = math.sqrt(sum([x * x for x in diff]) / nobj) / (sum(abs(ys)) / nobj) 
    return nrmse

def do_mnk(reg_param, train_set, ys):
    print(len(train_set))
    v, d, uT = numpy.linalg.svd(train_set, full_matrices=False)

    dd = numpy.diag(d)
    dd_reversed = numpy.diag([1 / (reg_param + di ** 2) for di in d])

    mult = [uT.T, dd, dd_reversed, v.T, ys]
    while len(mult) > 1:
        mult[-2] = mult[-2].dot(mult[-1])
        mult.pop()
    return mult[0]

def calc_risk(w, ntraits, cur_set, cur_ans): # using mean square
    return sum([(cur_obj.dot(w) - cur_y) ** 2 for (cur_obj, cur_y) in zip(cur_set, cur_ans)]) / ntraits

def loss_der(val):
   return val

def do_gradient_descent(ntraits, train_set, train_ys, test_set, test_ys):
    ALPHA = 0.01
    MU = 1e-16
    ITERS = 10000
    BATCH = 30

    w = numpy.zeros(ntraits + 1) # vector
    L = calc_risk(w, ntraits, train_set, train_ys)

    curObj = 0
    history_train = []
    history_test = []
    for i in range(0, ITERS):
        diffs_accum = numpy.zeros(ntraits + 1)
        for _ in range(0, BATCH):
            ans_diff = (train_set[curObj].dot(w) - train_ys[curObj])
            diffs_accum += train_set[curObj] * loss_der(ans_diff)
            curObj = (curObj + 1) % len(train_set)

        diffs_accum *= MU
        w = numpy.subtract(w, numpy.array(diffs_accum))

        L = ALPHA * calc_risk(w, ntraits, train_set, train_ys) + (1 - ALPHA) * L
        if (i % 100 == 0):
            history_train.append(calculate_effectiveness(w, train_set, train_ys))
            history_test.append(calculate_effectiveness(w, test_set, test_ys))
        print(i, L)

        # L = ALPHA * L + (1 - ALPHA) * (sum([x * x for x in ]) / ntraits)
        # w_k+1 = w_k - mu * Grad((<w_k, x_obj> - y_obj)^2)'
        # diff: 2 * (w_j - curObj_j) 

    return [w, history_train, history_test]

def plot_history(history_train, history_test):
    plt.xlabel("$Iteration$")
    plt.ylabel("$NRMSE$")
    plt.plot(list(range(0, len(history_train))), history_train, '-o')
    plt.plot(list(range(0, len(history_test))), history_test, '-x')
    plt.show()

def main():
    ntraits, train_set, test_set, train_ys, test_ys = read_data('LR/4.txt')

    reg = 1
    w_mnk = do_mnk(reg, train_set, train_ys)
    calculate_effectiveness(w_mnk, test_set, test_ys)

    [w_desc, history_train, history_test] = do_gradient_descent(ntraits, train_set, train_ys, test_set, test_ys)
    calculate_effectiveness(w_desc, test_set, test_ys)
    plot_history(history_train, history_test)

main()
