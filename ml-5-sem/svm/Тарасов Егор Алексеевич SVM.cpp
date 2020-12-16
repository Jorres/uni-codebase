#include <iostream>
#include <vector>
#include <ctime>
#include <cstdlib>
#include <set>
#include <climits>
#include <algorithm>
#include <cassert>
#include <iomanip>
#include <random>
 
#define NCF 
#define NONVALIDATE
using namespace std;
 
using calc = long double;
using mvector = vector<calc>;
using matrix = vector<vector<calc>>;
 
const int ITERATIONS = 3000;
int D = 2;
calc BETA = 5;

static std::mt19937 rnd(10);

const char* DATASET = "chips.txt";
// const string CORETYPE = "linear";
// const string CORETYPE = "poly";
const string CORETYPE = "gauss";
const int FOLDS_IN_CROSS = 5;

struct sets {
    set<int> ok;
    set<int> sup;
    set<int> c;
 
    sets(set<int>& _ok, set<int>& _sup) {
        ok = _ok;
        sup = _sup;
        c = set<int>();
    }

    int total() const {
        return ok.size() + sup.size() + c.size();
    }
};
 
calc get_dist_with_core(const vector<calc>& a, const vector<calc>& b) {
    if (CORETYPE == "linear") {
        calc ans = 0;
        for (size_t i = 0; i < a.size(); i++) {
            ans += a[i] * b[i];
        }
        return ans;
    } else if (CORETYPE == "poly") {
        calc ans = 0;
        for (size_t i = 0; i < a.size(); i++) {
            ans += a[i] * b[i];
        }
        calc st = ans;
        for (int i = 1; i < D; i++) {
            ans *= st;
        }
        return ans;
    } else if (CORETYPE == "gauss") {
        calc sq = 0;
        for (int i = 0; i < a.size(); i++) {
            sq += (a[i] - b[i]) * (a[i] - b[i]);
        }
        return exp(-BETA * sq);
    } else {
        assert(false);
    }
}

calc gen_noise(calc max_noise) {
    int rad = 1000;
    int ans = rnd() % rad - (rad / 2);
    return (calc)ans / (rad / 2) * max_noise + max_noise; // [1e-3:2e-3]
}

void read_csv_like(int n, int yn, matrix& props, matrix& data, mvector& ys) {
    set<mvector> dots;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < yn; j++) {
            cin >> props[i][j];
        }
        if (dots.count(props[i])) {
            for (int j = 0; j < yn; j++) {
                props[i][j] += gen_noise(1e-3);
            }
        }
        dots.insert(props[i]);
        cin >> ys[i];
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            data[i][j] = get_dist_with_core(props[i], props[j]);
        }
    }
}
 
calc get_dist(const matrix& data, int i, int j) {
    return data[i][i] + data[j][j] - 2 * data[i][j];
}
 
pair<int, int> pick_random(int n, const matrix& data, const mvector& ys, set<int>& picked) {
    int cur = rnd() % n;
    while (true) {
        if (picked.count(cur)) {
            cur = rnd() % n;
        } else {
            break;
        }
    }
 
    int prev = -1;
    for (int it = 0; it < n; it++) {
        calc min_dist = INT_MAX;
        calc min_id = -1;
        for (int other = 0; other < n; other++) {
            if (!picked.count(other) && 
                ys[other] != ys[cur] && 
                min_dist > get_dist(data, cur, other)) {
 
                min_dist = get_dist(data, cur, other);
                min_id = other;
            }
        }
        if (min_id != -1) {
            prev = cur;
            cur = min_id;
        } else {
            break;
        }
    }
 
    return make_pair(cur, prev);
}
 
sets init_sets(int n, const matrix& data, const mvector& ys) {
    set<int> ok, sup;
    for (int i = 0; i < 2; i++) {
        auto [a, b] = pick_random(n, data, ys, sup);
        if (a != -1 && b != -1) {
            sup.insert(a);
            sup.insert(b);
        } else {
            i--;
        }
    }
    // for (int i = 0; i < 4; i++) {
    //     int a = rnd() % n;
    //     if (sup.count(a)) {
    //         i--;
    //     } else {
    //         sup.insert(a);
    //     }
    // }
 
    for (int i = 0; i < n; i++) {
        if (!sup.count(i)) {
            ok.insert(i);
        }
    }
    return sets(ok, sup);
}
 
bool reshuffle_to_s(sets& S, const mvector& margins) {
    calc max_diff = INT_MIN;
    int min_elem = -1;

    for (auto elem : S.ok) {
        calc diff = 1 - margins[elem];
        if (diff > max_diff) {
            max_diff = diff;
            min_elem = elem;
        }
    }

    for (auto elem : S.c) {
        calc diff = margins[elem] - 1;
        if (diff > max_diff) {
            max_diff = diff;
            min_elem = elem;
        }
    }

    if (min_elem == -1) {
        return false;
    }
    S.c.erase(min_elem);
    S.ok.erase(min_elem);
    S.sup.insert(min_elem);
    return true;
}
 
bool reshuffle_from_s(sets& S, const mvector& lambdas, calc C) {
    calc max_diff = INT_MIN;
    int max_elem = -1;
    bool toC = false;
    for (auto elem : S.sup) {
        calc diff = 0;
        bool from;
        if (lambdas[elem] <= 0) {
            diff = -lambdas[elem];
            max_elem = elem;
            from = true;
        } else if (lambdas[elem] >= C) {
            diff = lambdas[elem] - C;
            max_elem = elem;
            from = false;
        }

        if (diff > max_diff) {
            max_diff = diff;
            max_elem = elem;
            toC = from;
        }
    }
    if (max_elem == -1) {
        return false;
    }
    S.sup.erase(max_elem);
    if (toC) {
        S.c.insert(max_elem);
    } else {
        S.ok.insert(max_elem);
    }
    return true;
}
 
bool close(calc a, calc b) {
    calc diff = a - b;
    return abs(diff) < 1e-8;
}
 
mvector solve_with_gauss(const matrix& eqs_src, const mvector& free_src) {
    matrix eqs = eqs_src;
    mvector free = free_src;
 
    int n = eqs.size();
 
    // cout << "There goes eqs" << endl;
    // for (int i = 0; i < (int)eqs.size(); i++) {
    //     for (auto x : eqs[i]) {
    //         cout << x << " ";
    //     }
    //     cout << free[i] << " " << endl;
    // }
 
    for (int i = 0; i < n; i++) { // putting 1 on [i][i]
        if (close(eqs[i][i], 0)) {
            for (int j = i + 1; j < n; j++) {
                if (!close(eqs[j][i], 0)) {
                    swap(eqs[i], eqs[j]);
                    swap(free[i], free[j]);
                    break;
                }
            }
        }
 
        for (int j = i + 1; j < n; j++) { // dividing line j by line_j[j]
            eqs[i][j] /= eqs[i][i];
        }
        free[i] /= eqs[i][i];
        eqs[i][i] = 1;
 
        for (int j = i + 1; j < n; j++) { // subtracting from all subsequent lines
            calc coef = eqs[j][i];
            for (int k = i; k < n; k++) {
                eqs[j][k] -= coef * eqs[i][k];
            }
            free[j] -= coef * free[i];
        }
    }
    
    mvector vars(n, 0);
    for (int i = n - 1; i >= 0; i--) {
        calc cur = free[i];
        for (int j = i + 1; j < n; j++) {
            cur -= vars[j] * eqs[i][j];
        }
        vars[i] = cur;
    }
 
    // for (auto elem : vars) {
    //     cout << "sol " << elem << " ";
    // }
    // cout << endl;
 
    vars.pop_back();
    return vars;
}
 
mvector calc_map_lambdas(const sets& S, const mvector& lambdas, calc C) {
    int n = S.sup.size() + S.ok.size() + S.c.size();
    vector<calc> map_lambdas(n, -1);
    int i = 0;
    for (auto elem : S.sup) {
        calc lambda = lambdas[i++];

        lambda = min(lambda, C);
        lambda = max(lambda, (calc)0);

        map_lambdas[elem] = lambda;
    }
    for (auto elem : S.ok) {
        map_lambdas[elem] = 0;
    }
    for (auto elem : S.c) {
        map_lambdas[elem] = C;
    }
    return map_lambdas;
}
 
mvector solve_linear(const matrix& data, const mvector& ys, const sets& S, calc C) {      
    int sSize = S.sup.size();
    matrix eqs(sSize + 1, mvector(sSize + 1));
    mvector free;

    {
        int i = 0;
        for (auto is : S.sup) {
            int j = 0;
            for (auto js : S.sup) {
                eqs[i][j] = data[is][js];
                j++;
            }
            i++;
        }
    }
 
    {
        int i = 0;
        for (auto is : S.sup) {
            eqs[sSize][i] = eqs[i][sSize] = ys[is];
            i++;
        }
        eqs[sSize][sSize] = 0;
    }
 
    for (auto is : S.sup) {
        int free_elem = +1;
        for (auto js : S.c) {
            free_elem -= data[js][is] * C;
        }
        free.push_back(free_elem);
    }
 
    calc corner = 0;
    for (auto elem : S.c) {
        corner -= ys[elem];
    }
    corner *= C;
    free.push_back(corner);

    // for (auto v : eqs) {
    //     for (auto vv : v) {
    //         cout << vv << " ";
    //     }
    //     cout << endl;
    // }
    // cout << eqs.size() << endl;
    //
    // for (auto v : free) {
    //     cout << v << " ";
    // }
    // cout << endl;
    // exit(0);
 
    return calc_map_lambdas(S, solve_with_gauss(eqs, free), C);
}
 
void calc_med(mvector& med, const mvector& lambdas, const mvector& ys, const matrix& scalars, bool strong) {
    int n = scalars.size();
    for (int i = 0; i < n; i++) {
        if (!strong || lambdas[i] > 0) {
            calc val = 0;
            for (int j = 0; j < n; j++) {
                val += lambdas[j] * ys[j] * scalars[i][j];
            }
            val -= ys[i];
            med.push_back(val);
        }
    }
}
 
pair<mvector, calc> restore_margins(const matrix& scalars, 
                                    const mvector& ys, 
                                    const mvector& lambdas) {
    int n = scalars.size();
 
    vector<calc> med;
    calc_med(med, lambdas, ys, scalars, true);
 
    if (med.size() == 0) {
        calc_med(med, lambdas, ys, scalars, false);
    }
 
    sort(med.begin(), med.end());
    calc w0 = med[med.size() / 2];
 
    vector<calc> ans;
    for (int i = 0; i < n; i++) {
        calc val = 0;
        for (int j = 0; j < n; j++) {
            val += lambdas[j] * ys[j] * scalars[i][j];
        }
        val -= w0;
        val *= ys[i];
        ans.push_back(val);
    }
 
    return make_pair(ans, w0);
}
 
void random_throw_to_s(sets& S) {
    int n = S.ok.size() + S.c.size();
    if (n == 0) { return; }

    int which = rnd() % n;
    set<int>& from = (S.ok.size() <= which) ? S.c : S.ok;
    if (which >= (int)S.ok.size()) {
        which -= S.ok.size();
    }

    int i = 0;
    for (auto elem : from) {
        if (i == which) {
            assert(S.sup.insert(elem).second);
            from.erase(elem);
            return;
        }
        i++;
    }
    assert(false);
}
 
void remember_best(const mvector& lambdas, const calc& bias, const mvector& margins, calc C, calc& bestiness, 
                   mvector& best_lambdas, calc& best_bias, const matrix& data, const mvector& ys) {
    int n = margins.size();
    calc curbestiness = 0;

    for (int i = 0; i < n; i++) {
        curbestiness += max((calc)0, 1 - margins[i]);
    }
 
    calc norm = 0;
 
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            norm += lambdas[i] * lambdas[j] * ys[i] * ys[j] * data[i][j];       
        }
    }
    
    calc tau = 1 / (2 * C);
    curbestiness += tau * norm * norm;
    // cout << curbestiness << " " << bestiness << endl;
 
    if (curbestiness < bestiness) {
        // cout << "UPGRADE" << endl;
        // cout << lambdas.size() << endl;
        best_lambdas = lambdas;
        best_bias = bias;
        bestiness = curbestiness;
    }                  
}

vector<bool> deleted;
 
pair<mvector, calc> solve_with_incas(int n, const matrix& data, const mvector& ys, calc C) {
    matrix q = matrix(n, mvector(n));
    deleted.assign(n, false);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            q[i][j] = data[i][j] * ys[i] * ys[j];
        }
    }
 
    sets S = init_sets(n, data, ys);
 
    bool progress_to_s = true;
 
    calc bestiness = INT_MAX;
    mvector best_lambdas;
    calc best_bias;
 
    int it = 0;
    while (it < ITERATIONS && progress_to_s) {
        bool progress_from_s = true;
        mvector lambdas;

        it++;

        while (progress_from_s) {
            lambdas = solve_linear(q, ys, S, C);
            // for (auto supp : S.sup) {
            //     cout << supp << ' ';
            // }
            // cout << endl;
            if (S.sup.size() > 2) {
                progress_from_s = reshuffle_from_s(S, lambdas, C);
            } else {
                progress_from_s = false; 
            }
        }
 
        auto [margins, bias] = restore_margins(data, ys, lambdas);
 
        remember_best(lambdas, bias, margins, C, bestiness, best_lambdas, best_bias, data, ys);
 
        progress_to_s = reshuffle_to_s(S, margins);

        if (!progress_to_s) {
            random_throw_to_s(S);
            progress_to_s = true;
        }

        if (it % 80 == 0) {
            random_throw_to_s(S);
        }
    }
 
    // for (auto elem : restore_margins(q, ys, best_lambdas).first) {
    //     cout << elem << " ";
    // }
    // cout << endl;
 
    return make_pair(best_lambdas, best_bias);
}

calc predict(const mvector& lambdas, const calc w0, const mvector& obj, const matrix& props, const mvector& ys) {
    calc val = 0;
    int n = lambdas.size();
    for (int i = 0; i < n; i++) {
        val += lambdas[i] * ys[i] * get_dist_with_core(obj, props[i]);
    }
    return (val - w0 < 0) ? -1 : 1;
}

void cross_validate(int n, const matrix& data, const mvector& ys, const matrix& props, const calc C) {
    int batch_size = data.size() / FOLDS_IN_CROSS;
    
    int global_correct = 0;
    for (int i = 0; i < FOLDS_IN_CROSS; i++) {
        matrix new_data;
        mvector new_ys;
        matrix val_data;
        mvector val_ys;

        matrix new_props;
        matrix val_props;
        for (int j = 0; j < n; j++) {
            int posl = i * batch_size;
            int posr = (i + 1) * batch_size - 1;
            if (j < posl || j > posr) {
                new_data.push_back(data[j]);
                new_ys.push_back(ys[j]);
                new_props.push_back(props[j]);
            } else {
                val_data.push_back(data[j]);
                val_ys.push_back(ys[j]);
                val_props.push_back(props[j]);
            }
        }
        auto [lambdas, bias] = solve_with_incas(new_data.size(), new_data, new_ys, C);
        // for (auto l : lambdas) {
        //     cout << l << " ";
        // }
        // cout << bias << endl;

        int id = 0;
        int correct = 0;
        for (auto prop : val_props) {
            assert(new_props.size() == lambdas.size());
            if (predict(lambdas, bias, prop, new_props, new_ys) == val_ys[id]) {
                correct++;
                global_correct++;
            }
            id++;
        }
        cout << "Batch num " << i << ", accuracy = " << (calc)correct / batch_size << endl;
    }
    cout << "Total accuracy " << (calc)global_correct / n << endl;
}

void printc(calc a) {
    cout << fixed << setprecision(12) << a;
}
 
int main() {
    ios::sync_with_stdio(false);
    cin.tie(NULL);
    srand(time(NULL));

    freopen(DATASET, "r", stdin);

    int n;
    cin >> n;
    int yn = 2;

    matrix data(n, mvector(n));
    mvector ys(n);
    matrix props(n, mvector(yn));

    read_csv_like(n, yn, props, data, ys);

    mvector Cs = {0.01, 0.1, 5, 10, 50, 100};
    mvector Ds = {2, 3, 4, 5};
    calc C = 50;
    BETA = 3;
#ifdef VALIDATE
    for (auto C : Cs) {
        for (auto beta : Ds) {
            cout << C << " " << beta << endl;
            BETA = beta;
            cross_validate(n, data, ys, props, C);
        }
    }
#else 
    auto [lambdas, bias] = solve_with_incas(n, data, ys, C);
    for (auto l : lambdas) {
        cout << l << " ";
    }
    cout << bias << endl;
    cout << data.size() << endl;
    for (int i = 0; i < n; i++) {
        for (auto v : props[i]) {
            cout << v << " ";
        }
        cout << ys[i] << endl;
    }
#endif
}

/*
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
 */
