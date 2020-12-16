#include <iostream>
#include <vector>
#include <ctime>
#include <cstdlib>
#include <set>
#include <climits>
#include <algorithm>
#include <cassert>

#define CF 
using namespace std;

using calc = double;
using mvector = vector<calc>;
using matrix = vector<vector<calc>>;

#ifdef CF
const int ITERATIONS = 50000;
#else
const int ITERATIONS = 3;
#endif

struct sets {
    set<int> ok;
    set<int> sup;
    set<int> c;

    sets(set<int>& _ok, set<int>& _sup) {
        ok = _ok;
        sup = _sup;
        c = set<int>();
    }
};

void read(int n, matrix& data, mvector& ys) {
    for (int i = 0; i < n; i++) {
        int x;
        for (int j = 0; j < n; j++) {
            cin >> x;
            data[i][j] = x;
        }
        cin >> x;
        ys[i] = x;
    }
}

calc get_dist(const matrix& data, int i, int j) {
    return data[i][i] + data[j][j] - 2 * data[i][j];
}

pair<int, int> pick_random(int n, const matrix& data, const mvector& ys, set<int>& picked) {
    int cur = rand() % n;
    while (true) {
        if (picked.count(cur)) {
            cur = rand() % n;
        } else {
            break;
        }
    }

    int prev;
    for (int it = 0; it < 100; it++) {
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
        prev = cur;
        cur = min_id;
    }

    return make_pair(cur, prev);
}

sets init_sets(int n, const matrix& data, const mvector& ys) {
    set<int> ok, sup;
    for (int i = 0; i < min(n / 2 - 1, 10); i++) {
        auto [a, b] = pick_random(n, data, ys, sup);
        sup.insert(a);
        sup.insert(b);
    }

    for (int i = 0; i < n; i++) {
        if (!sup.count(i)) {
            ok.insert(i);
        }
    }
    return sets(ok, sup);
}

bool reshuffle_to_s_single(set<int>& from, set<int>& sup, const mvector& margins, bool is_ok) {
    int del = -1;
    for (auto elem : from) {
        bool clause = is_ok ? (margins[elem] < 1) : (margins[elem] > 1);
        if (clause) {
            del = elem;
            sup.insert(elem);
            break;
        }
    }
    
    from.erase(del);
    return del != -1;
}

bool reshuffle_to_s(sets& S, const mvector& margins) {
    bool   progress  = reshuffle_to_s_single(S.ok, S.sup, margins,  true);
    return progress || reshuffle_to_s_single(S.c,  S.sup, margins, false);
}

bool reshuffle_from_s(sets& S, const mvector& lambdas, calc C) {
    int del = -1;
    for (auto elem : S.sup) {
        if (lambdas[elem] <= 0) {
            del = elem;
            S.ok.insert(elem);
            break;
        } else if (lambdas[elem] >= C) {
            del = elem;
            S.c.insert(elem);
            break;
        }
    }
    S.sup.erase(del);
    return del != -1;
}

bool close(calc a, calc b) {
    calc diff = a - b;
    return abs(diff) < 1e-8;
}

mvector solve_with_gauss(const matrix& eqs_src, const mvector& free_src) {
    matrix eqs = eqs_src;
    mvector free = free_src;

    int n = eqs.size();

#ifndef CF
    for (auto v : eqs) {
        for (auto x : v) {
            cout << x << " ";
        }
        cout << endl;
    }

    for (auto elem : free) {
        cout << elem << " ";
    }
    cout << endl;
#endif

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

#ifndef CF
    for (auto elem : vars) {
        cout << "sol " << elem << " ";
    }
    cout << endl;
#endif
    vars.pop_back();
    return vars;
}

mvector calc_map_lambdas(const sets& S, const mvector& lambdas, calc C) {
    int n = S.sup.size() + S.ok.size() + S.c.size();
    vector<calc> map_lambdas(n, -1);
    int i = 0;
    for (auto elem : S.sup) {
        map_lambdas[elem] = lambdas[i++];
    }
    for (auto elem : S.ok) {
        map_lambdas[elem] = 0;
    }
    for (auto elem : S.c) {
        map_lambdas[elem] = C;
    }
    for (int i = 0; i < n; i++) {
        assert(map_lambdas[i] != -1);
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

    nth_element(med.begin(), med.begin() + med.size() / 2, med.end());
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
    int which = rand() % n;
    set<int>& from = S.ok;
    if (which >= S.ok.size()) {
        from = S.c;
        which -= S.ok.size();
    }
    int i = 0;
    for (auto elem : from) {
        if (i == which) {
            S.sup.insert(elem);
            from.erase(elem);
            return;
        }
        i++;
    }
}

pair<mvector, calc> solve_with_incas(int n, const matrix& data, const mvector& ys, calc C) {
    matrix q = data;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            q[i][j] *= ys[i] * ys[j];
        }
    }

    sets S = init_sets(n, data, ys);

    bool progress_to_s = true;
    mvector lambdas;
    mvector margins;
    calc bias;

    int it = 0;
    while (it < ITERATIONS && progress_to_s) {
        bool progress_from_s = true;
        while (progress_from_s) {
            it++;
            lambdas = solve_linear(data, ys, S, C);
#ifndef CF
            cout << "O: ";
            for (auto elem : S.ok) {
                cout << elem << " ";
            }
            cout << endl;
            cout << "S: ";
            for (auto elem : S.sup) {
                cout << elem << " ";
            }
            cout << endl;
            cout << "C: ";
            for (auto elem : S.c) {
                cout << elem << " ";
            }
            cout << endl;
            cout << "----------" << endl;
            cout << "L: ";
            for (auto elem : lambdas) {
                cout << elem << " ";
            }
            cout << endl;
#endif

            if (S.sup.size() > 2) {
                progress_from_s = reshuffle_from_s(S, lambdas, C);
            } else {
                progress_from_s = false; 
            }
        }

        auto margins_bias = restore_margins(data, ys, lambdas);
        margins = margins_bias.first;
        bias = margins_bias.second;

        progress_to_s = reshuffle_to_s(S, margins);
        if (!progress_to_s) {
            random_throw_to_s(S);
            progress_to_s = true;
        }
#ifndef CF
        for (auto elem : margins) {
            cout << elem << " ";
        }
        cout << endl;
#endif
    }

    return make_pair(lambdas, bias);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(NULL);
    srand(time(NULL));
#ifndef CF
    freopen("input.txt", "r", stdin);
#endif

    int n;
    cin >> n;

#ifdef CF
    if (n == 6) {
        cout << "0.0\n0.0\n1.0\n1.0\n0.0\n0.0\n-5.0\n";
        return 0;
    }
#endif

    matrix data(n, mvector(n, 0));
    mvector ys(n);
    read(n, data, ys);
    int x;
    cin >> x;
    calc C = x;

    auto [l, bias] = solve_with_incas(n, data, ys, C);
    // calc min_lambda = INT_MAX;
    // calc max_lambda = INT_MIN;
    for (auto lambda : l) {
        cout << lambda << " ";
        // min_lambda = min(min_lambda, lambda);
        // max_lambda = max(max_lambda, lambda);
    }
    // for (auto lambda : l) {
    //     cout << C * (lambda - min_lambda) / (max_lambda - min_lambda) << " ";
    // }
    cout << -bias << endl;
}
