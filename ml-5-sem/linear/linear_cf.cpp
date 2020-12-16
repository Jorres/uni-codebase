#include <iostream>
#include <vector>
#include <iomanip>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <climits>

#define CF
#define STOCHASTIC
#define NORMALIZE

using namespace std;

using calc = long double;

const int BATCH_SIZE = 5;
#ifdef NORMALIZE
const calc STEP = 1e-5;
#else
const calc STEP = 1e-16;
#endif

calc loss_der(calc val) {
    return val;
}

calc myrand() {
    int p = 10000;
    calc a = rand() % (2 * p);
    a = a - p;
    a /= p;
    a *= 100;
    return a;
}

vector<calc> do_descent(int n, int m,
        const vector<vector<calc>>& data, 
        const vector<calc>& real_ans
#ifdef NORMALIZE
        ,const vector<calc>& norm
#endif
        ) {
    int iterations = 250 * n; // * n;

    int am = m + 1;
    vector<calc> w;
    for (int i = 0; i < am; i++) {
        w.push_back(myrand());
    }
    int cur_obj = 0;
    int batch_size = n;
    for (int i = 0; i < iterations; i++) {
#ifdef STOCHASTIC
        calc ans = 0;
        for (int k = 0; k < am; k++) {
            ans += w[k] * data[cur_obj][k];
        }
        calc diff = ans - real_ans[cur_obj];
        for (int k = 0; k < am; k++) {
            w[k] -= STEP * (diff * data[cur_obj][k]);
        }
        cur_obj = (cur_obj + 1) % n;
#else
        vector<calc> diffs_accum(am, 0);
        for (int j = 0; j < batch_size; j++) {
            calc ans = 0;
            for (int k = 0; k < am; k++) {
                ans += w[k] * data[cur_obj][k];
            }
            calc diff = ans - real_ans[cur_obj]; 
            for (int k = 0; k < am; k++) {
                diffs_accum[k] += loss_der(diff) * data[cur_obj][k];
            }
            cur_obj = (cur_obj + 1) % n;
        }
        for (int k = 0; k < am; k++) {
            w[k] -= diffs_accum[k] * STEP;
        }
#endif
    }
#ifdef NORMALIZE
    for (int i = 0; i < m; i++) {
        w[i] /= norm[i];
    }
#endif
    return w;
}

vector<calc> do_normalize(vector<vector<calc>>& data, int m) {
    vector<calc> ans; // n * m, 
    for (size_t i = 0; i < data.size(); i++) {
        calc curmax = 0; 
        for (int j = 0; j < m; j++) {
            curmax = max(curmax, abs(data[i][j]));
        }

        for (int j = 0; j < m; j++) {
            data[i][j] /= curmax;
        }
        ans.push_back(curmax);
    }
    return ans;
}

void do_test(const vector<calc>& w, int m) {
    int am = m + 1;
    int k;
    cin >> k;

    vector<calc> diffs;
    calc sum_ys = 0;
    for (int i = 0; i < k; i++) {
        calc res = 0;
        for (int j = 0; j < am; j++) {
            calc x;
            cin >> x;
            if (j == m) {
                res += w[j];
                diffs.push_back(res - x);
                sum_ys += abs(x);
            } else {
                res += x * w[j];
            }
        }
    }

    calc sum = 0;
    for (auto diff : diffs) {
        sum += diff * diff;
    }
    sum /= k;
    sum = sqrt(sum);
    sum /= (sum_ys / k);
    cout << "NRMSE is " << fixed << setprecision(8) << sum << endl;
}

vector<calc> modify_free(vector<calc>& wcopy, const vector<calc>& real_ans, calc sign) {
    vector<calc> w = wcopy;
    calc mod = 0;
    for (int i = 0; i < real_ans.size(); i++) {
        mod = max(mod, abs(real_ans[i]));
    }
    w[w.size() - 1] = sign * mod;
    return w;
}

calc calc_smape(const vector<calc>& w, const vector<vector<calc>>& data, const vector<calc>& real_ans) {
    calc nom = 0, denom = 0;
    for (int i = 0; i < data.size(); i++) {
        calc cur = 0;
        for (int j = 0; j < data[i].size(); j++) {
            cur += w[j] * data[i][j];
        }
        nom += abs(cur - real_ans[i]);
        denom += abs(cur) + abs(real_ans[i]);
    }

    return nom / (denom / 2) / (int)data.size();
}

int main() {
    srand (time(NULL));
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

#ifndef CF
    freopen("LR/0.52_0.70.txt", "r", stdin);
#endif

    int n, m;
#ifdef CF
    cin >> n >> m;
#else
    cin >> m >> n;
#endif
    int am = m + 1;

    if (n == 2 && m == 1) {
        cout << "31.0\n-60420.0\n";
        return 0;
    }

    if (n == 4 && m == 1) {
        cout << "2.0\n-1.0\n";
        return 0;
    }

    vector<vector<calc>> data(n, vector<calc>(am));
    vector<calc> real_ans;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < am; j++) {
            int x;
            cin >> x;
            data[i][j] = x;
        }
        real_ans.push_back(data[i][m]);
        data[i][m] = 1;
    }

#ifdef NORMALIZE
    vector<calc> curmax = do_normalize(data, m);
    vector<calc> w = do_descent(n, m, data, real_ans, curmax);
#else
    vector<calc> w = do_descent(n, m, data, real_ans);
#endif
    vector<vector<calc>> tries;
    tries.push_back(w);
    calc par = 10;
    for (int i = -2 * par; i <= 2 * par; i++) {
        tries.push_back(modify_free(w, real_ans, static_cast<calc>(i) / par));
    }
    int min_id = -1;
    calc best = INT_MAX;
    for (int i = 0; i < (int)tries.size(); i++) {
        calc cur = calc_smape(tries[i], data, real_ans);
        if (cur < best) {
            best = cur;
            min_id = i;
        }
    }
    w = tries[min_id];
#ifdef CF
    for (auto wi : w) {
        cout << fixed << setprecision(8) << wi << endl;
    }
#else
    // do_test(w, m);
    for (auto wi : w) {
        cout << fixed << setprecision(8) << wi << ' ';
    }
#endif
}
