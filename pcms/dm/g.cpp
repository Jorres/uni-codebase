#include <iostream>
#include <vector>
#include <climits>
#include <cassert>
#include <array>

using namespace std;

typedef long long ll;

const int A_SIZE = 7;

ll sumM(ll a, ll b) {
    return a + b;
}

ll multM(ll a, ll b) {
    return a * b;
}


struct polynome {
    vector<ll> data;

    void shrink() {
        while (data.size() > 1 && data.back() == 0) {
            data.pop_back();
        }
    }

    void print_polynome(int def = -1) {
        if (def == -1) {
            def = data.size();
            cout << data.size() - 1 << endl;
        }
        for (int i = 0; i < def; i++) {
            if (i < (int)data.size()) {
                cout << data[i] << " ";
            } else {
                cout << "0 ";
            }
        }
        cout << endl;
    }

    polynome multPolynoms(polynome& other, int coefs = INT_MAX) {
        polynome p;
        int total_size = data.size() + other.data.size();
        p.data.assign(min(coefs, total_size), 0);

        for (int i = 0; i < coefs && i < total_size; i++) {
            for (int first = 0; first <= i; first++) {
                int second = i - first;
                if (first < (int)data.size() && second < (int)other.data.size()) {
                    p.data[i] = sumM(p.data[i], multM(data[first], other.data[second]));
                }
            }
        }
        p.shrink();
        return p;
    }

    polynome find_compl(int coefs) {
        polynome p;
        p.data.assign(coefs, 0);

        p.data[0] = data[0]; // divideM(1, data[0]);
        for (int i = 1; i < coefs; i++) {
            ll sum = 0;
            for (int prev = i - 1; prev >= 0 && i - prev < deg(); prev--) {
                sum = sumM(sum, multM(p.data[prev], data[i - prev]));
            }
            p.data[i] = -sum; // divideM(sum, data[0]);
        }
        return p;
    }

    polynome dividePolynoms(polynome& other, int coefs) {
        polynome rev = other.find_compl(coefs);

        polynome p;
        p.data.assign(coefs, 0);

        for (int i = 0; i < coefs; i++) {
            for (int first = 0; first <= i; first++) {
                int second = i - first;
                if (first < deg() && second < rev.deg()) {
                    p.data[i] = sumM(p.data[i], multM(data[first], rev.data[second]));
                }
            }
        }
        p.shrink();
        return p;
    }

    void multByNumber(ll c) {
        for (auto& v : data) {
            v = multM(v, c);
        }
    }


    void shiftLeft(int a) {
        for (int i = a; i < deg(); i++) {
            data[i - a] = data[i];
        }
        for (int i = 0; i < a; i++) {
            data.pop_back();
        }
    }

    int deg() {
        return data.size();
    }
};

array<ll, A_SIZE> b_array = { {0, 1, 0, 0, 0, 0, 0} };

array<ll, A_SIZE> make_list(const array<ll, A_SIZE>& src) {
    polynome p;
    p.data = {1};
    polynome q;
    q.data.assign(A_SIZE, 0);
    q.data[0] = 1;
    for (int i = 1; i < A_SIZE; i++) {
        q.data[i] -= src[i];
    }

    polynome t = p.dividePolynoms(q, A_SIZE);
    array<ll, A_SIZE> ans;
    for (int i = 0; i < A_SIZE; i++) {
        ans[i] = t.data[i];
    }
    return ans;
}

ll fact(ll a) {
    if (a <= 1) {
        return 1;
    }
    return fact(a - 1) * a;
}


ll calc_c(long long by, long long from) {
    ll partial = 1;
    if ((from - by) > by) {
        by = from - by;
    }

    for (ll t = by + 1; t <= from; t++) {
        partial *= t;
    }
    for (ll t = 1; t <= from - by; t++) {
        partial /= t;
    }

    return partial;
}

array<ll, A_SIZE> make_mset(const array<ll, A_SIZE>& src) {
    vector<vector<ll>> dp(A_SIZE, vector<ll>(A_SIZE, 0));
    dp[0].assign(A_SIZE, 1);

    for (int i = 1; i < A_SIZE; i++) {
        for (int j = 1; j < A_SIZE; j++) {
            if (src[j] <= 0) {
                dp[i][j] = dp[i][j - 1];
                continue;
            }

            int bord = (i / j) + 1;
            for (int k = 0; k < bord; k++) {
                dp[i][j] += calc_c(k, src[j] + k - 1) * dp[i - k * j][j - 1];
            }
        }
    }
    
    array<ll, A_SIZE> res;
    for (int i = 0; i < A_SIZE; i++) {
        res[i] = dp[i][i];
    }
    return res;
}

array<ll, A_SIZE> make_Pair(const array<ll, A_SIZE>& fir, const array<ll, A_SIZE>& sec) {
    array<ll, A_SIZE> ans;
    for (int i = 0; i < A_SIZE; i++) {
        ll res = 0;
        for (int j = 0; j <= i; j++) {
            res += fir[j] * sec[i - j];
        }
        ans[i] = res;
    }
    return ans;
}

array<ll, A_SIZE> parse(const string& s, int left, int right) {
    if (left == right - 1) {
        assert(s[left] == 'B');
        return b_array;
    }

    if (s[left] == 'L') {
        auto prev = parse(s, left + 2, right - 1);
        return make_list(prev);
    }

    if (s[left] == 'S') {
        auto prev = parse(s, left + 2, right - 1);
        return make_mset(prev);
    }

    assert(s[left] == 'P');
    int bal = 0;
    int i = left;
    for (; i < right; i++) {
        if (s[i] == '(') {
            bal++;
        }
        if (s[i] == ')') {
            bal--;
        }
        if (bal == 1 && s[i] == ',') {
            break;
        }
    }
    auto l_arr = parse(s, left + 2, i);
    auto r_arr = parse(s, i + 1, right - 1);
    return make_Pair(l_arr, r_arr);
}

int main() {
    ios::sync_with_stdio(false);
    // freopen("input.txt", "r", stdin);
    
    string s;
    cin >> s;
    // spaces
    
    array<ll, A_SIZE> ans = parse(s, 0, s.size());
    for (int i = 0; i < A_SIZE; i++) {
        cout << ans[i] << " ";
    };
}

