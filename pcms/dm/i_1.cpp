#include <iostream>
#include <vector>
#include <climits>
#include <cassert>

using namespace std;

typedef long long ll;

const ll MODE = 104857601;

ll sumM(ll a, ll b) {
    return (a + b) % MODE;
}

ll multM(ll a, ll b) {
    return (a * b) % MODE;
}

ll reverse(ll b, ll pow) {
    ll res = 1;
    ll base = b;
    while (pow > 0) {
        if (pow % 2 == 1) {
            pow--;
            res = multM(res, base);
        } else {
            pow /= 2;
            base = multM(base, base);
        }
    }

    return res;
}

ll divideM(ll a, ll b) {
    // a / b by % m <==> a * (b^-1)
    return (a * reverse(b, MODE - 2)) % MODE;
}

ll negate_value(ll a) {
    return (a == 0 ? 0 : MODE - a);
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

        p.data[0] = divideM(1, data[0]);
        for (int i = 1; i < coefs; i++) {
            ll sum = 0;
            for (int prev = i - 1; prev >= 0 && i - prev < deg(); prev--) {
                sum = sumM(sum, multM(p.data[prev], data[i - prev]));
            }
            sum = MODE - sum;
            p.data[i] = divideM(sum, data[0]);
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

polynome get_q(const vector<ll>& start_coefs, int k) {
    polynome q;
    q.data.assign(k + 1, 1);
    for (int i = 0; i < k; i++) {
        q.data[i + 1] = negate_value(start_coefs[i]);
    }
    return q;
}

polynome get_p(const vector<ll>& start_values, const vector<ll>& start_coefs, int k) {
    polynome p;
    for (int i = 0; i < k; i++) {
        ll value = 0;
        int recur = 0;
        for (int j = i - 1; j >= 0 && recur < k; j--) {
            value = sumM(value, multM(start_coefs[recur], start_values[j]));
            recur++;
        }

        value = negate_value(value);
        ll diff = sumM(start_values[i], value);
        p.data.push_back(diff);
    }

    p.shrink();
    return p;
}

void compress_odd(polynome& p) {
    int last_free = 1;
    for (int i = 1; i < (int)p.data.size(); i++) {
        if (i % 2 == 0) {
            p.data[last_free++] = p.data[i];
        }
        p.data[i] = 0;
    }
    p.shrink();
}

polynome make_q_from_minus_x(polynome& t) {
    polynome p = t;
    for (int i = 0; i < p.deg(); i++) {
        if (i % 2 == 1) {
            p.data[i] = negate_value(p.data[i]);
        }
    }
    return p;
}

void convert(polynome& p, polynome& q, ll& n) {
    polynome neg_q = make_q_from_minus_x(q);
    q = q.multPolynoms(neg_q);
    p = p.multPolynoms(neg_q);


    if (n % 2 == 1) {
        p.shiftLeft(1);
    }

    n /= 2;

    compress_odd(p);
    compress_odd(q);
}

int main() {
    ios::sync_with_stdio(false);
    // freopen("input.txt", "r", stdin);

    int k;
    ll n;
    cin >> k >> n;
    n--;
    vector<ll> start_values(k);
    for (int i = 0; i < k; i++) {
        cin >> start_values[i];
    }
    vector<ll> start_coefs(k);
    for (int i = 0; i < k; i++) {
        cin >> start_coefs[i];
    }

    polynome p = get_p(start_values, start_coefs, k);
    polynome q = get_q(start_coefs, k);

    while (n > k) {
        convert(p, q, n);
    }

    polynome t = p.dividePolynoms(q, n + 1);
    cout << t.data[n] << endl;
}
