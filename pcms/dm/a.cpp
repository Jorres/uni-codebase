#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;

const ll MODE = 998244353;

ll sumM(ll a, ll b) {
    return (a + b) % MODE;
}

ll multM(ll a, ll b) {
    return a * b % MODE;
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
    return a * reverse(b, MODE - 2) % MODE;
}

struct polynome {
    vector<ll> data;

    void read_polynome(int d) {
        data.resize(d + 1);
        for (int i = 0; i < d + 1; i++) {
            cin >> data[i];
        }
    }

    polynome sumPolynoms(polynome& other, int coefs) {
        polynome p;
        int maxsize = max(data.size(), other.data.size());
        p.data.resize(min(maxsize, coefs));
        int minsize = min(data.size(), other.data.size());
        int i = 0;
        for (; i < coefs && i < minsize; i++) {
            p.data[i] = sumM(data[i], other.data[i]);
        }

        for (; i < coefs && i < maxsize; i++) {
            if (i < data.size()) {
                p.data[i] = data[i];
            } else {
                p.data[i] = other.data[i];
            }
        }
        p.crop();
        return p;
    }

    void crop() {
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
            if (i < data.size()) {
                cout << data[i] << " ";
            } else {
                cout << "0 ";
            }
        }
        cout << endl;
    }

    polynome multPolynoms(polynome& other, int coefs) {
        polynome p;
        int total_size = data.size() + other.data.size();
        p.data.assign(min(coefs, total_size), 0);

        for (int i = 0; i < coefs && i < total_size; i++) {
            for (int first = 0; first <= i; first++) {
                int second = i - first;
                if (first < data.size() && second < other.data.size()) {
                    p.data[i] = sumM(p.data[i], multM(data[first], other.data[second]));
                }
            }
        }
        p.crop();
        return p;
    }

    polynome find_compl() {
        polynome p;
        p.data.assign(1000, 0);

        p.data[0] = data[0]; // divideM(1, data[0]);
        for (int i = 1; i < 1000; i++) {
            int sum = 0;
            for (int prev = i - 1; prev >= 0 && i - prev < data.size(); prev--) {
                sum = sumM(sum, multM(p.data[prev], data[i - prev]));
            }
            sum = MODE - sum;
            p.data[i] = sum;
            // p.data[i] = divideM(sum, data[0]);
        }
        return p;
    }

    polynome dividePolynoms(polynome& other, int coefs) {
        polynome rev = other.find_compl();

        polynome p;
        p.data.assign(1000, 0);

        for (int i = 0; i < 1000; i++) {
            for (int first = 0; first <= i; first++) {
                int second = i - first;
                if (first < deg() && second < rev.deg()) {
                    p.data[i] = sumM(p.data[i], multM(data[first], rev.data[second]));
                }
            }
        }
        p.crop();
        return p;
    }

    void multByNumber(int c) {
        for (auto& v : data) {
            v = multM(v, c);
        }
    }

    int deg() {
        return data.size();
    }
};

int main() {
    ios::sync_with_stdio(false);
    // freopen("input.txt", "r", stdin);

    int k;
    cin >> k;

    vector<ll> start_value(k), start_coef(k);
    for (int i = 0; i < k; i++) {
        cin >> start_value[i];
    }
    for (int i = 0; i < k; i++) {
        cin >> start_coef[i];
    }

    polynome q;
    q.data.assign(k + 1, 1);
    for (int i = 0; i < k; i++) {
        q.data[i + 1] = -start_coef[i];
    }

    polynome p;
    for (int i = 0; i < k; i++) {
        ll value = 0;
        int coef = 0;
        for (int j = i - 1; j >= 0 && coef < k; j--, coef++) {
            value += start_coef[coef] * start_value[j];
        }

        ll diff = start_value[i] - value;
        p.data.push_back(diff);
    }

    p.crop();

    p.print_polynome();
    q.print_polynome();
}
