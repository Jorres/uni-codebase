#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;

const ll MODE = 1000000007;

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
            if (i < static_cast<int>(data.size())) {
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
            if (i < static_cast<int>(data.size())) {
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
                if (first < (int)data.size() && second < (int)other.data.size()) {
                    p.data[i] = sumM(p.data[i], multM(data[first], other.data[second]));
                }
            }
        }
        p.crop();
        return p;
    }

    polynome find_compl(int m) {
        polynome p;
        p.data.assign(m, 0);

        p.data[0] = divideM(1, data[0]);
        for (int i = 1; i < m; i++) {
            int sum = 0;
            for (int prev = i - 1; prev >= 0 && i - prev < (int)data.size(); prev--) {
                sum = sumM(sum, multM(p.data[prev], data[i - prev]));
            }
            sum = MODE - sum;
            p.data[i] = divideM(sum, data[0]);
        }
        return p;
    }

    polynome dividePolynoms(polynome& other, int coefs) {
        polynome rev = other.find_compl(coefs);
        // other.print_polynome();
        // rev.print_polynome();

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

    polynome calc_sqrt(int m) {
        // 1 + x * d_0 + x * x * d_1 + ... = () * ()
        polynome ans;
        ans.data.push_back(1);

        for (int i = 1; i < m; i++) {
            ll sum = 0;
            for (int fir = 1; fir < i; fir++) {
                int sec = i - fir;
                sum = sumM(sum, multM(ans.data[fir], ans.data[sec]));
            }
            sum = MODE - sum;
            ans.data.push_back(divideM(sumM(data[i], sum), 2));
        }

        return ans;
    }

    void shiftLeft(int a) {
        for (int i = a; i < (int)data.size(); i++) {
            data[i - a] = data[i];
        }
        for (int i = 0; i < a; i++) {
            data.pop_back();
        }
    }
};


int main() {
    ios::sync_with_stdio(false);
    // freopen("input.txt", "r", stdin);

    int k, m;
    cin >> k >> m;
    m++;

    polynome p;
    p.data.assign(2 * m, 0);

    int min_d = 2001;

    for (int i = 0; i < k; i++) {
        int w;
        cin >> w;
        min_d = min(min_d, w);
        p.data[w] = 1;
    }

    polynome d_root = p;

    polynome I;
    I.data.assign(1, 1);

    d_root.multByNumber(MODE - 4);
    d_root = d_root.sumPolynoms(I, min_d + m);
    d_root = d_root.calc_sqrt(min_d + m);
    d_root.multByNumber(MODE - 1);


    polynome nomin = d_root.sumPolynoms(I, m + min_d);

    polynome denom = p;

    denom.multByNumber(2);

    nomin.shiftLeft(min_d);
    denom.shiftLeft(min_d);

    polynome ans = nomin.dividePolynoms(denom, m);
    for (int i = 1; i < m; i++) {
        cout << ans.data[i] << " ";
    }
}                                                                                 
