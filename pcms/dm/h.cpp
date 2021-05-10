#include <iostream>
#include <vector>
#include <climits>

using namespace std;

const int MODE = 998244353;

int sumM(int a, int b) {
    return (a + b) % MODE;
}

int multM(int a, int b) {
    return ((long long) a * b) % MODE;
}

int reverse(int b, int pow) {
    int res = 1;
    int base = b;
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

int divideM(int a, int b) {
    // a / b by % m <==> a * (b^-1)
    return ((long long)a * reverse(b, MODE - 2)) % MODE;
}

struct polynome {
    vector<int> data;

    void read_polynome(int d) {
        data.resize(d + 1);
        for (int i = 0; i < d + 1; i++) {
            cin >> data[i];
        }
    }

    polynome sumPolynoms(polynome& other, int coefs = INT_MAX) {
        polynome p;
        int maxsize = max(data.size(), other.data.size());
        p.data.resize(min(maxsize, coefs));
        int minsize = min(data.size(), other.data.size());
        int i = 0;
        for (; i < coefs && i < minsize; i++) {
            p.data[i] = sumM(data[i], other.data[i]);
        }

        for (; i < coefs && i < maxsize; i++) {
            if (i < (int)data.size()) {
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
        p.crop();
        return p;
    } 

    polynome find_compl(int coefs) {
        polynome p;
        p.data.assign(coefs, 0);

        p.data[0] = data[0]; // divideM(1, data[0]);
        for (int i = 1; i < coefs; i++) {
            int sum = 0;
            for (int prev = i - 1; prev >= 0 && i - prev < (int)data.size(); prev--) {
                sum = sumM(sum, multM(p.data[prev], data[i - prev]));
            }
            sum = MODE - sum;
            p.data[i] = sum;
            // p.data[i] = divideM(sum, data[0]);
        }
        return p;
    }

    void shiftLeft(int a) {
        for (int i = a; i < deg(); i++) {
            data[i - a] = data[i];
        }
        for (int i = 0; i < a; i++) {
            data.pop_back();
        }
    }

    void shiftRight() {
        data.push_back(data.back());
        for (int i = data.size() - 2; i > 0; i--) {
            data[i] = data[i - 1];
        }
        data[0] = 0;
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
    //
    int k, n;
    cin >> k >> n;
    n++;

    // T_1 = x
    // T_k = x + T_k * T_k_-_1 || T_k * (1 - T_k-1) = x
    // T_k = x / (1 - T_k_-_1)
    
    polynome p, q;
    p.data = {0, 1};
    q.data = {1};
    
    for (int i = 3; i <= k; i++) {
        polynome x;
        x.data = {0, 1};

        polynome ps = q;
        ps.shiftRight();

        for (int j = 0; j < max(q.data.size(), p.data.size()); j++) {
            int l, r;
            if (j < q.data.size()) {
                l = q.data[j];
            } else {
                l = 0;
            }
            if (j < p.data.size()) {
                r = p.data[j];
            } else {
                break;
            }
            if (j >= q.data.size()) {
                q.data.push_back(sumM(l, MODE - r));
            } else {
                q.data[j] = sumM(l, MODE - r);
            }
        }
        p = ps;
    }

    polynome r = p.dividePolynoms(q, n);

    for (int i = 1; i < n; i++) { 
        if (i < (int)r.data.size()) {
            cout << r.data[i] << endl;
        } else {
            cout << 0 << endl;
        }
    }
}
