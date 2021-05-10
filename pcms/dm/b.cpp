#include <iostream>
#include <vector>

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

void calc_root(polynome& p, int m) {
    polynome ans;

    polynome x_i;
    x_i.data = {1};

    int coef = 1;
    int mult = 1;
    int fact = 1;

    for (int i = 0; i < m; i++) {
        fact = multM(fact, (i == 0 ? 1 : i));

        polynome cur = x_i;
        cur.multByNumber(divideM(mult, fact));

        if (i == 0) {
            coef = divideM(1, 2);
            mult = divideM(1, 2);
        } else {
            coef = sumM(coef, MODE - 1);
            mult = multM(coef, mult);
        }

        ans = ans.sumPolynoms(cur, m);
        x_i = x_i.multPolynoms(p, m);
    }
    ans.print_polynome(m);
}

void calc_e(polynome& p, int m) {
    polynome ans;

    polynome x_i;
    x_i.data = {1};

    int fact = 1;

    for (int i = 0; i < m; i++) {
        fact = multM(fact, (i == 0 ? 1 : i));

        polynome cur = x_i;
        cur.multByNumber(divideM(1, fact));

        ans = ans.sumPolynoms(cur, m);
        x_i = x_i.multPolynoms(p, m);
    }
    ans.print_polynome(m);
}

void calc_log(polynome& p, int m) {
    polynome ans;
    ans.data.assign(m, 0);

    polynome x_i = p;

    int div = 1;
    int coef = 1;

    for (int i = 1; i < m; i++) {

        polynome cur = x_i;
        cur.multByNumber(divideM(coef, div));
        div = sumM(div, 1);
        coef = MODE - coef;

        ans = ans.sumPolynoms(cur, m);
        x_i = x_i.multPolynoms(p, m);
    }
    ans.print_polynome(m);

}

int main() {
    ios::sync_with_stdio(false);
    // freopen("input.txt", "r", stdin);

    int n, m;
    cin >> n >> m;
    polynome p;
    p.read_polynome(n);

    calc_root(p, m);
    calc_e(p, m);
    calc_log(p, m);
}
