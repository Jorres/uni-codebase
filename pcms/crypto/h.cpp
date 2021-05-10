#include <iostream>
#include <vector>
#include <cassert>

using namespace std;

int p;
int y;

bool isPrime(int n) {
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            return false;
        }
    }
    return true;
}

int sum(int a, int b) {
    int c = a + b;
    if (c >= p) {
        c -= p;
    }
    return c;
}

int mult(int a, int b) {
    return (int) ((long long) a * b % p);
}

int pow(int a, int b) {
    int res = 1;
    while (b > 0) {
        if (b % 2 == 1) {
            res = mult(res, a);
            // b-- i tak soydet
        }
        a = mult(a, a);
        b /= 2;
    }
    return res;
}

int invert(int a) {
    return pow(a, p - 2);
}

vector<int> fft(vector<int> a, int n) {
    assert((int) a.size() == n);
    if (n == 1) {
        return vector<int>(1, a[0]);
    }
    vector<int> a0(n / 2, 0);
    vector<int> a1(n / 2, 0);
    for (int i = 0; i < n; i += 2) {
         a0[i / 2] = a[i];
         a1[i / 2] = a[i + 1];
    }
    vector<int> y0 = fft(a0, n / 2);
    vector<int> y1 = fft(a1, n / 2);

    vector<int> ans(n);
    int halved = n / 2;
    int root = 1;
    int w = pow(y, (p - 1) / n);
    for (int i = 0; i < n; i++) {
       ans[i] = sum(mult(y1[i % halved], root), y0[i % halved]);
       root = mult(root, w);
    }
    return ans;
}

int sign = 0;

void process_first(string& s) {
    char c;
    cin >> c;
    cout << c << endl;
    if (c == '-') {
        sign++;
    } else {
        s += c;
    }
    string rem;
    cin >> rem;
    s += rem;
}

int main() {
    freopen("input.in", "r", stdin);
    string fst_str, snd_str;
    process_first(fst_str);
    process_first(snd_str);

    int deg = (fst_str.size() - 1) + (snd_str.size() - 1) + 2;
    int t = 1;
    while ((1 << t) <= deg) {
        t++;
    }

    int n = (1 << t);

    vector<cmpl> a(n, 0), b(n, 0);
    for (int i = 0; i < (int)fst_str.size(); i++) {
        a[i] = cmpl{fst_str[i] - '0', 0};
    }
    for (int i = 0; i < (int)snd_str.size(); i++) {
        b[i] = cmpl{snd_str[i] - '0', 0};
    }

    vector<int> a_trans = fft(a, n);
    vector<int> b_trans = fft(b, n);

    for (int i = 0; i < n; i++) {
        a_trans[i] = mult(a_trans[i], b_trans[i]);
    }

    // reverse fft start
    vector<int> res = fft(a_trans, n);
    vector<int> real_res(n, 0);
    real_res[0] = divide(res[0], n);
    for (int i = 1; i < n; i++) {
        real_res[i] = divide(res[n - i], n);
    }
    // reverse fft end

    if (sign == 1) {
        cout << '-';
    }

    bool met = false;
    for (int i = n - 1; i >= 0; i--) {

    }
}
