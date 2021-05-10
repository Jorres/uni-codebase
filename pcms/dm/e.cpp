#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;

struct polynome {
    vector<ll> data;

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

    polynome multPolynoms(polynome& other) {
        polynome p;
        int total_size = data.size() + other.data.size();
        p.data.assign(total_size, 0);

        for (int i = 0; i < total_size; i++) {
            for (int first = 0; first <= i; first++) {
                int second = i - first;
                if (first < (int)data.size() && second < (int)other.data.size()) {
                    p.data[i] += data[first] * other.data[second];
                }
            }
        }
        p.crop();
        return p;
    }

    void multByNumber(int c) {
        for (auto& v : data) {
            v *= c;
        }
    }

    int deg() {
        return data.size();
    }

    ll get_value(ll x) {
        ll res = 0;
        ll x_deg = 1;
        for (int i = 0; i < deg(); i++) {
            res += data[i] * x_deg;
            x_deg *= x;
        }
        return res;
    }
};


int main() {
    ios::sync_with_stdio(false);
    // freopen("input.txt", "r", stdin);

    ll r, d;
    cin >> r >> d;
    polynome quazi;
    quazi.data.resize(d + 1);
    vector<ll> start_value(d + 1);
    for (int i = 0; i < d + 1; i++) {
        cin >> quazi.data[i];
    }

    ll rv = 1;
    for (int i = 0; i < d + 1; i++) {
        start_value[i] = quazi.get_value(i) * rv;
        rv *= r;
    }

    polynome base;
    base.data = {1, -r};
    polynome q = base;
    for (int i = 0; i < d; i++) {
        q = q.multPolynoms(base);
    }

    polynome p;
    for (int i = 0; i < d + 1; i++) {
        ll value = 0;
        ll coef = 0;
        for (int j = i - 1; j >= 0 && coef < d; j--, coef++) {
            value += start_value[j] * (-q.data[coef + 1]);
        }
        ll diff = start_value[i] - value;
        p.data.push_back(diff);
    }   
    p.crop(); 
    q.crop();

    p.print_polynome();
    q.print_polynome();
}                                                                                 
