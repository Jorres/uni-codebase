#include <iostream>
#include <string>
#include <vector>

using namespace std;

int main() {
    // freopen("input.txt", "r", stdin);
    ios::sync_with_stdio(false);

    string s;
    cin >> s;
    int n = s.size();

    long long p = 197;
    vector<long long> pow(n, 0);
    pow[0] = 1;
    for (int i = 1; i < n; i++) {
        pow[i] = pow[i - 1] * p;
    }


    vector<long long> pref_hash(n, 0);
    pref_hash[0] = s[0];
    for (int i = 1; i < n; i++) {
        pref_hash[i] = pref_hash[i - 1] + pow[i] * s[i];
    }

    int m;
    cin >> m;
    for (int i = 0; i < m; i++) {
        int a, b, c, d;
        cin >> a >> b >> c >> d;
        a--; b--; c--; d--;
        long long hl = (pref_hash[b] - (a == 0 ? 0 : pref_hash[a - 1])) * pow[c];
        long long hr = (pref_hash[d] - (c == 0 ? 0 : pref_hash[c - 1])) * pow[a];

        cout << ((hl == hr) ? "Yes" : "No") << '\n';
    }
}
