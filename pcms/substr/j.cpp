#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

void calculate_short_pref(vector<int>& data, int shift, const string& s) {
    vector<int> pref(s.size(), 0);
    int last = 0;
    for (int i = 1; i < (int)s.size(); i++) {
        while (last != 0 && s[last] != s[i]) {
            last = pref[last - 1];
        }

        if (s[i] == s[last]) {
            pref[i] = last + 1;
            last++;
        } else {
            pref[i] = 0;
        }
    }
    for (int i = (int)s.size() - 1; i >= 0; i--) {
        data[i + shift] = pref[i];
    }
}

void calculate_pref(vector<int>& prec, const string& s, int n) {
    string tmp;
    for (int i = n - 1; i >= 0; i--) {
        vector<int> pref(n);
        tmp = s[i] + tmp;
        calculate_short_pref(pref, i, tmp);
        for (int j = 0; j < n; j++) {
            prec[j] = max(prec[j], pref[j]);
        }
    }
}

int main() {
    // freopen("input.txt", "r", stdin);
    ios::sync_with_stdio(false);
    string s;
    cin >> s;

    int n = s.size();
    vector<int> ans(n);
    ans[0] = 1;

    // vector<vector<int>> pref = calculate_pref(s, n);

    vector<int> precalced_max(n);
    calculate_pref(precalced_max, s, n);
    for (int i = 1; i < n; i++) {
        ans[i] = ans[i - 1] + (i + 1) - precalced_max[i];
        // for (int j = 0; j < i; j++) {
        //     int cur_p = pref[j][i];
        //     neg = max(neg, cur_p);
        // }
    }

    for (auto a : ans) {
        cout << a << '\n';
    }
}
