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

void calculate_pref(vector<vector<int>>& pref, const string& s) {
    string tmp;
    for (int i = (int)s.size() - 1; i >= 0; i--) {
        tmp = s[i] + tmp;
        calculate_short_pref(pref[i], i, tmp);
    }

    // cout << s << endl;
    // for (int i = 0; i < n; i++) {
    //     for (int j = 0; j < n; j++) {
    //         cout << pref[i][j] << " ";
    //     }
    //     cout << endl;
    // }
}

// in pref[i][j] lies prefix function of pos j, starting from suffix [i..n - 1]
//

void calculate_help(vector<vector<int>>& help, const vector<vector<int>>& pref, int n) {
    // help[i][i] == 0
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            help[i][j] = max(help[i][j - 1], pref[i][j]);
            // cout << pref[i][j] << " ";
        }
        // cout << endl;
    }
    // cout << endl;

    // for (int i = 0; i < n; i++) {
    //     for (int j = 0; j < n; j++) {
    //         cout << help[i][j] << " ";
    //     }
    //     cout << endl;
    // }
}


void precalc_answers(vector<vector<int>>& ans, const string& s) {
    int n = s.size();
    vector<vector<int>> pref(n, vector<int>(n, 0)), help(n, vector<int>(n, 0));

    calculate_pref(pref, s);
    calculate_help(help, pref, n);
    for (int len = 2; len <= n; len++) {
        for (int start = 0; start + len - 1 < n; start++) {
            int right = start + len - 1;
            ans[start][right] = ans[start + 1][right] + len;
            ans[start][right] -= help[start][right];
        }
    }
}

int main() {
    // freopen("input.txt", "r", stdin);
    ios::sync_with_stdio(false);
    string s;
    cin >> s;
    int n = s.size();
    vector<vector<int>> ans(n, vector<int>(n, 1));
    precalc_answers(ans, s);

    int q;
    cin >> q;
    for (int i = 0; i < q; i++) {
        int a, b;
        cin >> a >> b;
        a--; b--;
        cout << ans[a][b] << '\n';
    }
}
