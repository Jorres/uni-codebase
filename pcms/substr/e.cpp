#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

void calculate_pref(vector<int>& z, const string& s) {
    int left = 0, right = 0;
    int n = s.size();
    for (int i = 1; i < n; i++) {
        z[i] = max(0, min(right - i, z[i - left]));
        while (i + z[i] < n && s[z[i]] == s[i + z[i]]) {
            z[i]++;
        }
        if (i + z[i] > right) {
            left = i;
            right = i + z[i];
        }
    }
}

int main() {
    // freopen("input.txt", "r", stdin);
    ios::sync_with_stdio(false);

    string p, s;
    cin >> p >> s;
    string p_r = p; reverse(p_r.begin(), p_r.end());
    string s_r = s; reverse(s_r.begin(), s_r.end());
    string c_r = p_r + '#' + s_r;
    string c = p + '#' + s;
    int c_size = c.size();
    vector<int> to_r(c_size, 0), to_l(c_size, 0);
    calculate_pref(to_r, c);
    calculate_pref(to_l, c_r);

    // cout << "     ";
    // for (int i = 0; i < 10; i++) {
    //     cout << i;
    // }
    // cout << "      ";
    // for (int i = 0; i < 10; i++) {
    //     cout << i;
    // }
    // cout << endl;
    //
    // cout << c << " " << c_r << endl;
    // for (int i = 0; i < c.size(); i++) {
    //     cout << to_r[i];
    // }
    // cout << " ";
    // for (int i = 0; i < c.size(); i++) {
    //     cout << to_l[i];
    // }
    // cout << endl;


    vector<int> ans;
    for (int i = 0; i < (int)s.size() - (int)p.size() + 1; i++) {
        int pref_f_idx = i + p.size() + 1;
        int fp = to_r[pref_f_idx];
        int unrev_idx = i + (int)p.size();
        int rev_idx = (int)s.size() - unrev_idx;
        int sp = to_l[rev_idx + p.size() + 1];
        // cout << i << " " << rev_idx << endl;
        if (fp + sp >= (int)p.size() - 1) {
            ans.push_back(i + 1);
        }
    }

    cout << ans.size() << endl;
    for (auto a : ans) {
        cout << a << " ";
    }
}
