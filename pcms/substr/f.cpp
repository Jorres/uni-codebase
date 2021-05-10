#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

void find_divs(vector<int>& divs, int value) {
    for (int i = 1; i * i < value; i++) {
        if (value % i == 0) {
            divs.push_back(i);
            divs.push_back(value / i);
        }
    }
    if ((int)sqrt(value) * (int)sqrt(value) == value) {
        divs.push_back((int)sqrt(value));
    }
    sort(divs.begin(), divs.end());
}

bool check_is_period(int length, const string& s, const vector<int>& z) {
    int cpos = length;
    for (; cpos < (int)s.size(); cpos += length) {
        if (z[cpos] < length) {
            return false;
        }
    }

    return true;
}

void calculate_z_function(vector<int>& z, const string& s) {
    int left = 0, right = 0;
    int n = (int)s.size();
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
    string s;
    cin >> s;
    vector<int> divs;
    find_divs(divs, (int)s.size());
    vector<int> z(s.size(), 0);
    calculate_z_function(z, s);

    // for (int i = 0; i < (int)s.size(); i++) {
    //     cout << z[i] << " ";
    // }
    // cout << endl;

    for (int i = 0; i < (int)divs.size(); i++) {
        if (check_is_period(divs[i], s, z)) {
            cout << divs[i] << endl;
            return 0;
        }
    }
}
