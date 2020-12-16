#include <iostream>
#include <set>
#include <climits>

using namespace std;

int pow(int a) {
    int ans = 0;
    while (a > 0) {
        a &= (a - 1);
        ans++;
    }
    return ans;
}

int main() {
    // freopen("input.in", "r", stdin);
    freopen("check.in", "r", stdin);
    freopen("check.out", "w", stdout);

    int n, m;
    cin >> n >> m;
    set<int> I;

    for (int i = 0; i < m; i++) {
        int am;
        cin >> am;
        int elem = 0;
        for (int j = 0; j < am; j++) {
            int val;
            cin >> val;
            elem |= (1 << (val - 1));
        }
        I.insert(elem);
    }

    if (I.count(0) == 0) {
        cout << "NO" << endl;
        return 0;
    }

    for (auto i : I) {
        int mask = INT_MAX;
        for (int j = 0; j < n; j++) {
            mask ^= (1 << j);
            if (I.count(i & mask) == 0) {
                cout << "NO" << endl;
                return 0;
            }
        }
    }

    for (auto i : I) {
        for (auto an_i : I) {
            if (pow(i) > pow(an_i)) {
                // cout << i << " " << an_i << endl;
                int diff = i & (~an_i);
                // cout << diff << endl;
                bool found = false;
                for (int j = 0; j < n && !found; j++) {
                    if ((diff & (1 << j)) > 0 && I.count(an_i | (1 << j))) {
                        found = true;
                    }
                }
                if (!found) {
                    cout << "NO" << endl;
                    return 0;
                }
            }
        }
    }
    cout << "YES" << endl;
}
