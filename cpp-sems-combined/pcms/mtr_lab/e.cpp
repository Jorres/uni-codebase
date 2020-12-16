#include <iostream> 
#include <vector>
#include <algorithm>
#include <climits>

using namespace std;

bool is_loop(int set, const vector<int>& loops) {
    for (auto loop : loops) {
        if ((set & loop) == loop) {
            return true;
        }
    }
    return false;
}

int main() {
    // freopen("input.in", "r", stdin);
    freopen("cycles.in", "r", stdin);
    freopen("cycles.out", "w", stdout);

    int n, m;
    cin >> n >> m;
    vector<pair<int, int>> x;
    for (int i = 0; i < n; i++) {
        int w;
        cin >> w;
        x.push_back(make_pair(w, i));
    }

    sort(x.begin(), x.end());

    vector<int> loops;
    for (int i = 0; i < m; i++) {
        int am;
        cin >> am;
        int loop = 0;
        for (int j = 0; j < am; j++) {
            int cur;
            cin >> cur;
            loop |= (1 << (cur - 1));
        }
        loops.push_back(loop);
    }

    int old = 0;
    long long ans = 0;
    for (int i = n - 1; i >= 0; i--) {
        int new_set = old | (1 << x[i].second);
        if (!is_loop(new_set, loops)) {
            ans += x[i].first;
            old = new_set;
        }
    }
    cout << ans << endl;
}
