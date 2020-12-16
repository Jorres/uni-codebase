#include <iostream>
#include <vector>
#include <set>
#include <queue>
#include <climits>

using namespace std;

typedef long long LL;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);

    int n, m;
    cin >> n >> m;
    vector<vector<int>> edges(n);
    vector<vector<LL>> weights(n);

    for (int i = 0; i < m; i++) {
        int a, b, w;
        cin >> a >> b >> w;
        a--; b--;
        edges[a].push_back(b);
        weights[a].push_back(w);

        edges[b].push_back(a);
        weights[b].push_back(w);
    }

    LL max_value = 2LL * INT_MAX;
    vector<LL> d(n, max_value);
    d[0] = 0;

    set<pair<LL, int>> q;
    q.insert(make_pair(0, 0));

    while (!q.empty()) {
        auto cur_edge = *q.begin();
        int v = cur_edge.second;
        q.erase(q.begin());

        if (d[v] < cur_edge.first) {
            continue;
        }

        for (int i = 0; i < (int)edges[v].size(); i++) {
            LL  w = weights[v][i];
            int to = edges[v][i];

            if (d[v] + w < d[to]) {
                d[to] = d[v] + w;
                q.insert(make_pair(d[to], to));
            }
        }
    }

    for (auto to_d : d) {
        if (to_d == max_value) {
            cout << -1 << " ";
        } else {
            cout << to_d << " ";
        }
    }
}
