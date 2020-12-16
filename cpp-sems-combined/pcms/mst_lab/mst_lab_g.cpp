#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>
#include <cmath>
#include <iomanip>

using namespace std;

const int INF = 2000000000;

// 20000 * 20000 * 2 == 800 mln
int get_dist(pair<int, int> a, pair<int, int> b) {
    return (a.first - b.first)   * (a.first - b.first) +
           (a.second - b.second) * (a.second - b.second);
}

int main() {
    ios::sync_with_stdio(false);
    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    }

    int n;
    cin >> n;
    vector<pair<int, int>> dots;
    vector<vector<int>> adj(n, vector<int>(n));

    for (int i = 0; i < n; i++) {
        int a, b;
        cin >> a >> b;
        dots.push_back(make_pair(a, b));
    }

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            adj[i][j] = get_dist(dots[i], dots[j]);
        }
    }

    vector<int> dist(n, INF), from(n, -1);
    vector<bool> used(n, false);
    dist[0] = 0;

    long double ans = 0;

    for (int i = 0; i < n; i++) {
        int min_v = -1;
        for (int j = 0; j < n; j++) {
            if (!used[j] && (min_v == -1 || dist[j] < dist[min_v])) {
                min_v = j;
            }
        }
        used[min_v] = true;

        if (from[min_v] != -1) {
            ans += sqrt((long double)adj[from[min_v]][min_v]);
        }

        for (int to = 0; to < n; to++) {
            if (dist[to] > adj[min_v][to]) {
                dist[to] = adj[min_v][to];
                from[to] = min_v;
            }
        }
    }
    cout << setprecision(10) << ans << endl;
}
