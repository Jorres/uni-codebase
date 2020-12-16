#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

const int NO_EDGE = 1000000000;
const int INF = 2000000000;

int main() {
    // freopen("input.in", "r", stdin);
    ios::sync_with_stdio(false);
    int n;
    cin >> n;
    n++;
    vector<vector<int>> adj(n, vector<int>(n));
    for (int i = 1; i < n; i++) {
        for (int j = 1; j < n; j++) {
            cin >> adj[i][j];
        }
    }

    for (int i = 1; i < n; i++) {
        adj[0][i] = 0;
        adj[i][0] = NO_EDGE;
    }

    vector<int> d(n, INF), p(n, -1);
    d[0] = 0;

    int changed = -1;
    for (int it = 0; it < 2 * n; it++) {
        changed = -1;
        for (int from = 0; from < n; from++) {
            for (int to = 0; to < n; to++) {
                if (adj[from][to] != NO_EDGE && d[to] > d[from] + adj[from][to]) {
                    changed = to;
                    d[to] = d[from] + adj[from][to];
                    p[to] = from;
                }
            }
        }
    }

    if (changed == -1) {
        cout << "NO" << endl;
        return 0;
    }

    int x = changed;
    for (int i = 0; i < n; i++) {
        x = p[x];
    }

    vector<int> path;
    int cur = x;
    for (; ; cur = p[cur]) {
        path.push_back(cur);
        if (cur == x && path.size() > 1) {
            break;
        }
    }

    cout << "YES" << endl << path.size() << endl;
    reverse(path.begin(), path.end());
    for (auto i : path) {
        cout << i << " ";
    }
}
