#include <iostream>
#include <vector>
#include <climits>

using namespace std;

typedef long long LL;

struct edge {
    int from, to;
    LL w;
    edge(int from_, int to_, LL w_) : from(from_), to(to_), w(w_) {
    }
};

const LL MAX_PATH = LLONG_MAX / 2;

void dfs(int v, vector<bool>& reachable,
        const vector<vector<edge>>& edges) {
    reachable[v] = true;
    for (auto e : edges[v]) {
        if (!reachable[e.to]) {
            dfs(e.to, reachable, edges);
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    bool debug = false;

    if (debug) {
        freopen("input.in", "r", stdin);
    }

    int n, m, s;
    cin >> n >> m >> s;
    s--;
    vector<vector<edge>> edges(n);
    for (int i = 0; i < m; i++) {
        int a, b;
        LL w;
        cin >> a >> b >> w;
        a--; b--;
        edges[a].push_back(edge(a, b, w));
    }

    vector<bool> reachable(n, false);
    dfs(s, reachable, edges);

    vector<LL> dist(n, MAX_PATH);
    dist[s] = 0;
    vector<bool> neg(n, false);
    for (int i = 0; i < 3 * n; i++) {
        for (int curv = 0; curv < n; curv++) {
            if (!reachable[curv]) {
                continue;
            }

            for (auto e : edges[curv]) {
                if (dist[curv] < MAX_PATH && dist[e.to] > dist[curv] + e.w) {
                    dist[e.to] = max(dist[curv] + e.w, -MAX_PATH);
                    if (i >= n) {
                        neg[curv] = true;
                    }
                }
            }
        }
    }

    for (int i = 0; i < n; i++) {
        if (!reachable[i]) {
            cout << "*" << endl;
        } else if (neg[i]) {
            cout << "-" << endl;
        } else {
            cout << dist[i] << endl;
        }
    }
}
