#include <iostream>
#include <vector>
#include <cstring>
#include <set>

using namespace std;

struct edge {
    int to;
    int cap;
};

const int N = 202;

struct wrap {
    vector<int> g[N];
    vector<edge> edges;
    vector<int> flow;
    int dist[N];
    int ptrs[N];
};

int q[N];

int min(int a, int b) {
    return a < b ? a : b;
}

bool bfs(int s, int t, wrap& w) {
    memset(w.dist, -1, sizeof(int) * N);
    int last = 0;
    int cur = 0;
    q[last++] = s;
    w.dist[s] = 0;

    while (cur < last && w.dist[t] == -1) {
        int v = q[cur++];

        for (int ind : w.g[v]) {
            if (w.dist[w.edges[ind].to] == -1 && w.edges[ind].cap > w.flow[ind]) {
                q[last++] = w.edges[ind].to;
                w.dist[w.edges[ind].to] = w.dist[v] + 1;
            }
        }
    }
    return w.dist[t] != -1;
}

int dfs(int v, int t, wrap& w, int c_min) {
    if (v == t) {
        return c_min;
    }

    edge e;
    for (; w.ptrs[v] < static_cast<int>(w.g[v].size()); ++w.ptrs[v]) {
        int ind = w.g[v][w.ptrs[v]];
        e = w.edges[ind];

        int possible = e.cap - w.flow[ind];

        if (w.dist[e.to] != w.dist[v] + 1 || possible == 0) {
            continue;
        }

        if (possible < c_min) {
            c_min = possible;
        }

        int pushed = dfs(e.to, t, w, c_min);

        if (pushed > 0) {
            w.flow[ind] += pushed;
            w.flow[ind ^ 1] -= pushed;
            return pushed;
        }
    }

    return 0;
}

long long dinic(int s, int t, wrap& w) {
    long long ans = 0;

    while (true) {
        if (!bfs(s, t, w)) {
            break;
        }
        memset(w.ptrs, 0, sizeof(int) * N);
        while (true) {
            int INF = 1e9;
            int pushed = dfs(0, t, w, INF);
            if (pushed == 0) {
                break;
            }
            ans += pushed;
        }
    }
    return ans;
}

void add_edge(wrap& w, int from, int ind, int cap, int to) {
    w.g[from].push_back(ind);
    w.edges[ind].cap = cap;
    w.edges[ind].to = to;
    w.g[to].push_back(ind + 1);
    w.edges[ind + 1].cap = 0;
    w.edges[ind + 1].to = from;
}

int main() {
    ios::sync_with_stdio(false);

    // freopen("input.in", "r", stdin);

    int n, m;
    cin >> n >> m;

    wrap w;
    w.edges.resize(6 * m);
    w.flow.resize(6 * m, 0);

    for (int i = 0; i < m; i++) {
        int a, b, l, r;
        cin >> a >> b >> l >> r;
        add_edge(w, 0, 6 * i, l, b);
        add_edge(w, a, 6 * i + 2, r - l, b);
        add_edge(w, a, 6 * i + 4, l, n + 1);
    }

    dinic(0, n + 1, w);

    for (int i = 0; i < m; i++) {
        if (w.flow[6 * i] != w.edges[6 * i].cap) {
            cout << "NO" << endl;
            return 0;
        }
    }

    cout << "YES" << endl;
    for (int i = 0; i < m; i++) {
        cout << w.edges[6 * i].cap + w.flow[6 * i + 2] << endl;
    }
}
