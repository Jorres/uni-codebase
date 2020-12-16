#include <iostream>
#include <vector>
#include <cstring>
#include <climits>
#include <set>
#include <cassert>

using namespace std;

struct edge {
    int to;
    int cap;
};

const int N = 27772;
const int INF = INT_MAX;

typedef long long LL;

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

int dinic(int s, int t, wrap& w) {
    int ans = 0;

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

void kuhn(int v, const vector<vector<int>>& edges,
          vector<int>& matching, vector<bool>& visited) {
    visited[v] = true;
    for (auto to : edges[v]) {
        assert(matching[to] != -1);  // inv for max parsoch
        int jump = matching[to];
        if (!visited[jump]) {
            // matching[to] = v;
            kuhn(jump, edges, matching, visited);
        }
    }
}

void print_cool_subset(int l, const vector<vector<int>>& edges, vector<int>& matching) {
    int v = 0;
    for (; v < l; v++) {
        if (matching[v] == -1) {
            break;
        }
    }
    assert(v != l);
    vector<bool> visited(l, false);
    kuhn(v, edges, matching, visited);

    vector<int> ans;
    for (int i = 0; i < l; i++) {
        if (visited[i]) {
            ans.push_back(i + 1);
        }
    }
    cout << ans.size() << endl;
    for (auto elem : ans) {
        cout << elem << " ";
    }
    cout << endl << endl;
}

void add_edge(wrap& w, int a, int b, int i, int cap = 1) {
    w.g[a].push_back(i);
    w.g[b].push_back(i + 1);
    w.edges[i].cap += cap;
    w.edges[i + 1].cap = 0;
    w.edges[i].to = b;
    w.edges[i + 1].to = a;
}

int main() {
    ios::sync_with_stdio(false);

    // freopen("input.in", "r", stdin);

    int l, r;
    while (cin >> r >> l) {
        int m;
        cin >> m;
        int cur_edge = 0;
        int total_edges = 2 * (l + r + m) + 1;
        wrap w;
        w.edges.resize(total_edges);
        w.flow.assign(total_edges, 0);

        int term = l + r + 1;
        for (int i = 0; i < m; i++) {
            int a, b;
            cin >> b >> a;
            add_edge(w, a, b + l, 2 * cur_edge);
            cur_edge++;
        }
        for (int i = 1; i <= l; i++) {
            add_edge(w, 0, i, 2 * cur_edge);
            cur_edge++;
        }
        for (int j = l + 1; j <= l + r; j++) {
            add_edge(w, j, term, 2 * cur_edge);
            cur_edge++;
        }

        int mat_size = dinic(0, term, w);

        if (mat_size == l) {
            cout << 0 << endl << endl;  // Hall!
            continue;
        }

        vector<vector<int>> edges(l);
        vector<int> matching(l + r, -1);
        for (int i = 1; i <= l; i++) {
            for (auto ind : w.g[i]) {
                int to = w.edges[ind].to - 1;
                if (to == -1) {
                    continue;
                }
                int from = i - 1;
                // from [0..l) to [l..l + r)
                edges[from].push_back(to);
                if (w.flow[ind] == 1) {
                    matching[to] = from;
                    matching[from] = to;
                }
            }
        }
        // for (int to = l; to < l + r; to++) {
        //     cout << matching[to] << " ";
        // }
        // cout << endl;
        print_cool_subset(l, edges, matching);
    }
}
