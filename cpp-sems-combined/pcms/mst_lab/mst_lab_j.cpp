#include <iostream>
#include <vector>
#include <climits>
#include <cassert>

using namespace std;

typedef long long LL;

struct edge {
    int to;
    LL w;
    edge(int to_, LL w_) : to(to_), w(w_) {
    }
};

vector<int> order;
void order_dfs(int v, vector<bool>& used, const vector<vector<edge>>& edges) {
    used[v] = true;
    for (edge e : edges[v]) {
        if (!used[e.to] && e.w == 0) {
            order_dfs(e.to, used, edges);
        }
    }
    order.push_back(v);
}

int cur_comp;
void compress_dfs(int v, vector<int>& comp, const vector<vector<edge>>& edges) {
    comp[v] = cur_comp;
    for (edge e : edges[v]) {
        if (comp[e.to] == -1 && e.w == 0) {
            compress_dfs(e.to, comp, edges);
        }
    }
}

void make_rev_edges(vector<vector<edge>>& edges,
                    vector<vector<edge>>& rev_edges) {
    rev_edges.assign(edges.size(), vector<edge>());
    for (int i = 0; i < (int)edges.size(); i++) {
        for (auto e : edges[i]) {
            rev_edges[e.to].push_back(edge(i, e.w));
        }
    }
}

void compress(int& start_vert, vector<vector<edge>>& edges,
              vector<vector<edge>>& rev_edges) {
    int n = edges.size();
    vector<bool> used(n, false);
    order.clear();
    cur_comp = 0;
    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            order_dfs(i, used, edges);
        }
    }

    vector<int> comp(n, -1);
    for (int i = n - 1; i >= 0; i--) {
        int v = order[i];
        if (comp[v] == -1) {
            compress_dfs(v, comp, rev_edges);
            cur_comp++;
        }
    }
    start_vert = comp[start_vert];
    // cout << start_vert << endl;

//    for (int i = 0; i < n; i++) {
//        cout << i + 1 << " " << comp[i] + 1 << endl;
//    }

    vector<vector<edge>> new_edges(cur_comp);
    for (int i = 0; i < n; i++) {
        for (auto e : edges[i]) {
            int left = comp[i];
            int right = comp[e.to];
            if (left != right) {
                new_edges[left].push_back(edge(right, e.w));
            }
        }
    }

    make_rev_edges(new_edges, rev_edges);
    edges = new_edges;
}

int remaining;
void check_connectivity(int v, const vector<vector<edge>>& edges, vector<bool>& used) {
    used[v] = true;
    remaining--;
    for (auto e : edges[v]) {
        if (!used[e.to]) {
            check_connectivity(e.to, edges, used);
        }
    }
}

void check_zero_connectivity(int v, const vector<vector<edge>>& edges, vector<bool>& used) {
    used[v] = true;
    remaining--;
    for (auto e : edges[v]) {
        if (!used[e.to] && e.w == 0) {
            check_zero_connectivity(e.to, edges, used);
        }
    }
}

void subtract_min(LL& ans, vector<vector<edge>>& edges,
                  vector<vector<edge>>& rev_edges, int start_vert) {
    int n = edges.size();
    for (int i = 0; i < n; i++) {
        if (i == start_vert) {
            continue;
        }
        LL min_edge = INT_MAX;
        for (auto e : rev_edges[i]) {
            min_edge = min(e.w, min_edge);
        }
        if (min_edge != INT_MAX)  {
            ans += min_edge;
        }
        for (auto& e : rev_edges[i]) {
            e.w -= min_edge;
        }
    }

    make_rev_edges(rev_edges, edges);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);

    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    }

    int n, m;
    cin >> n >> m;
    vector<vector<edge>> edges(n), rev_edges(n);
    for (int i = 0; i < m; i++) {
        int a, b;
        LL w;
        cin >> a >> b >> w;
        a--; b--;
        edges[a].push_back(edge(b, w));
        rev_edges[b].push_back(edge(a, w));
    }

    vector<bool> used(n, false);
    remaining = n;
    check_connectivity(0, edges, used);
    if (remaining > 0) {
        cout << "NO" << endl;
        return 0;
    }

    LL ans = 0;
    int start_vert = 0;
    while (true) {
        n = edges.size();
//         cout << "iter" << endl;
//         for (int i = 0; i < n; i++) {
//             for (auto e : edges[i]) {
//                 cout << i + 1 << " " << e.to + 1 << " " << e.w << endl;
//             }
//         }
        subtract_min(ans, edges, rev_edges, start_vert);
//         cout << "iter" << endl;
//         for (int i = 0; i < n; i++) {
//             for (auto e : edges[i]) {
//                 cout << i + 1 << " " << e.to + 1 << " " << e.w << endl;
//             }
//         }
        used.assign(n, false);
        remaining = n;
        check_zero_connectivity(start_vert, edges, used);
        if (remaining == 0) {
            cout << "YES" << endl << ans << endl;
            return 0;
        }
        compress(start_vert, edges, rev_edges);
    }
}
