#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

using namespace std;

int n;
vector<vector<int>> edges, r_edges;
void build_graph(const vector<vector<int>>& adj, int max_weight) {
    edges.assign(n, vector<int>());
    r_edges.assign(n, vector<int>());
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (adj[i][j] <= max_weight) {
                edges[i].push_back(j);
                r_edges[j].push_back(i);
            }
        }
    }
}

vector<int> order;
void order_dfs(int v, vector<bool>& used) {
    used[v] = true;
    for (int to : edges[v]) {
        if (!used[to]) {
            order_dfs(to, used);
        }
    }
    order.push_back(v);
}

int cur_comp;
void compress_dfs(int v, vector<int>& comp) {
    comp[v] = cur_comp;
    for (int to : r_edges[v]) {
        if (comp[to] == -1) {
            compress_dfs(to, comp);
        }
    }
}

bool check_connectivity() {
    vector<bool> used(n, false);
    order.clear();

    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            order_dfs(i, used);
        }
    }

    vector<int> comp(n, -1);
    cur_comp = 1;
    for (int i = n - 1; i >= 0; i--) {
        int v = order[i];
        if (comp[v] == -1) {
            if (cur_comp == 2) {
                return false;
            }
            compress_dfs(v, comp);
            cur_comp++;
        }
    }
    return true;
}

int main() {
    // freopen("input.in", "r", stdin);
    freopen("avia.in", "r", stdin);
    freopen("avia.out", "w", stdout);
    cin >> n;
    vector<vector<int>> adj(n, vector<int>(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> adj[i][j];
        }
    }

    int L = -1, R = 1e9;

    while (R - L > 1) {
        int M = (L + R) / 2;
        // cout << M << endl;
        build_graph(adj, M);
        bool connected = check_connectivity();
        if (connected) {
            R = M;
        } else {
            L = M;
        }
    }
    cout << R << endl;
}
