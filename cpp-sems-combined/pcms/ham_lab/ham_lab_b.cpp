#include <iostream>
#include <vector>
#include <deque>
#include <cassert>
#include <algorithm>

using namespace std;

void rearrange_deque(const vector<vector<int>>& adj, int n, deque<int>& q) {
    for (int i = 0; i < n + 1; i++) {
        int v = q[0];
        int u = q[1];
        if (adj[v][u] != 1) {
            int s = 2;
            while (true) {
                int nv = q[s];
                int nu = q[s + 1];
                if (adj[v][nv] == 1 && adj[u][nu] == 1) {
                    break;
                }
                s++;
            }
            int j = 0;
            while (1 + j < s - j) {
                swap(q[1 + j], q[s - j]);
                j++;
            }
        }

        q.push_back(v);
        q.pop_front();
    }
}

int main() {
    ios::sync_with_stdio(false);
    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    } else {
        freopen("chvatal.in", "r", stdin);
        freopen("chvatal.out", "w", stdout);
    }

    int n;
    cin >> n;

    vector<vector<int>> adj(n, vector<int>(n, 0));
    vector<int> deg(n, 0);

    for (int i = 1; i < n; i++) {
        string cur;
        cin >> cur;
        for (size_t j = 0; j < cur.size(); j++) {
            if (cur[j] == '1') {
                adj[i][j] = adj[j][i] = 1;
                deg[i]++;
                deg[j]++;
            }
        }
    }

    vector<vector<int>> cl_adj = adj;
    vector<pair<int, int>> added_edges;

    bool added = true;
    for (int i = 0; i < n * n && added; i++) {
        added = false;
        for (int u = 0; u < n; u++) {
            for (int v = u + 1; v < n; v++) {
                if (cl_adj[u][v] == 0 && deg[u] + deg[v] >= n) {
                    cl_adj[u][v] = 1;
                    cl_adj[v][u] = 1;
                    deg[u]++;
                    deg[v]++;
                    added_edges.push_back(make_pair(v, u));
                    added = true;
                }
            }
        }
    }

    for (int i = 0; i < n; i++) {
        if (deg[i] != n - 1) {
            exit(1);
        }
    }

    deque<int> d;
    for (int i = 0; i < n; i++) {
        d.push_back(i);
    }

    reverse(added_edges.begin(), added_edges.end());
    for (auto p : added_edges) {
        int l_v = p.first;
        int r_v = p.second;
        deg[l_v]--;
        deg[r_v]--;
        cl_adj[l_v][r_v] = 0;
        cl_adj[r_v][l_v] = 0;
        rearrange_deque(cl_adj, n, d);
    }

    for (auto v : d) {
        cout << v + 1 << " ";
    }
}
