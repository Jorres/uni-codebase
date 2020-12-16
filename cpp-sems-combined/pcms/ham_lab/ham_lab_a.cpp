#include <iostream>
#include <vector>
#include <deque>

using namespace std;

int main() {
    ios::sync_with_stdio(false);
    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    } else {
        freopen("fullham.in", "r", stdin);
        freopen("fullham.out", "w", stdout);
    }

    int n;
    cin >> n;

    vector<vector<int>> adj(n, vector<int>(n, 0));

    for (int i = 1; i < n; i++) {
        string cur;
        cin >> cur;
        for (size_t j = 0; j < cur.size(); j++) {
            if (cur[j] == '1') {
                adj[i][j] = adj[j][i] = 1;
            }
        }
    }

    deque<int> q;
    for (int i = 0; i < n; i++) {
        q.push_back(i);
    }

    int iter = n * (n - 1);
    for (int i = 0; i < iter; i++) {
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

    for (auto v : q) {
        cout << v + 1 << " ";
    }
}
