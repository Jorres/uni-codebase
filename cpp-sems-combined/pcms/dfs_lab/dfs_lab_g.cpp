#include <iostream>
#include <vector>
#include <climits>
#include <set>

using namespace std;

vector<int> tin, up;
set<int> ans;

int timer = 0;
void dfs(int v, vector<vector<int>>& edges, int p = -1) {
    timer++;
    tin[v] = up[v] = timer;

    int count = 0;
    for (auto to : edges[v]) {
       if (to == p) {
           continue;
       }

       if (tin[to] == -1) {
           dfs(to, edges, v);
           count++;
           up[v] = min(up[v], up[to]);
       } else {
           up[v] = min(up[v], tin[to]);
       }
    }
    if (p == -1 && count >= 2) {
        ans.insert(v);
    }
}

int main() {
    // freopen("input.in", "r", stdin);
    freopen("bipartite.in", "r", stdin);
    freopen("bipartite.out", "w", stdout);
    freopen("points.in", "r", stdin);
    freopen("points.out", "w", stdout);
    int n, m;
    cin >> n >> m;

    tin.assign(n, -1);
    up.assign(n, INT_MAX);

    vector<vector<int>> edges(n);

    for (int i = 0; i < m; i++) {
        int a, b;
        cin >> a >> b;
        a--; b--;
        edges[a].push_back(b);
        edges[b].push_back(a);
    }

    for (int i = 0; i < n; i++) {
        if (tin[i] == -1) {
            dfs(i, edges);
        }
    }

    cout << ans.size() << endl;
    for (int i : ans) {
        cout << i + 1 << " ";
    }
}
