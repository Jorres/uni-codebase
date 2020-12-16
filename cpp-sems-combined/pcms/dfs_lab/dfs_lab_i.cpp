#include <iostream>
#include <vector>

using namespace std;

vector<int> color;
bool dfs(int v, const vector<vector<int>>& edges,
         int cur_color) {
    color[v] = cur_color;

    for (int to : edges[v]) {
        if (color[to] == color[v]) {
            return false;
        }
        if (color[to] == -1) {
            if (!dfs(to, edges, 1 - cur_color)) {
                return false;
            }
        }
    }
    return true;
}

int main() {
    // freopen("input.in", "r", stdin);
    freopen("bipartite.in", "r", stdin);
    freopen("bipartite.out", "w", stdout);
    int n, m;
    cin >> n >> m;

    color.assign(n, -1);

    vector<vector<int>> edges(n);

    for (int i = 0; i < m; ++i) {
        int a, b;
        cin >> a >> b;
        a--; b--;
        edges[a].push_back(b);
        edges[b].push_back(a);
    }

    for (int i = 0; i < n; i++) {
        if (color[i] == -1) {
            if (!dfs(i, edges, 0)) {
                cout << "NO" << endl;
                return 0;
            }
        }
    }

    cout << "YES" << endl;
}
