#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

vector<int> order;
void order_dfs(int v, vector<bool>& used, const vector<vector<int>>& edges) {
    used[v] = true;
    for (int to : edges[v]) {
        if (!used[to]) {
            order_dfs(to, used, edges);
        }
    }
    order.push_back(v);
}

int cur_comp = 1;
void compress_dfs(int v, vector<int>& comp, const vector<vector<int>>& edges) {
    comp[v] = cur_comp;
    for (int to : edges[v]) {
        if (comp[to] == -1) {
            compress_dfs(to, comp, edges);
        }
    }
}

int main() {
    freopen("input.in", "r", stdin);
    int n, m;
    cin >> n >> m;

    vector<vector<int>> edges(n), r_edges(n);
    for (int i = 0; i < m; i++) {
        int a, b;
        cin >> a >> b;
        a--; b--;
        edges[a].push_back(b);
        r_edges[b].push_back(a);
    }

    vector<bool> used(n, false);
    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            order_dfs(i, used, edges);
        }
    }

    vector<int> comp(n, -1);
    for (int i = 0; i < n; i++) {
        int v = order[i];
        cout << v + 1 << endl;
        if (comp[v] == -1) {
            compress_dfs(v, comp, edges);
            cur_comp++;
        }
    }

    cout << cur_comp - 1 << endl;
    for (int i = 0; i < n; i++) {
        cout << cur_comp - comp[i] << " ";
    }
    cout << endl;
}
