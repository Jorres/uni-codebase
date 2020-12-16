#include <iostream>
#include <vector>
#include <set>
#include <algorithm>

using namespace std;

int timer = 0;
void order_dfs(int v, const vector<set<int>>& edges, vector<int>& visited, vector<pair<int, int>>& result) {
    visited[v] = true;

    for (int to : edges[v]) {
        if (!visited[to]) {
            order_dfs(to, edges, visited, result);
        }
    }

    timer++;
    result[v].first = timer;
}

int cur_comp = 0;
void compress_dfs(int v, const vector<vector<int>>& edges, vector<int>& visited, vector<int>& comp) {
    comp[v] = cur_comp;
    visited[v] = true;
    for (int to : edges[v]) {
        if (!visited[to]) {
            compress_dfs(to, edges, visited, comp);
        }
    }
}

int main() {
    int n, m;
    cin >> n >> m;

    vector<set<int>> e(n);

    for (int i = 0; i < m; i++) {
        int a, b;
        cin >> a >> b;
		if (a != b) {
            e[a - 1].insert(b - 1);
		}
    }

    vector<int> visited(n, 0);
    vector<pair<int, int>> result;
    for (int i = 0; i < n; i++) {
        result.push_back({0, i});
    }
    for (int i = 0; i < n; i++) {
        if (visited[i] == 0) {
            order_dfs(i, e, visited, result);
        }
    }

    visited.assign(n, 0);
    sort(result.begin(), result.end());

    vector<int> comp(n, -1);

    for (auto p : result) {
        if (visited[p.second] == 0) {
            compress_dfs(p.second, e, visited, comp);
            cur_comp++;
        }
    }

    set<pair<int, int>> result_set;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < e[i].size(); j++) {
            if (comp[i] != comp[e[i][j]]) {
                result_set.insert({i, e[i][j]});
            }
        }
    }

    cout << result_set.size() << endl;
}
