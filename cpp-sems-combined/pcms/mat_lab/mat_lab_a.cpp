#include <iostream>
#include <vector>

using namespace std;

bool dfs(int v, vector<bool>& visited, vector<int>& matching,
         const vector<vector<int>>& edges) {
    visited[v] = true;
    for (int to : edges[v]) {
        if (matching[to] == -1 || (!visited[matching[to]] &&
                                   dfs(matching[to], visited, matching, edges))) {
            matching[to] = v;
            return true;
        }
    }
    return false;
}

int main() {
    ios::sync_with_stdio(false);
    int n, m, k;
    cin >> n >> m >> k;
    vector<vector<int>> edges(n);
    for (int i = 0; i < k; ++i) {
        int a, b;
        cin >> a >> b;
        a--; b--;

        b += n;
        edges[a].push_back(b);
    }

    vector<int> matching(n + m, -1);
    vector<bool> visited(n, false);

    int ans = 0;
    for (int i = 0; i < n; i++) {
        visited.assign(n, false);
        if (!visited[i] && dfs(i, visited, matching, edges)) {
            ans++;
        }
    }

    cout << ans << endl;
}
