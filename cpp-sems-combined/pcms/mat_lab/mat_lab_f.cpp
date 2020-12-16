#include <iostream>
#include <vector>

using namespace std;

int cur_research;
void dfs(int v, vector<vector<bool>>& path_exists, vector<bool>& visited,
         const vector<vector<int>>& edges, const vector<int>& matching) {
    visited[v] = true;
    int straight = matching[v];
    for (auto to : edges[straight]) {
        if (!visited[to]) {
            path_exists[cur_research][to] = true;
            dfs(to, path_exists, visited, edges, matching);
        }
    }
}

int main() {
    ios::sync_with_stdio(false);

    int n;
    cin >> n;

    vector<vector<int>> edges(n);
    for (int i = 0; i < n; i++) {
        int k;
        cin >> k;
        for (int j = 0; j < k; j++) {
            int a;
            cin >> a;
            edges[i].push_back(a - 1);
        }
    }

    vector<vector<int>> ans(n);
    vector<int> chosen(n), matching(n);
    for (int i = 0; i < n; i++) {
        cin >> chosen[i];
        chosen[i]--;
        matching[chosen[i]] = i;
        ans[i].push_back(chosen[i]);
    }

    vector<vector<bool>> path_exists(n, vector<bool>(n, false));
    vector<bool> visited;
    for (int i = 0; i < n; i++) {
       cur_research = i;
       visited.assign(n, false);
       dfs(i, path_exists, visited, edges, matching);
    }

    for (int i = 0; i < n; i++) {
        for (auto to : edges[i]) {
            if (chosen[i] != to && path_exists[to][chosen[i]]) {
                ans[i].push_back(to);
            }
        }
    }

    for (int i = 0; i < n; i++) {
        cout << ans[i].size();
        for (auto to : ans[i]) {
            cout << " " << to + 1;
        }
        cout << endl;
    }
}
