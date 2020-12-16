#include <iostream>
#include <vector>
#include <set>

using namespace std;

void cover_dfs(bool is_left_part, int v, const vector<vector<int>>& edges,
         const vector<int>& matching,
         vector<bool>& l_vis, vector<bool>& r_vis) {
    int n = edges.size();
    if (is_left_part) {
        l_vis[v] = true;
        for (int to : edges[v]) {
            if (!r_vis[to] && matching[to + n] != v) {
                cover_dfs(false, to, edges, matching, l_vis, r_vis);
            }
        }
    } else {
        r_vis[v] = true;
        int from = matching[v + n];
        if (from != -1 && !l_vis[from]) {
            cover_dfs(true, from, edges, matching, l_vis, r_vis);
        }
    }
}

void construct_min_cover(int n, const vector<vector<int>>& edges, const vector<int>& matching,
                         vector<bool>& l_vis, vector<bool>& r_vis) {
    for (int i = 0; i < n; i++) {
        if (matching[i] == -1) {
            cover_dfs(true, i, edges, matching, l_vis, r_vis);
        }
    }
}

bool dfs(int v, vector<bool>& visited, vector<int>& matching,
         const vector<vector<int>>& edges) {
    visited[v] = true;
    for (int to : edges[v]) {
        to += visited.size();
        if (matching[to] == -1 || (!visited[matching[to]] &&
                                   dfs(matching[to], visited, matching, edges))) {
            matching[to] = v;
            return true;
        }
    }
    return false;
}

void construct_max_matching(int n, const vector<vector<int>>& edges,
                            vector<int>&matching) {
    vector<bool> visited;
    for (int i = 0; i < n; i++) {
        visited.assign(n, false);
        if (!visited[i]) {
            dfs(i, visited, matching, edges);
        }
    }
}

void solve() {
    int n, m;
    cin >> m >> n;
    vector<vector<int>> edges(m);
    set<int> fullset, tmp_fullset;
    for (int i = 0; i < n; ++i) {
        fullset.insert(i);
    }

    for (int i = 0; i < m; i++) {
        tmp_fullset = fullset;
        int a;
        while (true) {
            cin >> a;
            if (a == 0) {
                break;
            }
            tmp_fullset.erase(a - 1);
        }
        for (auto to : tmp_fullset) {
            edges[i].push_back(to);
        }
    }

    vector<int> matching(n + m, -1);
    construct_max_matching(m, edges, matching);

    for (int to = m; to < n + m; ++to) {
        if (matching[to] != -1) {
            matching[matching[to]] = to;
        }
    }

    // matching value 0..n - 1, 0..m - 1
    // matching index 0..n + m - 1

    vector<bool> l_vis(m, false);
    vector<bool> r_vis(n, false);
    construct_min_cover(m, edges, matching, l_vis, r_vis);

    vector<int> l_ans, r_ans;
    for (int i = 0; i < m; i++) {
        if (l_vis[i]) {
            l_ans.push_back(i);
        }
    }
    for (int i = 0; i < n; i++) {
        if (!r_vis[i]) {
            r_ans.push_back(i);
        }
    }

    cout << l_ans.size() + r_ans.size() << endl;
    cout << l_ans.size() << " " << r_ans.size() << endl;
    for (auto l : l_ans) {
        cout << l + 1 << " ";
    }
    cout << endl;
    for (auto r : r_ans) {
        cout << r + 1 << " ";
    }
    cout << endl << endl;
}

int main() {
    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    }

    ios::sync_with_stdio(false);
    cin.tie(0); cout.tie(0);

    int k;
    cin >> k;
    while (k > 0) {
        k--;
        solve();
    }
}
