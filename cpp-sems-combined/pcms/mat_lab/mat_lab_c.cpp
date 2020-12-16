#include <iostream>
#include <vector>

using namespace std;

void dfs(bool is_left_part, int v, const vector<vector<int>>& edges,
         const vector<int>& matching,
         vector<bool>& l_vis, vector<bool>& r_vis) {
    if (is_left_part) {
        l_vis[v] = true;
        for (int to : edges[v]) {
            if (!r_vis[to] && matching[to] != v) {
                dfs(false, to, edges, matching, l_vis, r_vis);
            }
        }
    } else {
        r_vis[v] = true;
        if (!l_vis[matching[v]] && matching[v] != -1) {
            dfs(true, matching[v], edges, matching, l_vis, r_vis);
        }
    }
}

int main() {
    ios::sync_with_stdio(false);

    int ls, rs;
    cin >> ls >> rs;
    vector<vector<int>> edges(ls);
    for (int i = 0; i < ls; i++) {
        int k;
        cin >> k;
        edges[i].resize(k);
        for (int j = 0; j < k; j++) {
            cin >> edges[i][j];
            edges[i][j]--;
        }
    }

    vector<bool> dfs_start(ls, true);
    vector<int> matching(rs, -1);
    for (int i = 0; i < ls; i++) {
        int match;
        cin >> match;
        if (match != 0) {
            matching[match - 1] = i;
            dfs_start[i] = false;
        }
    }

    vector<bool> l_vis(ls, false);
    vector<bool> r_vis(rs, false);
    for (int i = 0; i < ls; i++) {
        if (dfs_start[i]) {
            dfs(true, i, edges, matching, l_vis, r_vis);
        }
    }

    vector<int> l_ans, r_ans;
    for (int i = 0; i < ls; i++) {
        if (!l_vis[i]) {
            l_ans.push_back(i);
        }
    }
    for (int i = 0; i < rs; i++) {
        if (r_vis[i]) {
            r_ans.push_back(i);
        }
    }

    cout << l_ans.size() + r_ans.size() << endl;
    cout << l_ans.size();
    for (auto l : l_ans) {
        cout << " " << l + 1;
    }
    cout << endl;
    cout << r_ans.size();
    for (auto r : r_ans) {
        cout << " " << r + 1;
    }
    cout << endl;
}
