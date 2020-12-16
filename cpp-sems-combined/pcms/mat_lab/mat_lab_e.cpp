#include <iostream>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

struct paint {
    int x, y, type;
    char color;
};

void cover_dfs(bool is_left_part, int v, const vector<set<int>>& edges,
         const vector<int>& matching,
         vector<bool>& l_vis, vector<bool>& r_vis) {
    int n = edges.size();
    if (is_left_part) {
        l_vis[v] = true;
        for (int to : edges[v]) {
            if (!r_vis[to - n] && matching[to] != v) {
                cover_dfs(false, to, edges, matching, l_vis, r_vis);
            }
        }
    } else {
        r_vis[v - n] = true;
        int from = matching[v];
        if (from != -1 && !l_vis[from]) {
            cover_dfs(true, from, edges, matching, l_vis, r_vis);
        }
    }
}

void construct_min_cover(int n, const vector<set<int>>& edges, const vector<int>& matching,
                         vector<bool>& l_vis, vector<bool>& r_vis) {
    for (int i = 0; i < n; i++) {
        if (matching[i] == -1) {
            cover_dfs(true, i, edges, matching, l_vis, r_vis);
        }
    }
}

bool dfs(int v, vector<bool>& visited, vector<int>& matching,
         const vector<set<int>>& edges) {
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

void construct_max_matching(int n, const vector<set<int>>& edges,
                            vector<int>&matching) {
    vector<bool> visited;
    for (int i = 0; i < n; i++) {
        visited.assign(n, false);
        if (!visited[i]) {
            dfs(i, visited, matching, edges);
        }
    }
}

pair<int, int> get_diag(int n, int m, int i, int j) {
    int prim = i + j;
    int tmp = min(i, j);
    i -= tmp;
    j -= tmp;
    int sec;
    if (i >= j) {
        sec = i + m;
    } else {
        sec = m - j;
    }

    return make_pair(prim, sec + n + m - 2);
}

void color_board(int oddity, char to_color, const vector<vector<char>>& board,
                 int n, int m, vector<paint>& ans) {
    vector<pair<int, int>> coord;
    vector<pair<int, int>> cell_for_primary_diag(n + m - 1);
    vector<pair<int, int>> cell_for_secondary_diag(2 * (n + m - 1));

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if ((i + j) % 2 == oddity && board[i][j] != to_color) {
                coord.push_back(make_pair(i, j));
            }
            cell_for_primary_diag[get_diag(n, m, i, j).first] = make_pair(i, j);
            cell_for_secondary_diag[get_diag(n, m, i, j).second] = make_pair(i, j);
        }
    }
    int total_diags = n + m - 1;

    vector<set<int>> edges(total_diags);

    for (int i = 0; i < (int)coord.size(); i++) {
        pair<int, int> diag = get_diag(n, m, coord[i].first, coord[i].second);
        edges[diag.first].insert(diag.second);
    }

    vector<int> matching(2 * total_diags, -1);
    construct_max_matching(total_diags, edges, matching);

    for (int to = total_diags; to < 2 * total_diags; ++to) {
        if (matching[to] != -1) {
            matching[matching[to]] = to;
        }
    }

    vector<bool> l_vis(total_diags, false), r_vis(total_diags, false);
    construct_min_cover(total_diags, edges, matching, l_vis, r_vis);

    for (int i = 0; i < total_diags; i++) {
        if (!l_vis[i]) {
            pair<int, int> sample = cell_for_primary_diag[i];
            ans.push_back(paint{sample.first, sample.second, 0, to_color});
        }
        if (r_vis[i]) {
            pair<int, int> sample = cell_for_secondary_diag[i + total_diags];
            ans.push_back(paint{sample.first, sample.second, 1, to_color});
        }
    }
}

void set_ans(const std::vector<paint>& ans) {
    cout << ans.size() << endl;
    for (int i = 0; i < (int)ans.size(); i++) {
        cout << ans[i].type + 1 << " " << ans[i].x + 1 << " " << ans[i].y + 1 << " " <<
               ans[i].color << endl;
    }
}

int main() {
    ios::sync_with_stdio(false);
    // freopen("input.in", "r", stdin);
    int n, m;
    cin >> n >> m;
    vector<vector<char>> field(n, vector<char>(m));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            cin >> field[i][j];
        }
    }

    vector<paint> ans_1, ans_2;
    color_board(0, 'W', field, n, m, ans_1);
    color_board(1, 'B', field, n, m, ans_1);
    color_board(0, 'B', field, n, m, ans_2);
    color_board(1, 'W', field, n, m, ans_2);
    if (ans_1.size() < ans_2.size()) {
        set_ans(ans_1);
    } else {
        set_ans(ans_2);
    }
}
