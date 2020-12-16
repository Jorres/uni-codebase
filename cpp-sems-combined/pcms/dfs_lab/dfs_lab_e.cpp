#include <iostream>
#include <vector>

using namespace std;

enum {
    WHITE,
    GRAY,
    BLACK
};

vector<int> ans;
int junction = -1;
bool output = true;

int dfs(int v, const vector<vector<int>>& edges, vector<int>& color) {
    color[v] = GRAY;
    for (int to : edges[v]) {
        if (color[to] == GRAY) {
            ans.push_back(v);
            junction = to;
            return -1;
        } else if (color[to] == WHITE) {
            if (dfs(to, edges, color) == -1) {
                if (output) {
                    ans.push_back(v);
                }
                if (v == junction) {
                    output = false;
                }
                return -1;
            }
        }
    }

    color[v] = BLACK;
    return 0;
}

int main() {
    // freopen("input.in", "r", stdin);
    int n, m;
    cin >> n >> m;
    vector<vector<int>> edges(n);
    for (int i = 0; i < m; i++) {
        int a, b;
        cin >> a >> b;
        a--; b--;
        edges[a].push_back(b);
    }

    vector<int> color(n, WHITE);
    for (int i = 0; i < n && ans.size() == 0; i++) {
        if (color[i] == WHITE) {
            dfs(i, edges, color);
        }
    }

    if (ans.size() == 0) {
        cout << "NO" << endl;
    } else {
        cout << "YES" << endl;
        for (int i = (int)ans.size() - 1; i >= 0; i--) {
            cout << ans[i] + 1 << " ";
        }
    }
}
