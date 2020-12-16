#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <climits>

using namespace std;

int n;
vector<int> make_top_sort(const vector<vector<int>>& r_edges, vector<int> out_d) {
    vector<int> ans;
    queue<int> q;

    for (int i = 0; i < n; i++) {
        if (out_d[i] == 0) {
            q.push(i);
        }
    }

    while (!q.empty()) {
        int v = q.front();
        q.pop();
        ans.push_back(v);
        for (auto& from : r_edges[v]) {
            out_d[from]--;
            if (out_d[from] == 0) {
                q.push(from);
            }
        }
    }
    return ans;
}

int main() {
    // freopen("input.in", "r", stdin);
    int m, s;
    cin >> n >> m >> s;
    s--;

    vector<vector<int>> edges(n), r_edges(n);
    vector<int> out_d(n, 0);

    for (int i = 0; i < m; i++) {
        int a, b;
        cin >> a >> b;
        a--; b--;
        out_d[a]++;
        edges[a].push_back(b);
        r_edges[b].push_back(a);
    }

    vector<int> order = make_top_sort(r_edges, out_d);

    vector<int> d(n, -1);

    for (int i = 0; i < n; i++) {
        int v = order[i];
        if (out_d[v] == 0) {
            d[v] = 0;
        } else {
            for (int to : edges[v]) {
                if (d[to] == 0) {
                    d[v] = 1;
                    break;
                }
            }
            if (d[v] == -1) {
                d[v] = 0;
            }
        }
    }

    if (d[s] == 1) {
        cout << "First player wins" << endl;
    } else {
        cout << "Second player wins" << endl;
    }
}
