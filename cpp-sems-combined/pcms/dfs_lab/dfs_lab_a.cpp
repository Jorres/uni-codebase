#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <cassert>

using namespace std;

int main() {
    // freopen("input.in", "r", stdin);
    int n, m;
    cin >> n >> m;

    vector<vector<int>> edges(n), r_edges(n);
    vector<int> out_d(n, 0);
    int out_d_total = 0;
    for (int i = 0; i < m; i++) {
        int a, b;
        cin >> a >> b;
        a--; b--;
        out_d[a]++;
        out_d_total++;
        edges[a].push_back(b);
        r_edges[b].push_back(a);
    }

    queue<int> process;
    vector<int> answer;
    for (int i = 0; i < n; i++) {
        if (edges[i].size() == 0) {
            process.push(i);
        }
    }

    while (!process.empty()) {
        int v = process.front();
        process.pop();
        for (int from : r_edges[v]) {
            out_d[from]--;
            out_d_total--;
            if (out_d[from] == 0) {
                process.push(from);
            }
        }
        answer.push_back(v);
    }

    if (out_d_total != 0) {
        cout << -1 << endl;
    } else {
        assert(answer.size() == size_t(n));
        for (int i = n - 1; i >= 0; i--) {
            cout << answer[i] + 1 << " ";
        }
        cout << endl;
    }
}
