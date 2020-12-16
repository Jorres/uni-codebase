#include <iostream>
#include <vector>
#include <set>
#include <queue>
#include <algorithm>
#include <cassert>

using namespace std;

int main() {
    // freopen("input.in", "r", stdin);
    int n, m;
    cin >> n >> m;

    vector<multiset<int>> edges(n), r_edges(n);

    vector<int> out_d(n, 0);
    for (int i = 0; i < m; i++) {
        int a, b;
        cin >> a >> b;
        a--; b--;
        out_d[a]++;
        edges[a].insert(b);
        r_edges[b].insert(a);
    }

    queue<int> process;
    vector<int> answer;
    for (int i = 0; i < n; i++) {
        if (out_d[i] == 0) {
            process.push(i);
        }
    }

    while (!process.empty()) {
        int v = process.front();
        process.pop();
        for (int from : r_edges[v]) {
            out_d[from]--;
            if (out_d[from] == 0) {
                process.push(from);
            }
        }
        answer.push_back(v);
    }

    if ((int)answer.size() != n) {
        cout << "mada" << endl;
        return 0;
    }

    for (int i = n - 1; i > 0; i--) {
        if (edges[answer[i]].count(answer[i - 1]) == 0) {
            cout << "NO" << endl;
            return 0;
        }
    }

    cout << "YES" << endl;
}
