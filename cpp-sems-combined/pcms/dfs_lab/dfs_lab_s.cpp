#include <iostream>
#include <vector>
#include <set>

using namespace std;

int main() {
    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    } else {
        freopen("dfs.in", "r", stdin);
        freopen("dfs.out", "w", stdout);
    }

    int n, m;
    cin >> n >> m;

    vector<int> seq(m);
    vector<bool> visited(n, false), is_first(m, false), is_last(m, true);

    for (int i = 0; i < m; i++) {
        cin >> seq[i];
    }

    for (int i = 0; i < m; i++) {
        int cur = seq[i];
        for (int j = i + 1; j < m; j++) {
            if (seq[j] == cur) {
                is_last[i] = false;
                break;
            }
        }
        if (!visited[cur]) {
            is_first[i] = true;
        }
        visited[cur] = true;
    }

    vector<int> stack;
    set<pair<int, int>> ans;
    for (int i = 0; i < m; i++) {
        int v = seq[i];
        if (is_first[i]) {
            for (int from = 0; from < (int)stack.size() - 1; from++) {
                if (stack[from + 1] < v) {
                    ans.insert(make_pair(stack[from], v));
                }
            }
            if (stack.size() > 0) {
                ans.insert(make_pair(stack.back(), v));
            }
            stack.push_back(v);
        }
        if (is_last[i]) {
            stack.pop_back();
        }
    }

    cout << ans.size() << endl;
    for (auto p : ans) {
        cout << p.first << " " << p.second << endl;
    }
}
