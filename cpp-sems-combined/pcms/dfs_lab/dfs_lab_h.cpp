#include <iostream>
#include <vector>
#include <climits>
#include <set>

using namespace std;

struct edge {
    int to;
    int num;

    edge(int to_, int num_) : to(to_), num(num_) {}
};

vector<int> tin, min_time;
set<int> ans;
int timer = 0;

void dfs(int v, vector<vector<edge>>& edges, int p = -1) {
    timer++;
    tin[v] = min_time[v] = timer;

    for (auto to_edge : edges[v]) {
        int to = to_edge.to;
        if (tin[to] == -1) {
            dfs(to, edges, to_edge.num);
            min_time[v] = min(min_time[v], min_time[to]);
            if (min_time[to] > tin[v]) {
                ans.insert(to_edge.num);
            }
        } else if (p != to_edge.num) {
            min_time[v] = min(min_time[v], tin[to]);
        }
    }
}

int main() {
    // freopen("input.in", "r", stdin);
    freopen("bridges.in", "r", stdin);
    freopen("bridges.out", "w", stdout);
    int n, m;
    cin >> n >> m;

    tin.assign(n, -1);
    min_time.assign(n, INT_MAX);

    vector<vector<edge>> edges(n);

    for (int i = 0; i < m; i++) {
        int a, b;
        cin >> a >> b;
        a--; b--;
        edges[a].push_back(edge(b, i));
        edges[b].push_back(edge(a, i));
    }

    for (int i = 0; i < n; i++) {
        if (tin[i] == -1) {
            dfs(i, edges);
        }
    }

    cout << ans.size() << endl;
    for (int i : ans) {
        cout << i + 1 << " ";
    }
}
