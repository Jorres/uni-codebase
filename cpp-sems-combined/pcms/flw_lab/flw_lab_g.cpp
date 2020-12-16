#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>
#include <queue>

using namespace std;

struct edge {
    int from, to, cap, cost;
};

bool push_path(int v, int t, const vector<vector<edge>>& edges,
               vector<vector<int>>& flow, vector<bool>& visited) {
    visited[v] = true;
    if (v == t) {
        return true;
    }

    for (auto& e : edges[v]) {
        int to = e.to;
        if (!visited[to] && e.cap > flow[v][to]) {
            if (push_path(to, t, edges, flow, visited)) {
                flow[v][to]++;
                flow[to][v]--;
                return true;
            }
        }
    }
    return false;
}

void add_edge(vector<vector<edge>>& edges, int from, int to, int cap) {
    edges[from].push_back(edge{from, to, cap, 0});
    edges[to].push_back(edge{to, from, 0, 0});
}

vector<bool> reachable(int s, const vector<vector<edge>>& edges,
               const vector<vector<int>>& flow, vector<bool>& visited) {
    queue<int> q;
    q.push(s);
    vector<bool> new_visited = visited;
    new_visited[s] = true;
    while (!q.empty()) {
        int v = q.front();
        q.pop();
        for (auto& e : edges[v]) {
            int to = e.to;
            if (!new_visited[to] && e.cap - flow[v][to] > 0) {
                q.push(to);
                new_visited[to] = true;
            }
        }
    }
    return new_visited;
}

int main() {
    ios::sync_with_stdio(false);
    // freopen("input.in", "r", stdin);

    int n;
    cin >> n;

    int vertices = 2 * n + 2;
    vector<vector<edge>> edges(vertices);
    vector<edge> total_edges;

    vector<bool> visited(vertices, false);
    for (int i = 0; i <= n; i++) {
        visited[i] = true;
    }

    vector<vector<int>> flow(vertices, vector<int>(vertices, 0));

    for (int i = 1; i <= n; i++) {
        add_edge(edges, 0, i, 1);
        add_edge(edges, i + n, 2 * n + 1, 1);
    }

    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= n; j++) {
            int c;
            cin >> c;
            total_edges.push_back(edge{ i, j + n, 1, c });
        }
    }

    sort(total_edges.begin(), total_edges.end(), [](const auto& a, const auto& b) {
        return a.cost > b.cost;
    });

    int cur_weight = 0;
    int s = 0, t = 2 * n + 1;

    for (auto& e : total_edges) {
        add_edge(edges, e.from, e.to, 1);
        if (visited[e.from] && !visited[e.to]) {
            vector<bool> new_visited = reachable(e.to, edges, flow, visited);
            if (!new_visited[t]) {
                visited = new_visited;
                continue;
            } else {
                visited.assign(vertices, false);
                push_path(s, t, edges, flow, visited);
                visited.assign(vertices, false);
                visited = reachable(0, edges, flow, visited);
                cur_weight++;
            }
        }

        if (cur_weight == n) {
            cout << e.cost << endl;
            return 0;
        }
    }
}
