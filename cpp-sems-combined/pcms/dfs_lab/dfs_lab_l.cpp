#include <iostream>
#include <vector>
#include <climits>

using namespace std;

struct edge {
    int to;
    int num;

    edge(int to_, int num_) : to(to_), num(num_) {}
};

vector<int> tin, up;
int timer = 0;

int max_color = 1;
void custom_push(vector<edge>& st, edge e) {
    // cout << e.num << endl;
    st.push_back(e);
}

void paint_dfs(int v, vector<vector<edge>>& edges, vector<edge>& st,
               vector<int>& edge_color, vector<int>& visited, int p = -1) {
    visited[v] = true;
    for (auto to : edges[v]) {
        if (visited[to.to] == 0) {
            bool should_empty_after = (tin[v] <= up[to.to]);

            int start_st_size = st.size();
            custom_push(st, to);
            paint_dfs(to.to, edges, st, edge_color, visited, v);
            if (should_empty_after) {
                while ((int)st.size() > start_st_size) {
                    edge_color[st.back().num] = max_color;
                    st.pop_back();
                }
                max_color++;
            }
        } else {
            if (to.to != p && tin[v] > tin[to.to]) {
                custom_push(st, to);
            }
        }
    }

    if (p == -1) {
        if (st.size() > 0) {
            while ((int)st.size() > 0) {
                edge_color[st.back().num] = max_color;
                st.pop_back();
            }
            max_color++;
        }
    }
}

void dfs(int v, vector<vector<edge>>& edges, int p = -1) {
    timer++;
    tin[v] = up[v] = timer;

    for (auto to_edge : edges[v]) {
       int to = to_edge.to;
       if (to == p) {
           continue;
       }

       if (tin[to] == -1) {
           dfs(to, edges, v);
           up[v] = min(up[v], up[to]);
       } else {
           up[v] = min(up[v], tin[to]);
       }
    }
}

int main() {
    // freopen("input.in", "r", stdin);
    freopen("biconv.in", "r", stdin);
    freopen("biconv.out", "w", stdout);
    int n, m;
    cin >> n >> m;

    tin.assign(n, -1);
    up.assign(n, INT_MAX);

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

    vector<int> edge_color(m, 0);
    vector<int> visited(n, 0);
    for (int i = 0; i < n; i++) {
        if (visited[i] == 0) {
            vector<edge> st;
            paint_dfs(i, edges, st, edge_color, visited);
        }
    }

    cout << max_color - 1 << endl;
    for (int i = 0; i < m; i++) {
        cout << edge_color[i] << " ";
    }
}
