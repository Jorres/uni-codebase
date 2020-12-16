#include <iostream>
#include <vector>

using namespace std;

vector<int> order;
void order_dfs(int v, vector<bool>& used, const vector<vector<int>>& edges) {
    used[v] = true;
    for (int to : edges[v]) {
        if (!used[to]) {
            order_dfs(to, used, edges);
        }
    }
    order.push_back(v);
}

int cur_comp = 1;
void compress_dfs(int v, vector<int>& comp, const vector<vector<int>>& edges) {
    comp[v] = cur_comp;
    for (int to : edges[v]) {
        if (comp[to] == -1) {
            compress_dfs(to, comp, edges);
        }
    }
}

void compress(int n, vector<int>& comp, const vector<vector<int>>& edges) {
    vector<vector<int>> r_edges(n);
    for (int i = 0; i < n; i++) {
        for (int to : edges[i]) {
            r_edges[to].push_back(i);
        }
    }

    vector<bool> used(n, false);
    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            order_dfs(i, used, edges);
        }
    }

    for (int i = n - 1; i >= 0; i--) {
        int v = order[i];
        if (comp[v] == -1) {
            compress_dfs(v, comp, r_edges);
            cur_comp++;
        }
    }
}

void solve_2_sat(int n, vector<vector<int>>& edges,
                        vector<vector<int>>& ports_for_cable) {
    vector<vector<int>> r_edges(2 * n);
    for (int i = 0; i < 2 * n; i++) {
        for (int to : edges[i]) {
            r_edges[to].push_back(i);
        }
    }

    vector<int> comp(2 * n, -1);
    vector<int> answer(n);
    compress(2 * n, comp, edges);
    for (int i = 0; i < n; i++) {
        if (comp[i] == comp[i + n]) {
            cout << "NO" << endl;
            return;
        } else if (comp[i] > comp[i + n]) {
            // true == ports_for_cable[i][0], else [1]
            answer[i] = ports_for_cable[i][0];
        } else {
            answer[i] = ports_for_cable[i][1];
        }
    }

    cout << "YES" << endl;
    for (int to : answer) {
        cout << to + 1 << " ";
    }
}

int negate_event(int n, int event) {
    return (event + n) % (2 * n);
}

int main() {
    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    } else {
        freopen("chip.in", "r", stdin);
        freopen("chip.out", "w", stdout);
    }

    int n;
    cin >> n;
    vector<int> colors(n);
    for (int i = 0; i < n; i++) {
        cin >> colors[i];
    }
    vector<int> cable_for_port(2 * n);
    vector<vector<int>> ports_for_cable(n);
    for (int i = 0; i < 2 * n; i++) {
        cin >> cable_for_port[i];
        cable_for_port[i]--;
        ports_for_cable[cable_for_port[i]].push_back(i);
    }

    vector<vector<int>> edges(2 * n);
    for (int i = 0; i < 2 * n; i++) {
        int i_next = (i + 1) % (2 * n);
        int a = cable_for_port[i];
        int b = cable_for_port[i_next];
        int num_for_a = (i      == ports_for_cable[a][0]) ? 0 : 1;
        int num_for_b = (i_next == ports_for_cable[b][0]) ? 0 : 1;

        if (colors[a] == colors[b]) {
            int event_a_switched = a + n * num_for_a;
            int event_b_switched = b + n * num_for_b;
            edges[event_a_switched].push_back(negate_event(n, event_b_switched));
            edges[event_b_switched].push_back(negate_event(n, event_a_switched));
        }
    }
    solve_2_sat(n, edges, ports_for_cable);
}
