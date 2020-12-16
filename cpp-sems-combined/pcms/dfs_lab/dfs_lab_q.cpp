#include <iostream>
#include <vector>

using namespace std;

struct statement {
    int from, to;
    bool is_liar;

    statement(int from, int to, bool is_liar) :
        from(from), to(to), is_liar(is_liar) { }
};

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

bool satisfied(int n, int n_statements, const vector<statement>& statements) {
    vector<vector<int>> edges(2 * n), r_edges(2 * n);
    for (int i = 0; i < n_statements; i++) {
        int truth_first = statements[i].from;
        int truth_second = statements[i].to;
        int false_first = statements[i].from + n;
        int false_second = statements[i].to + n;

        if (statements[i].is_liar) {
            edges[false_first].push_back(truth_second);
            edges[false_second].push_back(truth_first);
            edges[truth_first].push_back(false_second);
            edges[truth_second].push_back(false_first);
        } else {
            edges[false_first].push_back(false_second);
            edges[truth_second].push_back(truth_first);
            edges[truth_first].push_back(truth_second);
            edges[false_second].push_back(false_first);
        }
    }

    vector<int> comp(2 * n, -1);
    compress(2 * n, comp, edges);
    for (int i = 0; i < n; i++) {
        if (comp[i] == comp[i + n]) {
            return false;
        }
    }

    return true;
}

int main() {
    // freopen("input.in", "r", stdin);
    freopen("truth.in", "r", stdin);
    freopen("truth.out", "w", stdout);
    int n, n_statements;
    cin >> n >> n_statements;
    vector<statement> statements;

    for (int i = 0; i < n_statements; i++) {
        int a, b;
        char c;
        cin >> a >> b >> c;
        a--; b--;
        statements.push_back(statement(a, b, (c == 'L')));
    }

    int L = 0, R = n_statements + 1;
    // left is smaller, right is fine or bigger than answer
    while (R - L > 1) {
        int M = (L + R) / 2;
        if (satisfied(n, M, statements)) {
            L = M;
        } else {
            R = M;
        }
    }

    cout << L << endl;
}
