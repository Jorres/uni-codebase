#include<iostream>
#include<vector>
#include<cassert>
#include<set>
#include<cmath>

using namespace std;

using matrix = vector<vector<double>>;

void push_edge(vector<vector<int>>& edges, vector<vector<int>>& redges, int a, int b) {
    edges[a].push_back(b);
    edges[b].push_back(a);
}

void topsort(int v, vector<vector<int>>& edges, vector<int>& result, vector<bool>& visited) {
    visited[v] = true;
    for (auto to : edges[v]) {
        topsort(to, edges, result, visited);
    }
    result.push_back(v);
}

void forward_calc(int v, vector<matrix>& values, vector<vector<int>>& redges, string& optype) {
    if (optype == "sum" || optype == "had") {
        int prev = redges[v][0];
        int r = values[prev].size();
        int c = values[prev][0].size();
        values[v] = matrix(r, vector<double>(c, 0));
        for (int i = 0; i < r; i++) {
            for (int j = 0; j < c; j++) {
                for (auto from : redges[v]) {
                    if (optype == "sum") {
                        values[v][i][j] += values[from][i][j];
                    } else {
                        values[v][i][j] *= values[from][i][j];
                    }
                }
            }
        }
    } else if (optype == "mul") {
        int fa = redges[v][0], fb = redges[v][1];
        int a = values[fa].size();
        int b = values[fa][0].size();
        int c = values[fb][0].size();
        values[v] = matrix(a, vector<double>(c, 0));
        for (int i = 0; i < a; i++) {
            for (int j = 0; j < c; j++) {
                for (int w = 0; w < b; w++) {
                    values[v][i][j] += values[fa][i][w] * values[fb][w][j]; 
                }
            }
        }
    } else if (optype == "tnh") {
        values[v] = values[redges[v][0]];
        for (auto& row : values[v]) {
            for (auto& elem : row) {
                elem = tanh(elem);    
            }
        }
    } else {
        assert(optype == "rlu");
        // values[v] = values[redges[v][0]];
        // for (auto& row : values[v]) {
        //     for (auto& elem : row) {
        //         elem = tanh(elem);    
        //     }
        // }
    }
}

int main() {
    // TODO
    // finish relu
    // check forward propagation
    ios::sync_with_stdio(false);

    int n, m, k;
    cin >> n >> m >> k;

    vector<vector<int>> edges(n, vector<int>());
    vector<vector<int>> redges(n, vector<int>());
    set<int> input_indices;
    vector<matrix> values(n);
    vector<matrix> derivs(n);

    vector<string> optype(n);
    vector<double> rlu_par(n, -1);
    for (int i = 0; i < n; i++) {
        string type;
        cin >> type;
        optype[i] = type;
        if (type == "var") {
            int r, c;
            cin >> r >> c;
            values[i] = matrix(r, vector<double>(c, 0));
            input_indices.insert(i);
        } else if (type == "tnh" || type == "rlu") {
            if (type == "rlu") {
                cin >> rlu_par[i];
            }
            int from;
            cin >> from;
            push_edge(edges, redges, from, i);
        } else if (type == "mul") {
            int a, b;
            cin >> a >> b;
            push_edge(edges, redges, a, i);
            push_edge(edges, redges, b, i);
        } else if (type == "sum" || type == "had") {
            int len;
            cin >> len;
            for (int j = 0; j < len; j++) {
                 int from;
                 cin >> from;
                 push_edge(edges, redges, from, i);
            }
        }
    }

    for (auto id : input_indices) {
        for (int i = 0; i < values[id].size(); i++) {
            for (int j = 0; j < values[id][i].size(); j++) {
                cin >> values[id][i][j];
            }
        }
    }

    vector<int> topsort_res;
    vector<bool> visited(n, false);
    for (int i = 0; i < n; i++) {
        if (!visited[i]) {
            topsort(i, edges, topsort_res, visited); 
        }
    }

    for (auto v : topsort_res) {
        if (input_indices.count(v) == 0) {
            forward_calc(v, values, redges, optype[v]);
        }
    }
}
