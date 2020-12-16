#include <iostream>
#include <vector>

using namespace std;

void myassert(bool clause) {
    if (!clause) {
        exit(1);
    }
}

int main() {
    ios::sync_with_stdio(false);
    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    } else {
        freopen("guyaury.in", "r", stdin);
        freopen("guyaury.out", "w", stdout);
    }

    int n;
    cin >> n;
    vector<vector<int>> adj(n, vector<int>(n, 0));
    for (int i = 1; i < n; i++) {
        string cur;
        cin >> cur;
        for (size_t j = 0; j < cur.size(); j++) {
            if (cur[j] == '1') {
                adj[i][j] = 1;
            } else {
                adj[j][i] = 1;
            }
        }
    }

    vector<int> path;
    path.push_back(0);
    for (int cur = 1; cur < n; cur++) {
        size_t j = 0;
        for (; j < path.size(); j++) {
            if (adj[path[j]][cur] == 0) {
                break;
            }
        }
        path.insert(path.begin() + j, cur);
    }

    int fin = n - 1;
    while (fin >= 0 && adj[path[fin]][path[0]] == 0) {
        fin--;
    }

    for (int i = fin; fin < n; i++) {
        int last_to = -1;

        for (int to = 0; to < fin; to++) {
            if (fin == i) { // need to find -> <-
                int next = (to + 1) % fin;
                if (adj[path[i]][path[next]] == 1 && adj[path[to]][path[i]] == 1) {
                    last_to = next;
                }
            } else { // need to find ->
                myassert(i > fin);
                if (adj[path[i]][path[to]] == 1) {
                    last_to = to;
                }
            }
        }

        int to = last_to;
        if (to != -1) {
            vector<int> new_path;
            for (int j = 0; j < to; j++) {
                new_path.push_back(path[j]);
            }
            for (int j = fin; j <= i; j++) {
                new_path.push_back(path[j]);
            }
            for (int j = to; j < fin; j++) {
                new_path.push_back(path[j]);
            }
            for (int j = i + 1; j < n; j++) {
                new_path.push_back(path[j]);
            }
            fin = i + 1;
            path = new_path;
        }
    }

    for (auto v : path) {
        cout << v + 1 << " ";
    }
}
