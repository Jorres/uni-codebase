#include <iostream>
#include <vector>
#include <climits>

using namespace std;

struct edge {
    int from, to, w;
    edge(int to_, int w_) : to(to_), w(w_) {}
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);
    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    }

    int n, s, f;
    cin >> n >> s >> f;
    s--; f--;
    vector<vector<long long>> edges(n, vector<long long>(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> edges[i][j];
        }
    }

    long long max_value = 1000LL * INT_MAX;
    vector<long long> d(n, max_value);
    d[s] = 0;
    vector<bool> sure(n, false);

    for (int i = 0; i < n; i++) {
        int min_v = -1;
        for (int v = 0; v < n; v++) {
            if (!sure[v] && (min_v == -1 || d[min_v] > d[v])) {
                min_v = v;
            }
        }

        if (d[min_v] < max_value) {
            sure[min_v] = true;
            for (int to = 0; to < n; to++) {
                if (edges[min_v][to] > 0) {
                    d[to] = min(d[to], d[min_v] + edges[min_v][to]);
                }
            }
        } else {
            break;
        }
    }

    if (d[f] == max_value) {
        cout << -1 << endl;
    } else {
        cout << d[f] << endl;
    }
}
