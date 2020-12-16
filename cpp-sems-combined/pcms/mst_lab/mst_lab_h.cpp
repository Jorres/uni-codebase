#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

using namespace std;

struct edge {
    int st, fn, w;
};

vector<int> p;
vector<int> rng;

int get(int a) {
    if (p[a] == a) {
        return a;
    }
    return p[a] = get(p[a]);
}

void unite(int a, int b) {
    a = get(a);
    b = get(b);

    if (rng[a] > rng[b]) {
        swap(a, b);
    }
    p[a] = b;
    if (rng[b] == rng[a]) {
        rng[b]++;
    }
}

int m;
bool able_to_mst(int max_diff, const vector<vector<bool>>& connect,
                 const vector<edge>& edges) {
    int l = 0, r = 0;
    while (r < m && edges[r].w - edges[l].w <= max_diff) {
        r++;
    }
    r--;

    while (true) {
        if (connect[l][r]) {
            return true;
        }
        if (r == m - 1) {
            return false;
        }

        if (edges[l].w == edges[m - 1].w) {
            return false;
        }

        while (edges[l].w == edges[l + 1].w) {
            l++;
        }
        l++;

        while (r < m && edges[r].w - edges[l].w <= max_diff) {
            r++;
        }
        r--;
    }
}

int main() {
    ios::sync_with_stdio(false);
    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    }

    int n;
    cin >> n >> m;
    if (m == 0) {
        cout << "NO" << endl;
        return 0;
    }

    vector<edge> edges(m);
    for (int i = 0; i < m; i++) {
        cin >> edges[i].st >> edges[i].fn >> edges[i].w;
        edges[i].st--;
        edges[i].fn--;
    }

    sort(edges.begin(), edges.end(), [](const edge& a, const edge& b) {
        return a.w < b.w;
    });

    vector<vector<pair<int, int>>> lst(n);
    for (int i = 0; i < m; i++) {
        int a = edges[i].st;
        int b = edges[i].fn;
        lst[a].push_back(make_pair(b, i));
        lst[b].push_back(make_pair(a, i));
    }

    vector<vector<bool>> from_to(m, vector<bool>(m, false));
    int comps = n;

    p.resize(n);
    rng.resize(n);
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            p[j] = j;
            rng[j] = 0;
        }
        comps = n;

        for (int j = i; j < m; j++) {
            int a = edges[j].st;
            int b = edges[j].fn;
            if (get(a) != get(b)) {
                comps--;
                unite(a, b);
            }
            from_to[i][j] = (comps == 1);
        }
    }

    if (!from_to[0][m - 1]) {
        cout << "NO" << endl;
        return 0;
    }

    int L = -1, R = 2e9 + 1;
    while (R - L > 1) {
        int M = L + (R - L) / 2;
        if (able_to_mst(M, from_to, edges)) {
            R = M;
        } else {
            L = M;
        }
    }
    cout << "YES" << endl << R << endl;
}
