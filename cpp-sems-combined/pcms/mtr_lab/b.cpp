#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

struct edge {
    int to, from;
    long long w;
    int num;

    bool operator<(const edge& a) const {
        return w > a.w;
    }
};

vector<int> p;
vector<int> sz;
int get(int v) {
    if (p[v] == -1) {
        return v;
    }
    return p[v] = get(p[v]);
}

void unite(int a, int b) {
    a = get(a);
    b = get(b);
    if (sz[a] > sz[b]) {
        p[b] = a;
    } else {
        if (sz[b] == sz[a]) {
            sz[b]++;
        }
        p[a] = b;
    }
}

int main() {
    freopen("destroy.in", "r", stdin);
    freopen("destroy.out", "w", stdout);
    // freopen("input.in", "r", stdin);
    int n, m;
    long long s;
    cin >> n >> m >> s;
    p.assign(n, -1);
    sz.assign(n, 1);
    vector<edge> e;
    for (int i = 0; i < m; i++) {
        int a, b;
        long long w;
        cin >> a >> b >> w;
        a--; b--;
        e.push_back(edge{a, b, w, i});
    }

    sort(e.begin(), e.end());

    vector<bool> taken(m, false);
    for (int i = 0; i < m; i++) {
        int v = e[i].from;
        int u = e[i].to;
        // cout << v << " " << u << endl;
        if (get(v) == get(u)) {
            continue;
        }
        unite(v, u);
        taken[i] = true;
    }

    long long cur_sum = 0;
    vector<int> ans_edges;
    int ans = 0;
    for (int i = m - 1; i >= 0; i--) {
        if (!taken[i] && cur_sum + e[i].w <= s) {
            ans++;
            cur_sum += e[i].w;
            ans_edges.push_back(e[i].num);
        }
    }

    cout << ans << endl;
    for (auto edge_ : ans_edges) {
        cout << edge_ + 1 << " ";
    }
}
