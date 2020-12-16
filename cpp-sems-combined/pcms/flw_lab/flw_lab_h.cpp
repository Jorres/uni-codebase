#include <iostream>
#include <vector>
#include <cstring>
#include <climits>
#include <set>

using namespace std;

struct edge {
    int to, from;
    long long cap, cost;
};

const int N = 600;
const long long INF = LLONG_MAX;

struct wrap {
    vector<int> g[N];
    vector<edge> edges;
    vector<int> prev_e, prev_v;
    vector<long long> flow;
    vector<long long> dist;
    vector<long long> potential;
    int n;
};

void ford_bellman(int s, wrap& w) {
    w.dist.assign(w.n, INF);
    w.dist[s] = 0;
    w.prev_e.assign(w.n, -1);
    w.prev_v.assign(w.n, -1);

    for (int i = 0; i < w.n - 1; i++) {
        for (int v = 0; v < w.n; v++) {
            for (int ind : w.g[v]) {
                int to = w.edges[ind].to;
                long long cost = w.edges[ind].cost;
                long long difference = w.edges[ind].cap - w.flow[ind];

                if (difference == 0) {
                    continue;
                }

                if (w.dist[v] != INF && (w.dist[to] == INF || w.dist[to] > w.dist[v] + cost)) {
                    w.dist[to] = w.dist[v] + cost;
                    w.prev_e[to] = ind;
                    w.prev_v[to] = v;
                }
            }
        }
    }

    w.potential = w.dist;
}

long long push_path(int t, wrap& w) {
    long long path_cost = 0;
    long long min_push_value = LLONG_MAX;

    for (int v = t; w.prev_v[v] != -1; v = w.prev_v[v]) {
        int ind = w.prev_e[v];
        min_push_value = min(min_push_value, w.edges[ind].cap - w.flow[ind]);
    }

    for (int v = t; w.prev_v[v] != -1; v = w.prev_v[v]) {
        int ind = w.prev_e[v];
        path_cost += min_push_value * w.edges[ind].cost;
        w.flow[ind] += min_push_value;
        w.flow[ind ^ 1] -= min_push_value;
    }

    return path_cost;
}

long long get_shifted_edge(int a, int b, int ind, wrap& w) {
    return w.edges[ind].cost + w.potential[b] - w.potential[a];
}


void uprise(vector<pair<long long, int>>& h, int pos) {
    while (pos != 0) {
        int prev = pos / 2;
        if (h[prev] < h[pos]) {
            swap(h[prev], h[pos]);
            pos = prev;
        } else {
            break;
        }
    }
}

void push(vector<pair<long long, int>>& h, pair<long long, int> elem) {
    h.push_back(elem);
    uprise(h, h.size() - 1);
}

void sieve(vector<pair<long long, int>>& h, int pos) {
    while (true) {
        int left = 2 * pos;
        int right = 2 * pos + 1;
        int to = pos;
        if (left < (int)h.size() && h[pos] > h[left]) {
            to = left;
        }
        if (right < (int)h.size() && h[to] > h[right]) {
            to = right;
        }
        if (pos == to) {
            break;
        }
        swap(h[pos], h[to]);
        pos = to;
    }
}

pair<long long, int> pop(vector<pair<long long, int>> &h) {
    pair<long long, int> ans = h[0];
    h[0] = h.back();
    h.pop_back();
    sieve(h, 0);
    return ans;
}

void djkstra(int s, wrap& w) {
    w.dist.assign(w.n, INF);
    w.prev_e.assign(w.n, -1);
    w.prev_v.assign(w.n, -1);
    w.dist[s] = 0;
    vector<pair<long long, int>> heap;
    push(heap, make_pair(0, s));

    while (!heap.empty()) {
        auto p = pop(heap);

        int v = p.second;
        long long dist = p.first;
        if (dist > w.dist[v]) {
            continue;
        }

        for (int ind : w.g[v]) {
            if (w.flow[ind] == w.edges[ind].cap) {
                continue;
            }

            auto e = w.edges[ind];
            int to = e.to;
            long long cur_dist = dist + get_shifted_edge(v, to, ind, w);
            if (cur_dist < w.dist[to]) {
                w.dist[to] = cur_dist;
                w.prev_v[to] = v;
                w.prev_e[to] = ind;
                push(heap, make_pair(cur_dist, to));
            }
        }
    }

    for (int i = 0; i < w.n; i++) {
        if (w.dist[i] == INF) {
            w.potential[i] = INF;
        } else {
            w.potential[i] += w.dist[i];
        }
    }
}

long long mcmf(int s, int t, wrap& w) {
    long long ans = 0;

    ford_bellman(s, w);

    while (true) {
        ans += push_path(t, w);
        djkstra(s, w);
        if (w.prev_v[t] == -1) {
            break;
        }
    }

    return ans;
}

void add_edge(wrap& w, int from, int to, int ind, int cap, int cost) {
    w.g[from].push_back(ind);
    w.edges[ind].cap = cap;
    w.edges[ind].cost = cost;
    w.edges[ind].to = to;

    w.g[to].push_back(ind + 1);
    w.edges[ind + 1].cap = 0;
    w.edges[ind + 1].cost = -cost;
    w.edges[ind + 1].to = from;
}

int main() {
    ios::sync_with_stdio(false);

    // freopen("input.in", "r", stdin);

    int n, m;
    cin >> n >> m;

    wrap w;
    w.edges.resize(2 * m);
    w.flow.resize(2 * m, 0);
    w.n = n;

    for (int i = 0; i < m; i++) {
        int a, b, cap, c;
        cin >> a >> b >> cap >> c;
        a--; b--;
        add_edge(w, a, b, 2 * i, cap, c);
    }

    cout << mcmf(0, n - 1, w) << endl;
}
