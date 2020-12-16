#pragma GCC optimize("O3")
#include <iostream>
#include <vector>
#include <queue>
#include <climits>

using namespace std;

const int max_weight = 10000;

void bucket_advance(int bucket, int increment) {
    return (bucket + increment) % max_weight;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);

    bool debug = true;
    if (debug) {
        freopen("input.in", "r", stdin);
    }

    int n, m;
    cin >> n >> m;
    vector<vector<pair<int, unsigned int>>> edges(n);
    for (int i = 0; i < m; i++) {
        int a, b, w;
        cin >> a >> b >> w;
        a--; b--;
        edges[a].push_back(make_pair(b, w));
        edges[b].push_back(make_pair(a, w));
    }

    vector<vector<int>> buckets(w + 1);
    vector<int> d(n, INT_MAX); // does not work on long long

    buckets[0].push_back(0);

    int cur_bucket = 0;
    int labeled_vertices = 0;
    bool is_labeled(n, false);
    int left_in_buckets = 1;

    // left to calculate d
    while (labeled_vertices != n && left_in_buckets > 0) {
        for (int i = 0; i < buckets[cur_bucket].size(); i++) {
            int v = buckets[cur_bucket][i];
            if (is_labeled(v)) {
                continue;
            }
            if_labeled[v] = true;
            for (auto to : edges[v]) {
                int to_bucket = bucket_advance(cur_bucket, to.second);
                buckets[to_bucket].push_back(to.first);
                left_in_buckets++;
            }
        }
        
        left_in_buckets -= buckets[cur_bucket].size();
        buckets[cur_bucket].clear();
        cur_bucket = bucket_advance(cur_bucket, 1);
    }

    for (auto to_d : d) {
        if (to_d == max_value) {
            cout << -1 << " ";
        } else {
            cout << to_d << " ";
        }
    }
}
