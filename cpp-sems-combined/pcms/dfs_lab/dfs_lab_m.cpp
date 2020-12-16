#include <iostream>
#include <vector>
#include <queue>

using namespace std;

int ALPH_SIZE = 26;

void add_edges(const string& a, const string& b,
               vector<vector<int>>& edges, vector<vector<int>>& r_edges) {
    if (a.size() != b.size()) {
        return;
    }
    for (int i = 0; i < (int)a.size(); i++) {
        if (a[i] != b[i]) {
            edges[b[i] - 'a'].push_back(a[i] - 'a');
            r_edges[a[i] - 'a'].push_back(b[i] - 'a');
            return;
        }
    }
}

void top_sort(vector<int>& order, vector<vector<int>>& edges,
              vector<vector<int>>& r_edges) {
    queue<int> q;
    vector<int> deg(ALPH_SIZE);
    for (int i = 0; i < ALPH_SIZE; i++) {
        deg[i] = edges[i].size();
        if (deg[i] == 0) {
             q.push(i);
        }
    }

    while (!q.empty()) {
        int v = q.front();
        q.pop();
        for (int to : r_edges[v]) {
            deg[to]--;
            if (deg[to] == 0) {
                q.push(to);
            }
        }
        order.push_back(v);
    }
}

int main() {
    // freopen("input.in", "r", stdin);
    freopen("tiv.in", "r", stdin);
    freopen("tiv.out", "w", stdout);

    int n;
    cin >> n;

    vector<vector<int>> edges(ALPH_SIZE), r_edges(ALPH_SIZE);
    vector<string> words(n);
    vector<bool> non_zero(ALPH_SIZE, false);

    for (int i = 0; i < n; i++) {
        cin >> words[i];
        non_zero[words[i][0] - 'a'] = true;
    }
    if (words[0].size() == 1) {
        non_zero[words[0][0] - 'a'] = false;
    }

    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            if (words[i] == words[j] || words[i].size() > words[j].size()) {
                cout << "No" << endl;
                return 0;
            }
            add_edges(words[i], words[j], edges, r_edges);
        }
    }

    vector<int> order;
    top_sort(order, edges, r_edges);

    if ((int)order.size() != ALPH_SIZE) {
        cout << "No" << endl;
        return 0;
    }

    vector<int> ans(ALPH_SIZE, -1);
    for (int i = 0; i < ALPH_SIZE; i++) {
        if (!non_zero[i] && edges[i].size() == 0) {
            ans[i] = 0;
            int cur_digit = 1;
            for (int j = 0; j < ALPH_SIZE; j++) {
                int v = order[j];
                if (v != i) {
                    ans[v] = cur_digit++;
                }
            }
            cout << "Yes" << endl;
            for (int j = 0; j < ALPH_SIZE; j++) {
                cout << ans[j] << " ";
            }
            return 0;
        }
    }
    cout << "No" << endl;
}
