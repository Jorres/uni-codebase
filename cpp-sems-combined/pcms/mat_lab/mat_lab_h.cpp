#include <iostream>
#include <vector>
#include <set>

using namespace std;

bool odd = false;

void find_euler_loop(vector<set<int>>& edges, vector<int>& euler_loop, vector<int>& deg) {
    vector<int> st;
    if (odd) {
        st.push_back(0);
    } else {
        st.push_back(1);
    }

    while (!st.empty()) {
        int v = st.back();
        if (deg[v] == 0) {
            euler_loop.push_back(v);
            st.pop_back();
        } else {
            int first_to = *(edges[v].begin());
            edges[v].erase(first_to);
            edges[first_to].erase(v);

            deg[v]--;
            deg[first_to]--;
            st.push_back(first_to);
        }
    }
}

int main() {
    // freopen("input.in", "r", stdin);
    ios::sync_with_stdio(false);
    int n, m;
    cin >> n >> m;

    vector<set<int>> edges(n + 1);
    vector<int> deg(n + 1, 0);
    for (int i = 0; i < m; ++i) {
        int a, b;
        cin >> a >> b;

        edges[a].insert(b);
        edges[b].insert(a);
        deg[a]++;
        deg[b]++;
    }

    for (int i = 1; i <= n; i++) {
        if (deg[i] % 2 != 0) {
            odd = true;
            break;
        }
    }

    for (int i = 1; i <= n; i++) {
        if (deg[i] % 2 != 0) {
            edges[i].insert(0);
            edges[0].insert(i);
            deg[i]++;
            deg[0]++;
        }
    }

    vector<int> euler_loop;
    find_euler_loop(edges, euler_loop, deg);

    vector<vector<int>> ans(1);

    for (size_t i = 0; i < euler_loop.size(); ++i) {
        if (euler_loop[i] == 0) {
            if (ans.back().size() > 0) {
                ans.push_back(vector<int>());
            }
        } else {
            ans.back().push_back(euler_loop[i]);
        }
    }

    if (ans.back().size() == 0) {
        ans.pop_back();
    }

    cout << ans.size() << endl;
    for (auto& v : ans) {
        for (auto to : v) {
            cout << to << " ";
        }
        cout << endl;
    }
}

