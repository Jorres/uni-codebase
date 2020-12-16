#include <iostream>
#include <vector>

using namespace std;

int main() {
    ios::sync_with_stdio(false);

    int n;
    cin >> n;
    vector<vector<int>> adj(n, vector<int>(n, 0));
    vector<int> deg(n, 0);

    for (int i = 0; i < n; i++) {
        int k;
        cin >> k;
        for (int j = 0; j < k; j++) {
            int to;
            cin >> to;
            to--;

            adj[i][to]++;
            deg[i]++;
        }
    }

    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += deg[i];
    }
    if (sum % 2 == 1) {
        cout << -1 << endl;
        return 0;
    }

    vector<int> st, ans;
    st.push_back(0);
    while (!st.empty()) {
        int v = st.back();
        if (deg[v] == 0) {
            ans.push_back(v);
            st.pop_back();
        } else {
            for (int i = 0; i < n; i++) {
                if (adj[v][i] > 0) {
                    deg[i]--;
                    deg[v]--;
                    adj[v][i]--;
                    adj[i][v]--;
                    st.push_back(i);
                    break;
                }
            }
        }
    }

    cout << ans.size() - 1 << endl;
    for (auto v : ans) {
        cout << v + 1 << " ";
    }
}
