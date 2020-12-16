#include <iostream>
#include <vector>

using namespace std;

int main() {
    bool debug = false;
    if (debug) {
        freopen("input.in", "r", stdin);
    }

    int n, m;
    cin >> n >> m;
    vector<vector<int>> ans(n, vector<int>(n, 1e8 + 1));
    for (int i = 0; i < m; i++) {
        int a, b, w;
        cin >> a >> b >> w;
        a--; b--;
        ans[a][b] = w;
    }

    for (int i = 0; i < n; i++) {
        ans[i][i] = 0;
    }

    for (int k = 0; k < n; k++) {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                ans[i][j] = min(ans[i][j], ans[i][k] + ans[k][j]);
            }
        }
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cout << ans[i][j] << " ";
        }
        cout << endl;
    }
}
