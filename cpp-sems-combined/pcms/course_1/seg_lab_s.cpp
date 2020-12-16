#include <iostream>
#include <vector>

using namespace std;

const int MAXVERTEXES = 100000;
const int MAXHEIGHT = 18;

int br[MAXVERTEXES][MAXHEIGHT];
int height[MAXVERTEXES];

void uprise(int & v, int to_h) {
    for (int j = MAXHEIGHT - 1; j >= 0 && height[v] > to_h; j--) {
        if (height[br[v][j]] >= to_h) {
            v = br[v][j];
        }
    }
}

int lca(int v, int u) {
	if (height[v] < height[u]) {
		swap(v, u);
	}
	uprise(v, height[u]);
	if (v == u) {
		return u;
	}
	for (int j = MAXHEIGHT - 1; j >= 0; j--) {
		if (br[v][j] != br[u][j]) {
			v = br[v][j];
			u = br[u][j];
		}
    }
    return br[u][0];
}

void recalc_lca(int v, int pre) {
	height[v] = height[pre] + 1;
	br[v][0] = pre;
	for (int j = 1; j < MAXHEIGHT; j++) {
		br[v][j] = br[br[v][j - 1]][j - 1];
	}
}

void dfs(int v, int pre, vector<vector<int>> const & edges) {
	if (v != 0) {
		recalc_lca(v, pre);
	}
	for (int to : edges[v]) {
		if (to != pre) {
			dfs(to, v, edges);
		}
	}
}

int main() {
	// freopen("input.in", "r", stdin);
	ios::sync_with_stdio(false);

	int n;
	while (cin >> n) {
		if (n == 0) {
			break;
		}

		height[0] = 0;
		for (int j = 0; j < MAXHEIGHT; j++) {
			br[0][j] = 0;
		}

		vector<vector<int>> edges(n);
		for (int i = 0; i < n - 1; i++) {
			int a, b;
			cin >> a >> b;
			a--; b--;
			edges[a].push_back(b);
			edges[b].push_back(a);
		}

		dfs(0, -1, edges);

		int cur_root = 0;

		int m;
		cin >> m;
		char op;
		int v, u;

		for (int i = 0; i < m; i++) {
			cin >> op >> v;
			v--;
			if (op == '?') {
				cin >> u;
				u--;
				int l1 = lca(cur_root, v);
				int l2 = lca(cur_root, u);
				int l3 = lca(v, u);
				cout << (l1 ^ l2 ^ l3) + 1 << endl;
			} else {
				cur_root = v;
			}
		}
	}
}
