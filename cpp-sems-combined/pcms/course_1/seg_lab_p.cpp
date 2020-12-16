#include <iostream>
#include <vector>
#include <cassert>

using namespace std;

typedef long long LL;

const int MAXVERTEXES = 300010;
const int MAXHEIGHT = 19;

// vector<int> traversal;
int traversal_size = 1;
vector<pair<int, int>> vertex_to_ind_edges;

vector<LL> leaf, add;

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
	// cout << "enter " << v << endl;
	recalc_lca(v, pre);
	for (int to : edges[v]) {
		if (to != pre) {
			vertex_to_ind_edges[to].first = traversal_size++;
			// traversal.push_back(to);
			dfs(to, v, edges);
		}
	}
	vertex_to_ind_edges[v].second = traversal_size++;
	// traversal.push_back(v);
	// cout << "exit " << v << endl;
}

void push(int v) {
	leaf[2 * v] += add[v];
	add[2 * v] += add[v];
	leaf[2 * v + 1] += add[v];
	add[2 * v + 1] += add[v];
	add[v] = 0;
}

void t_add(int curv, int vl, int vr, int r, LL val) {
	if (r < vl) {
		return;
	}
	if (r >= vr) {
		leaf[curv] += val;
		add[curv] += val;
		return;
	}

	assert(vl != vr);
	push(curv);

	int m = (vl + vr) / 2;
	t_add(2 * curv, vl, m, r, val);
	t_add(2 * curv + 1, m + 1, vr, r, val);
}

LL get_leaf(int curv, int vl, int vr, int pos) {
	assert(vl <= pos && pos <= vr);

	if (vl == vr) {
		return leaf[curv];
	}

	push(curv);

	int m = (vl + vr) / 2;
	if (pos <= m) {
		return get_leaf(2 * curv, vl, m, pos);
	} else {
		return get_leaf(2 * curv + 1, m + 1, vr, pos);
	}
}

int main() {
	// freopen("input.in", "r", stdin);
	ios::sync_with_stdio(false);
	int n;
	cin >> n;
	// creating node 0, and value in vertex == value between this vertex and its parent
	vector<vector<int>> edges(n + 1);
	edges[0].push_back(1);
	edges[1].push_back(0);
	for (int i = 0; i < n - 1; i++) {
		int v, u;
		cin >> v >> u;
		edges[v].push_back(u);
		edges[u].push_back(v);
	}
	vertex_to_ind_edges.resize(n + 1);
	// traversal.push_back(1);
	dfs(1, 0, edges);  // construct traversal, initialize binary rise

	/*for (int t : traversal) {
		cout << t << " ";
	}
	cout << endl;*/

	int L = 0, R = (1 << 20) - 1;

	leaf.resize(1 << 21);
	add.resize(1 << 21);

	int m;
	cin >> m;
	char op;
	int v, u;
	LL val;
	for (int i = 0; i < m; i++) {
		cin >> op >> v;
		if (op == '+') {
			cin >> u >> val;
			int l = lca(v, u);
			t_add(1, L, R, vertex_to_ind_edges[v].first, val);
			t_add(1, L, R, vertex_to_ind_edges[u].first, val);
			t_add(1, L, R, vertex_to_ind_edges[l].first, -val);
			if (l != 1) {
				t_add(1, L, R, vertex_to_ind_edges[br[l][0]].first, -val);
			}
		} else {
			cout << get_leaf(1, L, R, vertex_to_ind_edges[v].first) -
					get_leaf(1, L, R, vertex_to_ind_edges[v].second) << endl;
		}
	}
}
