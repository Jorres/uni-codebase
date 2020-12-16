#include <iostream>
#include <vector>

using namespace std;

const int MAXLOG = 19;

vector<pair<int, int>> t_v;
vector<int> v_to_range;
vector<vector<pair<int, int>>> sp;

void dfs(int v, vector<vector<int>> const& e, int h = 0) {
	v_to_range[v] = (int)t_v.size();

	t_v.push_back(make_pair(h, v));
	for (int to : e[v]) {
		dfs(to, e, h + 1);
		t_v.push_back(make_pair(h, v));
	}
}

void build_sp() {
	int n = (int)t_v.size();
	sp.assign(n, vector<pair<int, int>>(MAXLOG));
	for (int i = 0; i < n; i++) {
		sp[i][0] = t_v[i];
	}

	for (int i = 1; i < MAXLOG; i++) {
		for (int j = 0; j < n; j++) {
			int add = (1 << (i - 1));
			sp[j][i] = sp[j][i - 1];
			if (j + add < n && sp[j][i - 1].first > sp[j + add][i - 1].first) {
				sp[j][i] = sp[j + add][i - 1];
			}
		}
	}
}

int lca(int v1, int v2, vector<int> const & layer_for_len) {
	int l = v_to_range[v1], r = v_to_range[v2];
	if (l > r) { swap(l, r); }
	int len = r - l + 1;
	int lr = layer_for_len[len];

	if (sp[l][lr].first < sp[r - (1 << lr) + 1][lr].first) {
		return sp[l][lr].second;
	} else {
		return sp[r - (1 << lr) + 1][lr].second;
	}
}

int main() {
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);
	// freopen("input.in", "r", stdin);
	int n, m;
	cin >> n >> m;
	vector<vector<int>> edges(n);
	for (int i = 0; i < n - 1; i++) {
		int p;
		cin >> p;
		edges[p].push_back(i + 1);
	}

	v_to_range.assign(2 * n, 0);

	dfs(0, edges);
	build_sp();

	vector<int> layer_for_len(4 * n + 1, 0);
	int cursize = 1;
	int curlayer = 0;
	for (int i = 1; i <= 4 * n; i++) {
		if (cursize * 2 < i) {
			cursize *= 2;
			curlayer++;
		}
		layer_for_len[i] = curlayer;
	}

	long long a1, a2;
	long long x, y, z;
	cin >> a1 >> a2 >> x >> y >> z;
	int ask1 = (int)a1, ask2 = (int)a2;
	long long sum = 0;
	for (int i = 0; i < m; i++) {
		int ans = lca(ask1, ask2, layer_for_len);
		a1 = (x * a1 + y * a2 + z) % n;
		a2 = (x * a2 + y * a1 + z) % n;
		ask1 = (int)((a1 + ans) % n);
		ask2 = (int)a2;
		sum += (long long)ans;
	}

	cout << sum << endl;
}

