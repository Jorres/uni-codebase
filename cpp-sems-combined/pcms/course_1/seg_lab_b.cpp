#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

void recalc(int & u, int & v, int ans, int i, int n) {
	u = ((17 * u + 751 + ans + 2 * i) % n) + 1;
	v = ((13 * v + 593 + ans + 5 * i) % n) + 1;
}

int get_layers(int n) {
	int size = 1;
	int ans = 1;
	while (size * 2 < n) {
		size *= 2;
		ans++;
	}
	return ans;
}

int get_ans(vector<vector<int>> const & st, vector<int> const & layer_for_len, int l, int r) {
	if (l > r) {
		swap(l, r);
	}
	int len = r - l + 1;
	int curlayer = layer_for_len[len];
	return min(st[l][curlayer], st[r - (1 << curlayer) + 1][curlayer]);
}

int main() {
	freopen("input.in", "r", stdin);
	int n, m;
	cin >> n >> m;
	int layers = get_layers(n);
	vector<vector<int>> st(n, vector<int>(layers));

	vector<int> layer_for_len(n + 1, 0);
	int cursize = 1;
	int curlayer = 0;
	for (int i = 1; i <= n; i++) {
		if (cursize * 2 < i) {
			cursize *= 2;
			curlayer++;
		}
		layer_for_len[i] = curlayer;
	}

	cin >> st[0][0];
	for (int i = 1; i < n; i++) {
		st[i][0] = (23 * st[i - 1][0] + 21563) % 16714589;
	}

	for (int i = 1; i < layers; i++) {
		for (int j = 0; j < n; j++) {
			if (j + (1 << (i - 1)) >= n) {
				st[j][i] = st[j][i - 1];
			} else {
				st[j][i] = min(st[j][i - 1], st[j + (1 << (i - 1))][i - 1]);
			}
		}
	}

	int u, v, ans = 0;
	cin >> u >> v;
	for (int i = 0; i < m; i++) {
		ans = get_ans(st, layer_for_len, u - 1, v - 1);
		if (i != m - 1) {
			recalc(u, v, ans, (i + 1), n);
		}
	}
	cout << u << " " << v << " " << ans << endl;
}
