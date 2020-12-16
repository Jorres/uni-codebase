#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>
#include <cassert>

using namespace std;

typedef long long LL;

struct rect {
	rect() {
		l = d = r = u = 0;
	}
	rect(int x1, int y1, int x2, int y2) : l(x1), d(y1), r(x2), u(y2) {}
	int l, d, r, u;
};

typedef vector<vector<rect>> rect_rectangle;
typedef vector<vector<rect_rectangle>> sparse_table;

const int MAX_LEVEL = 6;
const LL MOD = 1000000007LL;

void fill(sparse_table & st, int n, int m) {
	for (int i = 0; i <= MAX_LEVEL; i++) {
		for (int j = 0; j <= MAX_LEVEL; j++) {
			if (i == 0 && j == 0) {
				continue;
			}

			for (int d = 0; d < n; d++) {
				for (int l = 0; l < m; l++) {
					rect prevl, prevr;
					if (i > 0) {
						prevl = st[d][l][i - 1][j];
						if (d + (1 << (i - 1)) >= n) {
							prevr = prevl;
						} else {
							prevr = st[d + (1 << (i - 1))][l][i - 1][j];
						}
					} else if (j > 0) {
						prevl = st[d][l][i][j - 1];
						if (l + (1 << (j - 1)) >= m) {
							prevr = prevl;
						} else {
							prevr = st[d][l + (1 << (j - 1))][i][j - 1];
						}
					} else {
						assert(false);
					}

					st[d][l][i][j].l = max(prevl.l, prevr.l);
					st[d][l][i][j].r = min(prevl.r, prevr.r);
					st[d][l][i][j].d = max(prevl.d, prevr.d);
					st[d][l][i][j].u = min(prevl.u, prevr.u);
				}
			}
		}
	}
}

int four_max(int a, int b, int c, int d) {
	int tmax = max(a, b);
	tmax = max(tmax, c);
	return tmax = max(tmax, d);
}

int four_min(int a, int b, int c, int d) {
	int tmin = min(a, b);
	tmin = min(tmin, c);
	return tmin = min(tmin, d);
}

LL get_ans(sparse_table const & st, vector<int> const & ask, vector<int> const & layer_for_len) {
	int d = ask[0], l = ask[1], u = ask[2], r = ask[3];
	int i_l = layer_for_len[u - d + 1];
	int j_l = layer_for_len[r - l + 1];
	int i_len = (1 << i_l);
	int j_len = (1 << j_l);

	int left_side = four_max(st[d][l][i_l][j_l].l, st[u - i_len + 1][l][i_l][j_l].l,
							 st[d][r - j_len + 1][i_l][j_l].l,
							 st[u - i_len + 1][r - j_len + 1][i_l][j_l].l);

	int right_side = four_min(st[d][l][i_l][j_l].r, st[u - i_len + 1][l][i_l][j_l].r,
							 st[d][r - j_len + 1][i_l][j_l].r,
							 st[u - i_len + 1][r - j_len + 1][i_l][j_l].r);

	int down_side = four_max(st[d][l][i_l][j_l].d, st[u - i_len + 1][l][i_l][j_l].d,
							 st[d][r - j_len + 1][i_l][j_l].d,
							 st[u - i_len + 1][r - j_len + 1][i_l][j_l].d);

	int up_side = four_min(st[d][l][i_l][j_l].u, st[u - i_len + 1][l][i_l][j_l].u,
							 st[d][r - j_len + 1][i_l][j_l].u,
							 st[u - i_len + 1][r - j_len + 1][i_l][j_l].u);
	if (right_side <= left_side || up_side <= down_side) {
		return 0;
	}
	return ((LL)right_side - left_side) * ((LL)up_side - down_side);
}

vector<int> gen_ask(LL A, LL B, LL & prev, int n, int m) {
	vector<int> ans(4);

	for (int i = 0; i < 4; i++) {
		ans[i] = (int)((A * prev + B) % MOD);
		prev = ans[i];
		if (i % 2 == 0) {
			ans[i] %= n;
		} else {
			ans[i] %= m;
		}
	}
	return ans;
}

int main() {
	ios::sync_with_stdio(false);
	// freopen("input.in", "r", stdin);

	int n, m;
	cin >> n >> m;

	sparse_table st(n, vector<rect_rectangle>(m,
		rect_rectangle(MAX_LEVEL + 1, vector<rect>(MAX_LEVEL + 1))));

	vector<int> layer_for_len(max(n, m) + 1, 0);

	int curlayer = 0;
	for (int i = 1; i < (int)layer_for_len.size(); i++) {
		if ((1 << curlayer) * 2 < i) {
			curlayer++;
		}
		layer_for_len[i] = curlayer;
	}
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < m; j++) {
			int x1, y1, x2, y2;
			cin >> x1 >> y1 >> x2 >> y2;
			if (x1 > x2) {
				swap(x1, x2);
			}
			if (y1 > y2) {
				swap(y1, y2);
			}
			st[i][j][0][0] = rect(x1, y1, x2, y2);
		}
	}

	fill(st, n, m);
	int q;
	LL A, B, prev;

	cin >> q >> A >> B >> prev;

	LL sum = 0;
	for (int i = 0; i < q; i++) {
		vector<int> ask = gen_ask(A, B, prev, n, m);

		if (ask[0] > ask[2]) {
			swap(ask[0], ask[2]);
		}
		if (ask[1] > ask[3]) {
			swap(ask[1], ask[3]);
		}

		sum += get_ans(st, ask, layer_for_len);
		sum %= MOD;
	}

	cout << sum << endl;
}
