#pragma GCC optimize("O3")
#include <cstdio>
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <climits>

using namespace std;

int n, mod;


struct matrix {
	matrix() {
		for (int i = 0; i < 2; i++) {
			for (int j = 0; j < 2; j++) {
				data[i][j] = 0;
			}
		}
	}

	int data[2][2];
};

struct node {
	int l, r;
	matrix val;
};

matrix neutral;

matrix mult(matrix const & a, matrix const & b) {
	matrix c;
	for (int i = 0; i < 2; i++) {
		for (int j = 0; j < 2; j++) {
			for (int k = 0; k < 2; k++) {
				c.data[i][j] += a.data[i][k] * b.data[k][j];
			}
			c.data[i][j] %= mod;
		}
	}
	return c;
}

matrix find_mult(vector<node>& t, int v, int l, int r) {
	if (t[v].r < l || t[v].l > r) {
		return neutral;
	}
	if (l <= t[v].l && t[v].r <= r) {
		return t[v].val;
	}
	return mult(find_mult(t, 2 * v + 1, l, r), find_mult(t, 2 * v + 2, l, r));
}

int main() {
	// freopen("input.in", "r", stdin);
	ios::sync_with_stdio(false);
	neutral.data[0][0] = neutral.data[1][1] = 1;

	int m;
	scanf("%d %d %d", &mod, &n, &m);
	int b = 1;
	while (b < n) {
		b *= 2;
	}
	vector<node> t(2 * b - 1);

	for (int i = 0; i < b; i++) {
		if (i < n) {
			matrix x;
			for (int t1 = 0; t1 < 2; t1++) {
				for (int t2 = 0; t2 < 2; t2++) {
					scanf("%d", &x.data[t1][t2]);
				}
			}
			t[b + i - 1].val = x;
		} else {
			t[b + i - 1].val = neutral;
		}
		t[b + i - 1].l = i;
		t[b + i - 1].r = i;
	}
	n = b;

	for (int i = n - 2; i >= 0; i--) {
		t[i].val = mult(t[2 * i + 1].val, t[2 * i + 2].val);
		t[i].l = t[2 * i + 1].l;
		t[i].r = t[2 * i + 2].r;
	}

	for (int i = 0; i < m; i++) {
		int l, r;
		scanf("%d %d", &l, &r);
		matrix x = find_mult(t, 0, l - 1, r - 1);
		for (int t1 = 0; t1 < 2; t1++) {
			for (int t2 = 0; t2 < 2; t2++) {
				printf("%d ", x.data[t1][t2]);
			}
			printf("\n");
		}
		printf("\n");
	}
}

