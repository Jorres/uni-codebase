#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <algorithm>
#include <cassert>
#include <vector>
#include <string>

using namespace std;

struct node {
	int p, size, min;
};

int get(vector<node> &a, int v) {
	if (a[v].p == v)
		return v;
	return a[v].p = get(a, a[v].p);
}

void unite(vector<node> &a, int v, int u) {
	v = get(a, v), u = get(a, u);

	if (v == u) {
		return;
	}

	if (a[v].size > a[u].size) {
		a[u].p = v;
		a[v].size += a[u].size;
		a[v].min = min(a[v].min, a[u].min);
	} else {
		a[v].p = u;
		a[u].size += a[v].size;
		a[u].min = min(a[v].min, a[u].min);
	}
}


int main() {
	ios::sync_with_stdio(0);
	// freopen("input.txt", "r", stdin);

	int n, q;
	cin >> n >> q;
	vector<node> seg(n), real(n);
	for (int i = 0; i < n; i++) {
		seg[i].min = seg[i].p = real[i].min = real[i].p = i;
		seg[i].size = real[i].size = 1;
	}

	for (int i = 0; i < q; i++) {
		int type, x, y;
		cin >> type >> x >> y;
		x--; y--;
		if (type == 1) {
			unite(real, x, y);
		} else if (type == 2) {
			int curv = seg[get(seg, x)].min + seg[get(seg, x)].size;
			while (curv <= y) {
				unite(seg, x, curv);
				unite(real, x, curv);
				curv = seg[get(seg, x)].min + seg[get(seg, x)].size;
			}
		} else {
			cout << (get(real, x) == get(real, y) ? "YES" : "NO") << endl;
		}
	}
}
