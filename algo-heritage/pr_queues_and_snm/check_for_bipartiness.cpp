#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

struct node {
	int parent;
	int size;
};

vector<node> a;
vector<int> color;

pair<int, int> get(int p, int clr) {
	if (a[p].parent == p) {
		return make_pair(p, clr);
	}

	return get(a[p].parent, (clr ^ color[p]));
}

void unite(int p1, int p2) {
	pair<int, int> pl = get(p1, 0), pr = get(p2, 0);

	p1 = pl.first, p2 = pr.first;

	if (p1 == p2) {
		return;
	}

	if (a[p1].size > a[p2].size) {
		if (pl.second == pr.second) {
			color[p2] ^= 1;
		}
		a[p1].size += a[p2].size;
		a[p2].parent = p1;
	} else {
		if (pl.second == pr.second) {
			color[p1] ^= 1;
		}
		a[p2].size += a[p1].size;
		a[p1].parent = p2;
	}
}

int main() {
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);
	// freopen("input.txt", "r", stdin);

	int n, m;
	cin >> n >> m;

	a.resize(n);
	color.assign(n, 0);

	for (int i = 0; i < n; i++) {
		a[i].parent = i;
		a[i].size = 1;
	}

	int shift = 0;
	for (int i = 0; i < m; i++) {
		int type, l, r;
		cin >> type >> l >> r;

		// inv - vertex color == xor on way to root
		// look at two: if their clr is different
		// unite the roots and continue
		// else we need to change every single color -
		// xor value on root of subtree

		l = (l - 1 + n + shift) % n;
		r = (r - 1 + n + shift) % n;

		if (type == 0) {
			unite(l, r);
		} else {
			pair<int, int> pl = get(l, 0);
			pair<int, int> pr = get(r, 0);
			if (pl.second == pr.second) {
				cout << "YES" << endl;
				shift = (shift + 1) % n;
			} else {
				cout << "NO" << endl;
			}
		}
	}
}
