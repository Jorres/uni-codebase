#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

typedef long long LL;
int n;

struct node {
	int l, r;
	LL val;
};

void update(vector<node>& t, int pos, LL val) {
	pos += n - 1;
	t[pos].val = val;
	if (pos == 0) {
		return;
	}

	while (true) {
		pos = (pos - 1) / 2;
		t[pos].val = t[2 * pos + 1].val + t[2 * pos + 2].val;
		if (pos == 0) {
			break;
		}
	}
}

LL sum(vector<node>& t, int v, int l, int r) {
	if (t[v].r < l || t[v].l > r) {
		return 0;
	}
	if (l <= t[v].l && t[v].r <= r) {
		return t[v].val;
	}
	return sum(t, 2 * v + 1, l, r) + sum(t, 2 * v + 2, l, r);
}

int main() {
	// freopen("input.in", "r", stdin);

	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);

	cin >> n;
	int b = 1;
	while (b < n) {
		b *= 2;
	}
	vector<node> t(2 * b - 1);

	for (int i = 0; i < b; i++) {
		if (i < n) {
			LL x; cin >> x;
			t[b + i - 1].val = x;
		} else {
			t[b + i - 1].val = 0;
		}
		t[b + i - 1].l = i;
		t[b + i - 1].r = i;
	}
	n = b;

	for (int i = n - 2; i >= 0; i--) {
		t[i].val = t[2 * i + 1].val + t[2 * i + 2].val;
		t[i].l = t[2 * i + 1].l;
		t[i].r = t[2 * i + 2].r;
	}

	string s;
	while (cin >> s) {
		if (s == "set") {
			int i;
			LL x;
			cin >> i >> x;
			update(t, i - 1, x);
		} else if (s == "sum") {
			int l, r;
			cin >> l >> r;
			cout << sum(t, 0, l - 1, r - 1) << endl;
		}
	}
}
