#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

using namespace std;

struct line {
	int d, u;
	int x;
	bool start;
};

const int MAXHEIGHT = 22;
const int MAXLEN = (1 << MAXHEIGHT);
const int SHIFT = 1000000;

int tree[2 * MAXLEN] = {0};
int  add[2 * MAXLEN] = {0};
int  pos[2 * MAXLEN] = {0};

int left(int v) {
	return 2 * v;
}

int right(int v) {
	return 2 * v + 1;
}

void push(int v) {
	add[left(v)] += add[v];
	tree[left(v)] += add[v];
	add[right(v)] += add[v];
	tree[right(v)] += add[v];

	add[v] = 0;
}

void t_add(int v, int al, int ar, int vl, int vr, int val) {
	if (al > vr || ar < vl) {
		return;
	}
	if (al <= vl && vr <= ar) {
		add[v] += val;
		tree[v] += val;
		return;
	}
	push(v);

	int m = (vl + vr) / 2;
	t_add(left(v), al, ar, vl, m, val);
	t_add(right(v), al, ar, m + 1, vr, val);
	if (tree[left(v)] > tree[right(v)]) {
		tree[v] = tree[left(v)];
		pos[v] = pos[left(v)];
	} else {
		tree[v] = tree[right(v)];
		pos[v] = pos[right(v)];
	}
}

void shift(line & a) {
	a.d += SHIFT;
	a.u += SHIFT;
	a.x += SHIFT;
}

int main() {
	// freopen("input.in", "r", stdin);
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);
	int n;
	cin >> n;
	vector<line> w;
	for (int i = 0; i < n; i++) {
		line a, b;
		cin >> a.x >> a.d;
		cin >> b.x >> b.u;
		a.u = b.u;
		b.d = a.d;

		a.start = true;
		b.start = false;

		shift(a);
		shift(b);
		b.x++;

		w.push_back(a);
		w.push_back(b);
	}

	sort(w.begin(), w.end(), [](line const &a, line const &b) {
		if (a.x != b.x) {
			return a.x < b.x;
		}

		if (a.start != b.start) {
			return a.start == false;
		}
		return false;
	});

	for (int i = 0; i < MAXLEN; i++) {
		pos[i + MAXLEN] = i;
	}

	for (int i = MAXLEN - 1; i > 0; i--) {
		pos[i] = pos[left(i)];
	}

	/*for (int i = 0; i < (int)w.size(); i++) {
		cout << "At pos " << w[i].x - SHIFT << " "  << w[i].d - SHIFT << " "
		<< w[i].u - SHIFT << " " << (w[i].start ? "true" : "false") << endl;
	}*/

	int ans = -1;
	pair<int, int> ansp = {0, 0};
	int cur = 0;
	while (cur < (int)w.size()) {
		int curx = w[cur].x;
		while (cur < (int)w.size() && w[cur].x == curx) {
			t_add(1, w[cur].d, w[cur].u, 0, MAXLEN - 1, (w[cur].start ? 1 : -1));
			cur++;
		}

		if (tree[1] > ans) {
			ans = tree[1];
			ansp.first = curx;
			ansp.second = pos[1];
		}
	}

	cout << ans << endl << ansp.first - SHIFT << " " << ansp.second - SHIFT << endl;
}
