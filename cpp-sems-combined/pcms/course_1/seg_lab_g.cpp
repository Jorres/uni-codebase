#include <iostream>
#include <vector>
#include <climits>

using namespace std;

typedef long long LL;

const int MAX_HEIGHT = 17;
const int PRE_LEN = (1 << MAX_HEIGHT);

LL  tree[2 * PRE_LEN] = {0};
LL  add[2 * PRE_LEN] = {0};
int set[2 * PRE_LEN] = {0};

int left(int v) {
	return 2 * v;
}

int right(int v) {
	return 2 * v + 1;
}

void push(int v) {
	int l = left(v), r = right(v);
	if (set[v]) {
		tree[l] = tree[r] = tree[v];
		set[l] = set[r] = 1;
	} else {
		tree[l] += add[v];
		tree[r] += add[v];

		add[l] += add[v];
		add[r] += add[v];
	}

	set[v] = 0;
	add[v] = 0;
}

LL get_min(int v, int vl, int vr, int askl, int askr) {
	if (askl >= vr || askr <= vl) {
		return LONG_LONG_MAX;
	}
	if (askl <= vl && vr <= askr) {
		return tree[v];
	}
	push(v);
	int m = (vl + vr) / 2;
	return min(get_min(left(v), vl, m, askl, askr), get_min(right(v), m, vr, askl, askr));
}

void t_set(int v, int vl, int vr, int askl, int askr, LL val) {
	if (askl >= vr || askr <= vl) {
		return;
	}
	if (askl <= vl && vr <= askr) {
		set[v] = 1;
		tree[v] = val;
		return;
	}
	push(v);
	int m = (vl + vr) / 2;
	t_set(left(v), vl, m, askl, askr, val);
	t_set(right(v), m, vr, askl, askr, val);
	tree[v] = min(tree[left(v)], tree[right(v)]);
}

void t_add(int v, int vl, int vr, int askl, int askr, LL val) {
	if (askl >= vr || askr <= vl) {
		return;
	}
	if (askl <= vl && vr <= askr) {
		add[v] += val;
		tree[v] += val;
		return;
	}
	push(v);
	int m = (vl + vr) / 2;
	t_add(left(v), vl, m, askl, askr, val);
	t_add(right(v), m, vr, askl, askr, val);
	tree[v]= min(tree[left(v)], tree[right(v)]);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);

    // freopen("input.in", "r", stdin);
    int n;
    cin >> n;
    for (int i = 0; i < n; i++) {
    	cin >> tree[PRE_LEN + i];
    }

    for (int i = PRE_LEN - 1; i > 0; i--) {
    	tree[i] = min(tree[left(i)], tree[right(i)]);
    }

    string s;
    int l, r;
    while (cin >> s >> l >> r) {
    	LL val;
    	if (s == "min") {
    		cout << get_min(1, 0, PRE_LEN, l - 1, r) << endl;
    	} else if (s == "set") {
    		cin >> val;
    		t_set(1, 0, PRE_LEN, l - 1, r, val);
    	} else if (s == "add") {
    		cin >> val;
    		t_add(1, 0, PRE_LEN, l - 1, r, val);
    	}
    }
}

