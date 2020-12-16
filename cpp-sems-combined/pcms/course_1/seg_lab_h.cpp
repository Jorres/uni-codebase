#include <iostream>
#include <vector>
#include <cassert>

using namespace std;

typedef pair<bool, int> pbi;

struct node {
	node* left;
	node* right;
	pbi minv;
	node(pbi data) {
		minv = data;
		left = nullptr;
		right = nullptr;
	}
};
int start_n;
node* build(int l, int r) {
	if (l == r) {
		return new node(make_pair(l >= start_n, l));
	}
	int m = (l + r) / 2;
	node* cur = new node(make_pair(true, 0));
	cur->left = build(l, m);
	cur->right = build(m + 1, r);
	cur->minv = min(cur->left->minv, cur->right->minv);
	return cur;
}

void set(node* v, int l, int r, int pos, pbi val) {
	if (pos < l || pos > r) {
		return;
	}
	if (l == r && l == pos) {
		v->minv = val;
		return;
	}

	int m = (l + r) / 2;
	set(v->left, l, m, pos, val);
	set(v->right, m + 1, r, pos, val);
	v->minv = min(v->left->minv, v->right->minv);
}

pbi get(node* v, int l, int r, int al, int ar) {
	if (al > r || ar < l) {
		return make_pair(true, 0);
	}
	if (al <= l && r <= ar) {
		return v->minv;
	}
	int m = (l + r) / 2;
	return min(get(v->left, l, m, al, ar), get(v->right, m + 1, r, al, ar));
}

int get_wrap(node* root, int pos, int n) {
	pbi ans = get(root, 0, n - 1, pos, n - 1);
	if (ans.first == true) {
		ans = get(root, 0, n - 1, 0, pos - 1);
	}
	return ans.second;
}

void clear(node* v) {
	if (v->left != nullptr) {
		clear(v->left);
		clear(v->right);
	}
	delete v;
}

int main() {
	ios::sync_with_stdio(false);
	// freopen("input.in", "r", stdin);
	int n, m;
	cin >> start_n >> m;
	n = (1 << 17);
	node* root = build(0, n - 1);

	string s;
	int pos;
	while (cin >> s >> pos) {
		pos--;
		if (s[1] == 'x') {
			set(root, 0, n - 1, pos, make_pair(false, pos));
		} else {
			assert(s[1] == 'n');
			int ans = get_wrap(root, pos, n);
			cout << ans + 1 << endl;
			set(root, 0, n - 1, ans, make_pair(true, 0));
		}
	}
	clear(root);
}
