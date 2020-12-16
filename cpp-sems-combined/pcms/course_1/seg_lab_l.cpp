#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

using namespace std;

struct node {
	node * left, * right;
	int lb, rb;

	vector<int> subarray;

	node(int val, int _lb, int _rb) : node(_lb, _rb) {
		subarray.push_back(val);
	}

	node(int _lb, int _rb) {
		lb = _lb;
		rb = _rb;
		left = right = nullptr;
	}
};

unsigned int a__, b__;
unsigned int cur__ = 0;
unsigned int nextRand(int sh) {
	cur__ = cur__ * a__ + b__;
	return cur__ >> (32 - sh);
}


node* build(vector<int> const & f, int l, int r) {
	if (l == r) {
		return new node(f[l], l, r);
	}

	int m = (l + r) / 2;

	node* cur = new node(l, r);

	node* left = build(f, l, m);
	node* right = build(f, m + 1, r);

	cur->left = left;
	cur->right = right;

	cur->subarray.resize(r - l + 1);

	merge(left->subarray.begin(), left->subarray.end(),
		right->subarray.begin(), right->subarray.end(), cur->subarray.begin());
	return cur;
}

pair<int, int> get_indexes(vector<int> const & f, int x, int y) {
	return make_pair(lower_bound(f.begin(), f.end(), x) - f.begin(),
					 upper_bound(f.begin(), f.end(), y) - f.begin() - 1);
}

void clear(node * v) {
	if (v->left != nullptr) {
		clear(v->left);
	}
	if (v->right != nullptr) {
		clear(v->right);
	}
	delete v;
}

int get_ans(node * v, int l, int r, int x, int y) {
	if (v->rb < l || v->lb > r) {
		return 0;
	}


	if (l <= v->lb && v->rb <= r) {
		pair<int, int> ind = get_indexes(v->subarray, x, y);
		return ind.second - ind.first + 1;
	}

	return get_ans(v->left, l, r, x, y) + get_ans(v->right, l, r, x, y);
}

int main() {
	// freopen("input.in", "r", stdin);
	int q;
	cin >> q >> a__ >> b__;

	vector<int> f(1 << 17);
	for (int i = 0; i < (1 << 17); i++) {
		f[i] = nextRand(24);
	}

	node * root = build(f, 0, (1 << 17) - 1);

	unsigned int ans = 0;
	for (int i = 0; i < q; i++) {
		int l = nextRand(17);
		int r = nextRand(17);
		if (l > r) { swap(l, r); }

		int x = nextRand(24);
		int y = nextRand(24);
		if (x > y) { swap(x, y); }

		unsigned int tmp = static_cast<unsigned int>(get_ans(root, l, r, x, y));
		ans += tmp;
		b__ += tmp;
	}

	cout << ans << endl;
	clear(root);
}
