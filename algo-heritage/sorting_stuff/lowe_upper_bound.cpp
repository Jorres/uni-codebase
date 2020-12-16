#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

void quick_sort(vector<int> &a, int l, int r) {
	int value = a[(l + 2 * r) / 3];
	int i = l, j = r;
	while (i < j) {
		while (a[i] < value)
			i++;
		while (a[j] > value)
			j--;
		if (i <= j) {
			swap(a[i], a[j]);
			i++;
			j--;
		}
	}
	if (j > l)
		quick_sort(a, l, j);
	if (i < r)
		quick_sort(a, i, r);
}

int lower_bound(const vector<int> &a, int val) {
	int l = 0, r = static_cast<int>(a.size()) - 1;
	// left is strictly less, right is more or equal
	while (r - l > 1) {
		int m = (l + r) / 2;
		if (a[m] >= val) {
			r = m;
		} else {
			l = m;
		}
	}
	if (a[l] < val && a[r] > val) {
		return -1;
	}
	return r;
}

int upper_bound(const vector<int> &a, int val) {
	int l = 0, r = static_cast<int>(a.size()) - 1;
	// left is less or equal, right is strictly less
	while (r - l > 1) {
		int m = (l + r) / 2;
		if (a[m] > val) {
			r = m;
		} else {
			l = m;
		}
	}
	if (a[l] < val && a[r] > val) {
		return -1;
	}
	return l;
}

int main() {
	ios::sync_with_stdio(false);
	// freopen("input.txt", "r", stdin);
	int n;
	cin >> n;
	vector<int> a(n + 2);
	a[0] = INT_MIN; a[n + 1] = INT_MAX;
	for (int i = 1; i <= n; i++) {
		cin >> a[i];
	}
	int k;
	cin >> k;
	for (int j = 0; j < k; j++) {
		int val;
		cin >> val;
		cout << lower_bound(a, val) << " " << upper_bound(a, val) << endl;
	}
}
