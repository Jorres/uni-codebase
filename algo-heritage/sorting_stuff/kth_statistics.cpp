#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int find_kth(vector<int> &a, int k, int l, int r) {
	if (r - 1 == l) {
		return a[l];
	}

	int value = a[(l + r) / 2];
	int i = l, j = r - 1;
	while (i <= j) {
		while (i < r && a[i] < value)
			i++;
		while (j >= l && a[j] > value)
			j--;
		if (i <= j) {
			swap(a[i], a[j]);
			i++;
			j--;
		}
	}
	if (i == r) {
		i--;
	}
	if (j < k) {
		return find_kth(a, k, j + 1, r);
	}
	return find_kth(a, k, l, i);
}

int main() {
	ios::sync_with_stdio(false);
	// freopen("input.txt", "r", stdin);

	int n, k;
	int A, B, C;
	cin >> n >> k;
	k--;
	vector<int> a(n);

	cin >> A >> B >> C >> a[0] >> a[1];
	for (int i = 2; i < n; i++) {
		a[i] = a[i - 2] * A + a[i - 1] * B + C;
	}

	cout << find_kth(a, k, 0, n) << endl;
}

