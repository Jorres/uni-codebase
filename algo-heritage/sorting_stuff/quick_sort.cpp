#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <vector>
#include <ctime>
#include <cstdlib>
#include <string>
#include <algorithm>
#include <map>

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

int main() {
	ios::sync_with_stdio(false);
	// srand(time(NULL));
	// freopen("input.txt", "r", stdin);
	int n;
	cin >> n;
	vector<int> a(n);
	for (int i = 0; i < n; i++) {
		cin >> a[i];
	}
	quick_sort(a, 0, n - 1);
	for (int i = 0; i < n; i++) {
		cout << a[i] << " ";
	}
}
