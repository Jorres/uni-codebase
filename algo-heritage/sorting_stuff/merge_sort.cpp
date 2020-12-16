#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <cstdlib>
#include <cstring>
#include <vector>

using namespace std;

unsigned int cur = 0;
int a, b;

unsigned int nextRand24() {
	cur = cur * a + b;
	return cur >> 8;
}

long long ans = 0;

void merge_sort(vector<int> &data, int l, int r) {
	if (r - l == 1) {
		return;
	}

	int m = (l + r) / 2;
	merge_sort(data, l, m);
	merge_sort(data, m, r);
	vector<int> temp;
	int i = l, j = m;
	while (i < m && j < r) {
		if (data[i] <= data[j]) {
			temp.push_back(data[i++]);
		} else {
			ans += (long long)m - i;
			temp.push_back(data[j++]);
		}
	}
	while (i < m) {
		temp.push_back(data[i++]);
	}
	while (j < r) {
		temp.push_back(data[j++]);
	}
	for (int i = 0; i < (int)temp.size(); i++) {
		data[l + i] = temp[i];
	}
}

int main() {
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);
	// freopen("input.txt", "r", stdin);

	int n, m;
	cin >> n >> m >> a >> b;
	vector<int> data(n);
	for (int i = 0; i < n; i++) {
		data[i] = nextRand24() % m;
	}
	cout << endl;
	merge_sort(data, 0, n);
	cout << ans << endl;
}


