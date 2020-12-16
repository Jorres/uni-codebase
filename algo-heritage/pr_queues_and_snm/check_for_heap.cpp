#define _CRT_SECURE_NO_WARNINGS
#include <vector>
#include <iostream>
#include <algorithm>
#include <cassert>

using namespace std;

typedef pair<int, int> PII;

const int SIZE = 16;

#define mp make_pair
#define pb push_back

int main() {
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);

	// freopen("input.txt", "r", stdin);

	int n;
	cin >> n;
	vector<int> a(n);
	for (int i = 0; i < n; i++) {
		cin >> a[i];
	}

	for (int i = 0; i < n; i++) {
		if (2 * i + 1 < n) {
			if (a[i] > a[2 * i + 1]) {
				cout << "NO" << endl;
				return 0;
			}
		}
		if (2 * i + 2 < n) {
			if (a[i] > a[2 * i + 2]) {
				cout << "NO" << endl;
				return 0;
			}
		}
	}
	cout << "YES" << endl;
}
