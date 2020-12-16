#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <iomanip>

using namespace std;

const int SIZE = 1000;
const int ALPH = 26;

int main() {
	ios::sync_with_stdio(false);
	// freopen("input.txt", "r", stdin);

	string src[SIZE];
	int n, m, k;
	cin >> n >> m >> k;
	for (int i = 0; i < n; i++) {
		cin >> src[i];
	}

	string res[SIZE];
	int cnt[ALPH], p[ALPH];

	for (int i = m; i > (m - k); i--) {
		for (int j = 0; j < ALPH; j++) {
			cnt[j] = p[j] = 0;
		}
		for (int j = 0; j < n; j++) {
			cnt[src[j][i - 1] - 'a']++;
		}

		for (int j = 1; j < ALPH; j++) {
			p[j] = p[j - 1] + cnt[j - 1];
		}

		for (int j = 0; j < n; j++) {
			char c = src[j][i - 1];
			res[p[c - 'a']++] = src[j];
		}
		swap(res, src);
	}

	for (int i = 0; i < n; i++) {
		cout << src[i] << endl;
	}
}

