#pragma GCC optimize("Ofast")
#pragma GCC optimize("unroll-loops")

#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <cstdlib>
#include <cstring>

using namespace std;

const int SIZE = 1e5 + 1, ALPH = (1 << 16);

unsigned int cur = 0;
int a, b;

unsigned int nextRand24() {
	cur = cur * a + b;
	return cur >> 8;
}

unsigned int nextRand32() {
	return (nextRand24() << 8) ^ nextRand24();
}

unsigned int elems[SIZE], res[SIZE];

int main() {
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);
	// freopen("input.txt", "r", stdin);

	int t, n;
	cin >> t >> n;
	int cnt[ALPH], p[ALPH];
	for (int i = 0; i < t; i++) {
		cin >> a >> b;
		for (int j = 0; j < n; j++) {
			elems[j] = nextRand32();
		}

		memset(cnt, 0, sizeof(int) * ALPH);
		memset(p, 0, sizeof(int) * ALPH);
		for (int j = 0; j < n; j++) {
			cnt[elems[j] & (ALPH - 1)]++;
		}

		for (int j = 1; j < ALPH; j++) {
			p[j] = cnt[j - 1] + p[j - 1];
		}
		for (int j = 0; j < n; j++) {
			res[p[elems[j] & (ALPH - 1)]++] = elems[j];
		}
		swap(res, elems);

		memset(p, 0, sizeof(int) * ALPH);
		memset(cnt, 0, sizeof(int) * ALPH);
		for (int j = 0; j < n; j++) {
			cnt[elems[j] >> 16]++;
		}
		for (int j = 1; j < ALPH; j++) {
			p[j] = p[j - 1] + cnt[j - 1];
		}
		for (int j = 0; j < n; j++) {
			res[p[elems[j] >> 16]++] = elems[j];
		}
		swap(res, elems);

		unsigned long long ans = 0;
		for (int j = 1; j <= n; j++) {
			ans += (unsigned long long)(elems[j - 1]) * j;
		}
		cout << ans << '\n';
	}
}


