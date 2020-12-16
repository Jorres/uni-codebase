#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

typedef unsigned int uint;

uint a, b, cur = 0;

uint nextRand() {
	cur = cur * a + b;
	return cur >> 8;
}

int main() {
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);

	// freopen("input.in", "r", stdin);

	int m, q;
	cin >> m >> q >> a >> b;
	int n = (1 << 24);
	vector<uint> prefix_sums(n + 1, 0);
	for (int i = 0; i < m; i++) {
		int val = nextRand();
		int t1 = nextRand();
		int t2 = nextRand();
		if (t1 > t2) {
			swap(t1, t2);
		}
		prefix_sums[t1] += val;
		prefix_sums[t2 + 1] += (uint)((1LL << 32) - val);
	}

	long long modifier = 0;
	for (int i = 0; i < n; i++) {
		modifier += prefix_sums[i];
		prefix_sums[i] = (uint)(modifier % (1LL << 32));
	}

	for (int i = 1; i < n; i++) {
		prefix_sums[i] += prefix_sums[i - 1];
	}

	uint sum = 0;
	for (int i = 0; i < q; i++) {
		uint t1 = nextRand();
		uint t2 = nextRand();
		if (t1 > t2) {
			swap(t1, t2);
		}
		sum += prefix_sums[t2];
		if (t1 > 0) {
			sum += (uint)((1LL << 32) - prefix_sums[t1 - 1]);
		}
	}

	cout << sum << endl;
}
