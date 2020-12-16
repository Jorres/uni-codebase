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

int total_comps = 0;
void try_to_add(vector<vector<PII>> &c, int l, int r, int n) {
	if (r < n) {
		c[c.size() - 1].pb(mp(l, r));
		total_comps++;
	}
}

int main() {
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);

	vector<vector<PII>> comps;

	int n;
	cin >> n;

	comps.pb(vector<PII>());
	for (int i = 0; i < 4; i++) {
		try_to_add(comps, 4 * i + 1, 4 * i + 2, n);
		try_to_add(comps, 4 * i, 4 * i + 3, n);
	}

	comps.pb(vector<PII>());
	for (int i = 0; i < 4; i++) {
		try_to_add(comps, 4 * i, 4 * i + 1, n);
		try_to_add(comps, 4 * i + 2, 4 * i + 3, n);
	}

	comps.pb(vector<PII>());
	for (int i = 0; i < 4; i++) {
		try_to_add(comps, 4 * i + 1, 4 * i + 2, n);
	}

	for (int csize = 8; csize <= SIZE; csize *= 2) {
		comps.push_back(vector<PII>());
		for (int block = 0; block < SIZE / csize; block++) {
			int f = block * csize, l = (block + 1) * csize - 1;
			for (int i = f; i < f + csize / 2; i++)
				try_to_add(comps, i, l - (i - f), n);
		}

		for (int length = csize / 2; length > 1; length /= 2) {
			comps.push_back(vector<PII>());
			for (int block = 0; block < SIZE / (length * 2); block++)
				for (int i = 0; i < length; i++)
					try_to_add(comps, block * length * 2 + i, block * length * 2 + i + length, n);
		}

		comps.push_back(vector<PII>());
		for (int i = 0; i < SIZE - 1; i += 2)
			try_to_add(comps, i, i + 1, n);
	}

	assert(comps.size() <= 12);
	cout << n << " " << total_comps << " " << comps.size() << endl;
	for (int i = 0; i < (int)comps.size(); i++) {
		cout << comps[i].size() << " ";
		for (int j = 0; j < (int)comps[i].size(); j++) {
			cout << comps[i][j].first + 1 << " " << comps[i][j].second + 1 << " ";
		}
		cout << endl;
	}
}
