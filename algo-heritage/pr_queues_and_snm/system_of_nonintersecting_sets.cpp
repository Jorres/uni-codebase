#define _CRT_SECURE_NO_WARNINGS
#include <vector>
#include <iostream>
#include <algorithm>
#include <cassert>
#include <string>

using namespace std;

typedef pair<int, int> PII;

#define mp make_pair
#define pb push_back

struct node {
	int min, max, amount, parent;
};

vector<node> p;

int get(int v) {
	if (p[v].parent == v)
		return v;
	return (p[v].parent = get(p[v].parent));
}

void union_(int p1, int p2) {
	p1 = get(p1), p2 = get(p2);
	if (p1 == p2) {
		return;
	}
	if (p[p1].amount > p[p2].amount) {
		p[p1].max = max(p[p1].max, p[p2].max);
		p[p1].min = min(p[p1].min, p[p2].min);
		p[p1].amount += p[p2].amount;
		p[p2].parent = p1;
	} else {
		p[p2].max = max(p[p1].max, p[p2].max);
		p[p2].min = min(p[p1].min, p[p2].min);
		p[p2].amount += p[p1].amount;
		p[p1].parent = p2;
	}
}

int main() {
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);

	// freopen("input.txt", "r", stdin);

	int n;
	cin >> n;
	p.resize(n);

	for (int i = 0; i < n; i++) {
		p[i].parent = i;
		p[i].min = i;
		p[i].max = i;
		p[i].amount = 1;
	}

	string s;
	while (cin >> s) {
		int p1, p2;
		if (s == "union") {
			cin >> p1 >> p2;
			union_(p1 - 1, p2 - 1);
		} else if (s == "get") {
			cin >> p1;
			int r = get(p1 - 1);
			cout << p[r].min + 1 << " " << p[r].max + 1 << " " << p[r].amount << endl;
		} else {
			assert(false);
		}
	}
}
