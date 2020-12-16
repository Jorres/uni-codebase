#pragma GCC optimize("O3")
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

typedef long long LL;

struct query {
	int num;
	int l, r;
};

const int SIZE = 1000001;

int data[SIZE] = {0};

LL cur_ans = 0;

void add(LL val) {
	cur_ans += (2 * data[val] + 1) * val;
	data[val]++;
}

void del(LL val) {
	data[val]--;
	cur_ans -= (2 * data[val] + 1) * val;
}

int main() {
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);
	// freopen("input.in", "r", stdin);
	int n, m;
	cin >> n >> m;
	vector<LL> source_data(n);
	for (int i = 0; i < n; i++) {
		cin >> source_data[i];
	}

	vector<query> queries(m);

	for (int i = 0; i < m; i++) {
		cin >> queries[i].l >> queries[i].r;
		queries[i].l--;
		queries[i].r--;
		queries[i].num = i;
	}

	sort(queries.begin(), queries.end(), [](query const & a, query const & b) {
		if (a.l / 512 != b.l / 512) {
			return a.l < b.l;
		}

		return a.r < b.r;
	});

	int a = 1, b = 0;
	vector<LL> result(m);

	for (int i = 0; i < (int)queries.size(); i++) {
		query const & q = queries[i];
		while (a > q.l) {
			a--;
			add(source_data[a]);
		}
		while (b < q.r) {
			b++;
			add(source_data[b]);
		}
		while (a < q.l) {
			del(source_data[a]);
			a++;
		}
		while (b > q.r) {
			del(source_data[b]);
			b--;
		}
		result[q.num] = cur_ans;
	}

	for (auto a : result) {
		cout << a << "\n";
	}
}
