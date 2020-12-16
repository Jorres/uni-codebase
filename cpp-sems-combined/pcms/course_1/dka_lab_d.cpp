#include <iostream>
#include <vector>
#include <set>

using namespace std;

typedef long long LL;

const int MOD = 1000000007;

int main() {
	// freopen("problem5.in", "r", stdin);
	// freopen("problem5.out", "w", stdout);
	ios::sync_with_stdio(false);

	freopen("input.in", "r", stdin);



	int n, m, k, l;
	vector<int> t;
	vector<vector<int>> d;

	cin >> n >> m >> k >> l;

	vector<int> t(n, 0);
	for (int i = 0; i < k; i++) {
		int x; cin >> x;
		t[x - 1] = 1;
	}	

	vector<vector<int>> d(n);
	int a, b; char ch;
	for (int i = 0; i < m; i++) {
		cin >> a >> b >> ch;
		d[a - 1].push_back(b - 1);
	}

	vector<vector<int>> dp(n, vector<int>(l + 1, 0));
	dp[0][0] = 1;

	set<int> prev;
	set<int> now;

	now.insert(0);
	for (int len = 1; len <= l; len++) {
		prev = now;
		now.clear();
		for (int from : prev) {
			for (int j = 0; j < d[from].size(); j++) {
				int to = d[from][j];
				dp[to][len] += dp[from][len - 1];
				dp[to][len] %= MOD;
				now.insert(to);
			}
		}
	}

	long long answer = 0;
	for (int i = 0; i < n; i++) {	
		if (t[i] == 1) {
			answer += dp[i][l];
			answer %= MOD;
		}
	}

	cout << answer << endl;
}	