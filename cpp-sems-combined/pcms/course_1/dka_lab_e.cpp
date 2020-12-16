#include <iostream>
#include <vector>
#include <set>
#include <queue>
#include <map>

using namespace std;

typedef long long LL;

const int MOD = 1000000007;

void convert(int& rn, int& rm, int& rk, int& rl, vector<int>& rt, vector<vector<int>>& rd) {
	rm = 0;

	int n, m, k, l;
	cin >> n >> m >> k >> l;

	set<int> t;
	for (int i = 0; i < k; i++) {
		int x; cin >> x;
		t.insert(x - 1);
	}

	vector<vector<vector<int>>> d(n, vector<vector<int>>(255));
	for (int i = 0; i < m; i++) {
		int start, end; 
		char a;
		cin >> start >> end >> a;
		d[start - 1][a].push_back(end - 1);
	}

	queue<set<int>> q;

	set<int> tmp;
	tmp.insert(0);
	q.push(tmp);

	map<set<int>, int> enm;
	enm.insert(make_pair(tmp, 0));

	rd.push_back(vector<int>());
	while (!q.empty()) {
		set<int> cur = q.front();
		q.pop();
		for (char c = 'a'; c <= 'z'; c++) {
			set<int> to_by_letter;
			for (int fr : cur) {
				for (int to : d[fr][c]) {
					to_by_letter.insert(to);
				}
			}

			if (!enm.count(to_by_letter)) {
				enm.insert(make_pair(to_by_letter, (int)enm.size()));
				rd.push_back(vector<int>());
				q.push(to_by_letter);
			} 

			rd[enm[cur]].push_back(enm[to_by_letter]);
			rm++;
		}
	}

	rn = rd.size();
	rl = l;
	// rm counted
	// rk not needed
	rt.assign(rn, 0);

	for (pair<set<int>, int> p : enm) {
		set<int> const& state = p.first;
		int num = p.second;

		for (int term : t) {
			if (state.count(term)) {
				rt[num] = 1;
				break;
			}
		}
	}

}

int main() {
	freopen("problem5.in", "r", stdin);
	freopen("problem5.out", "w", stdout);
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);

	// freopen("input.in", "r", stdin);


	int n, m, k, l;
	vector<int> t;
	vector<vector<int>> d;

	convert(n, m, k, l, t, d);

	vector<vector<int>> dp(n, vector<int>(l + 1, 0));
	dp[0][0] = 1;

	set<int> prev;
	set<int> now;

	now.insert(0);
	for (int len = 1; len <= l; len++) {
		prev = now;
		now.clear();
		for (int from : prev) {
			for (int j = 0; j < (int)d[from].size(); j++) {
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