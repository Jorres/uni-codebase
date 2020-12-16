#include <iostream>
#include <vector>
#include <queue>

using namespace std;

const int MOD = 1000000007;
const int WHITE = 0;
const int GRAY = -1;
const int BLACK = -2;

vector<int> t, color, visited, loop_checked, term_color;
vector<vector<int>> d;

int n;

bool term_reachable(int v) {
	if (t[v]) {
		return true;
	}

	term_color[v] = BLACK;

	for (int to : d[v]) {
		if (term_color[to] == WHITE) {
			if (term_reachable(to)) {
				return true;
			}
		}
	}

	return false;
}

bool loops_present(int v) {
	color[v] = GRAY;
	visited.push_back(v);

	for (int to : d[v]) {
		if (color[to] == WHITE) {
			if (loops_present(to)) {
				return true;
			}
		} else if (color[to] == GRAY) {
			if (!loop_checked[to]) {
				term_color.assign(n, 0);
				if (term_reachable(to)) {
					return true;					
				}
				loop_checked[to] = true;
				for (int i = visited.size() - 1; i >= 0 && visited[i] != to; i--) {
					loop_checked[visited[i]] = true;
				}
			}
		} // else fine
	}

	color[v] = BLACK;
	visited.pop_back();
	return false;
}

vector<vector<int>> tmp_data;
vector<int> ref, tmp_color;

void hlp_bfs() {
	queue<int> q;
	q.push(0);
	vector<int> launched(n, 0);
	while (!q.empty()) {
		int f = q.front();
		q.pop();
		for (int to : d[f]) {
			tmp_data[f].push_back(to);
			ref[to]++;
			if (!launched[to]) {
				q.push(to);
				launched[to] = 1;
			}
		}
	}
}

int main() {

	freopen("problem3.in", "r", stdin);
	freopen("problem3.out", "w", stdout);

	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);

	// freopen("input.in", "r", stdin);

	int m, k;
	cin >> n >> m >> k;

	t.assign(n, 0);
	for (int i = 0; i < k; i++) {
		int x;
		cin >> x;
		t[x - 1] = 1;
	}	

	d.resize(n);

	int a, b;
	char ch;
	for (int i = 0; i < m; i++) {
		cin >> a >> b >> ch;
		a--;
		b--;
		d[a].push_back(b);
	}

	tmp_color.assign(n, 0);
	tmp_data.resize(n);
	ref.assign(n, 0);
	hlp_bfs();

	d = tmp_data;

	color.assign(n, 0);
	loop_checked.assign(n, 0);

	if (loops_present(0)) {
		cout << -1;
		return 0;
	}

	vector<int> dp(n, 0);
	dp[0] = 1;

	queue<int> q;
	q.push(0);

	while (!q.empty()) {
		int cur = q.front();
		q.pop();

		for (int to : d[cur]) {
			dp[to] = static_cast<int>(static_cast<long long>(dp[to]) + dp[cur]) % MOD;
			ref[to]--;

			if (ref[to] == 0) {
				q.push(to);
			}
		}
	}

	int answer = 0;
	for (int i = 0; i < n; i++) {
	
		if (t[i] == 1) {
			answer = static_cast<int>((static_cast<long long>(answer) + dp[i]) % MOD);
		}
	}

	cout << answer;
}