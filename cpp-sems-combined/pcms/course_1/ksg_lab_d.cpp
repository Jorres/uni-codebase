#pragma GCC optimize ("03")
#pragma GCC optimize ("unroll-loops")

#include <iostream>
#include <vector>
#include <string>
#include <set>
#include <cassert>

using namespace std;	

struct grammar {
	int n;
	char st;
	set<pair<char, string>> rules;

	void read() {
		cin >> n >> st;
		for (int i = 0; i < n; i++) {
			pair<char, string> elem;
			cin >> elem.first;
			cin >> elem.second;
			
			getline(cin, elem.second);
			
			bool empty = true;
			string real;
			for (char c : elem.second) {
				if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
					empty = false;
					real.push_back(c);
				} 
			}

			if (empty) {
				elem.second = "";
			} else {
				elem.second = real;
			}
			
			rules.insert(elem);
		}
	}
};

int main() {
	ios::sync_with_stdio(false);
	// freopen("input.in", "r", stdin);
	freopen("cf.in", "r", stdin);
	freopen("cf.out", "w", stdout);

	grammar g;
	g.read();

	string w;
	cin >> w;
	int max_length = 5;

	int wl = (int)w.size();

	vector<vector<vector<bool>>> dp(((int)'z' + 1), vector<vector<bool>>(wl + 1, vector<bool>(wl + 1, false)));
	vector<vector<vector<vector<bool>>>> h(g.rules.size(), vector<vector<vector<bool>>>(wl + 1,
			vector<vector<bool>>(wl + 1, vector<bool>(max_length + 1, false))
		));

	int pos = 0;
	for (auto rule : g.rules) {
		if (rule.second.size() == 1 && rule.second[0] >= 'a' && rule.second[0] <= 'z')  {
			for (int i = 0; i < wl; i++) {
				if (w[i] == rule.second[0]) {
					dp[rule.first][i][i + 1] = true;
				}
			}
		} else if (rule.second.size() == 0) {
			for (int i = 0; i <= wl; i++) {
				dp[rule.first][i][i] = true;
			}	
		}

		for (int i = 0; i <= wl; i++) {
			h[pos][i][i][0] = true;
		}

		pos++;
	}

	for (int tmp = 0; tmp < 7; tmp++) {

	for (int m = 1; m <= wl; m++) {
		for (int left = 0; left + m < wl; left++) {
			int right = left + m;

			for (int k = 1; k <= max_length; k++) {
				int rule_pos = 0; 

				for (auto rule : g.rules) {
					char c = rule.second[k - 1];
					for (int r = left; r <= right; r++) {
						if (c >= 'a' && c <= 'z') {
							h[rule_pos][left][right][k] = h[rule_pos][left][right][k] ||
							(h[rule_pos][left][r][k - 1] && 
							r == right - 1 && w[r] == c);
						} else { 
							h[rule_pos][left][right][k] = h[rule_pos][left][right][k] || 
							(h[rule_pos][left][r][k - 1] && dp[c][r][right]);
						}
					}
					rule_pos++;
				}
			}
		}

		for (int i = 0; i <= wl; i++) {
			for (int j = i; j <= wl; j++) {
				int rule_pos = 0;
				for (auto rule : g.rules) {
					dp[rule.first][i][j] = dp[rule.first][i][j] || 
					h[rule_pos][i][j][rule.second.size()];
					rule_pos++;
				}
			}
		}
	}

	}

	cout << (dp[g.st][0][wl] ? "yes" : "no") << endl;
}