#include <iostream>
#include <vector>
#include <string>
#include <set>

using namespace std;
int main() {
	ios::sync_with_stdio(false);
	// freopen("input.in", "r", stdin);
	freopen("automaton.in", "r", stdin);
	freopen("automaton.out", "w", stdout);

	vector<vector<pair<char, char>>> trans(255, vector<pair<char, char>>());

	int n;
	string tmp;
	cin >> n >> tmp;
	char start = tmp[0];
	for (int i = 0; i < n; i++) { 
		cin >> tmp;
		char fr = tmp[0];

		string data; 
		cin >> data;
		cin >> data;

		char to = '#';
		if (data.size() == 2) 
			to = data[1];
		trans[fr].push_back(make_pair(data[0], to));
	}

	int m;
	cin >> m;

	vector<bool> vis, old;

	for (int i = 0; i < m; i++) {
		string w;
		cin >> w;

		old.assign((int)'Z' + 1, false);
		old[start] = true;
		for (char smb : w) {
			vis.assign((int)'Z' + 1, false);
			for (char o = 'A'; o <= 'Z'; o++) {
				if (!old[o]) {
					continue;
				}
				for (auto p : trans[o]) {
					if (p.first == smb) {
						vis[p.second] = true;
					}
				}
			}
			old = vis;
		}

		cout << (old['#'] ? "yes" : "no") << endl;
	}
}