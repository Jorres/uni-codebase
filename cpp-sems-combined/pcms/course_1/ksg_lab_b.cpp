#include <iostream>
#include <string>
#include <vector>

using namespace std;

int main() {
	ios::sync_with_stdio(false);
	freopen("input.in", "r", stdin);

	// freopen("epsilon.in", "r", stdin);
	// freopen("epsilon.out", "w", stdout);

	int n;
	char st;
	cin >> n >> st;
	string s;

	getline(cin, s);

	vector<bool> eps((int)'Z' + 1, false);
	vector<pair<char, vector<char>>> rules;

	for (int i = 0; i < n; i++) {

		getline(cin, s);

		if (s.size() == 4) {
			eps[s[0]] = true;
			continue;
		}

		vector<char> to;
		bool satisfies = true;
		for (int j = 1; j < (int)s.size(); j++) {
			if (s[j] >= 'a' && s[j] <= 'z') {
				satisfies = false;
				break;
			}
			if (s[j] >= 'A' && s[j] <= 'Z') {
				to.push_back(s[j]);
			}
		}

		if (satisfies) {
			rules.push_back(make_pair(s[0], to));
		}
	}
	cout << 
	bool converted;
	do {
		converted = false;
		for (int i = 0; i < (int)rules.size(); i++) {
			if (eps[rules[i].first]) {
				continue;
			}

			int j = 0;
			for (; j < (int)rules[i].second.size(); j++) {
				if (!eps[rules[i].second[j]]) {
					break;
				}
			}

			if (j == (int)rules[i].second.size()) {
				converted = true;
				eps[rules[i].first] = true;
			}
		}

	} while (converted);

	for (char z = 'A'; z <= 'Z'; z++) {
		if (eps[z]) {
			cout << z << " ";
		}
	}
}