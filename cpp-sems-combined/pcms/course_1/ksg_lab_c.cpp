#include <iostream>
#include <vector>

using namespace std;

void compress(int n, vector<pair<char, vector<char>>>& rules, char start) {
	vector<bool> generating((int)'Z' + 1, false);

	int sz = (int)rules.size();

	for (int i = 0; i < sz; i++) {
		bool generates = true;
		for (char r : rules[i].second) {
			if (r >= 'A' && r <= 'Z') {
				generates = false;
				break;
			}
		}
		if (generates) {
			generating[rules[i].first] = true;
		}
	}

	bool found_new;
	do {
		found_new = false;
		for (int i = 0; i < sz; i++) {
			if (generating[rules[i].first]) {
				continue;
			}
			int j = 0;
			for (; j < (int)rules[i].second.size(); j++) {
				int rch = rules[i].second[j];
				if (rch >= 'A' && rch <= 'Z' && !generating[rch]) {
					break;
				}
			}
			if (j == (int)rules[i].second.size()) {
				generating[rules[i].first] = true;
				found_new = true;
			}
		}
	} while (found_new);

	int shift = 0;
	for (int i = 0; i < (int)rules.size(); i++) {
		bool contains_nong = !generating[rules[i].first];

		for (int j = 0; j < (int)rules[i].second.size(); j++) {
			char r = rules[i].second[j];
			if (r >= 'A' && r <= 'Z' && !generating[r]) {
				contains_nong = true;
				break;
			}
		}
		if (contains_nong) { // has non-generating
			shift++;
		} else {
			rules[i - shift] = rules[i];
		}
	}
	for (int i = 0; i < shift; i++) {
		rules.pop_back();
	}

	// find non_reachable
	vector<bool> reachable((int)'Z' + 1, false);

	for (int i = 0; i < (int)rules.size(); i++) {
		if (rules[i].first == start) {
			reachable[start] = true;
		}
	}

	do {
		found_new = false;

		for (int i = 0; i < sz; i++) {
			if (!reachable[rules[i].first]) {
				continue;
			}
			int j = 0;
			for (; j < (int)rules[i].second.size(); j++) {
				int rch = rules[i].second[j];
				if (rch >= 'A' && rch <= 'Z' && !reachable[rch]) {
					reachable[rch] = true;
					found_new = true;
				}
			}
		}
	} while (found_new);

	shift = 0;
	for (int i = 0; i < (int)rules.size(); i++) {
		bool contains_nond = !reachable[rules[i].first];

		for (int j = 0; j < (int)rules[i].second.size(); j++) {
			char r = rules[i].second[j];
			if (r >= 'A' && r <= 'Z' && !reachable[r]) {
				contains_nond = true;
				break;
			}
		}
		if (contains_nond) { // has non-generating
			shift++;
		} else {
			rules[i - shift] = rules[i];
		}
	}
	for (int i = 0; i < shift; i++) {
		rules.pop_back();
	}
}

int main() {
	ios::sync_with_stdio(false);
	// freopen("input.in", "r", stdin);
	freopen("useless.in", "r", stdin);
	freopen("useless.out", "w", stdout);

	int n;
	char start;
	cin >> n >> start;
	string s;

	getline(cin, s);

	vector<pair<char, vector<char>>> rules;
	vector<bool> globally((int)'Z' + 1, false);

	globally[start] = true;

	for (int i = 0; i < n; i++) {
		getline(cin, s);
		globally[s[0]] = true;

		vector<char> to;
		for (int j = 1; j < (int)s.size(); j++) {
			if ((s[j] >= 'A' && s[j] <= 'Z') || (s[j] >= 'a' && s[j] <= 'z')) {
				to.push_back(s[j]);
				globally[s[j]] = true;
			}
		}
		rules.push_back(make_pair(s[0], to));
	}

	for (int i = 0; i < 100; i++) {
		 compress(n, rules, start);
	}

	vector<bool> valid((int)'Z' + 1, false);
	for (int i = 0; i < (int)rules.size(); i++) {
		valid[rules[i].first] = true;
		for (int j = 0; j < (int)rules[i].second.size(); j++) {
			valid[rules[i].second.size()] = true;
		}
	}

	for (char z = 'A'; z <= 'Z'; z++) {
		if (!valid[z] && globally[z]) {
			cout << z << " ";
		}
	}
}
