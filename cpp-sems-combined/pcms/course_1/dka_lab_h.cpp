#include <iostream>
#include <vector>
#include <set>
#include <map>
#include <queue>

using namespace std;

enum {
	WHITE, 
	BLACK
};

struct automaton {
	int n, m, k;
	vector<map<char, int>> d;
	vector<int> t;

	automaton(int _n, int _m, int _k) : n(_n), m(_m), k(_k) {
		d.resize(n);
		t.assign(n, 0);
	}

	void insert_edge(int a, int b, char c) {
		d[a - 1].insert(make_pair(c, b - 1));
	}

	void print() {
		cout << n << " " << m << " " << k << endl;
		for (int i = 0; i < n; i++) {
			if (t[i] == 1) {
				cout << i + 1 << " ";
			}
		}
		cout << endl;
		for (int i = 0; i < n; i++) {
			for (auto p : d[i]) {
				cout << i + 1 << " " << p.second + 1 << " " << p.first << endl;
			}
		}
	}


	vector<set<int>> get_equivalent() {
		vector<vector<int>> differ(n + 1, vector<int>(n + 1, 0));

		queue<pair<int, int>> q;

		for (int i = 0; i < n; i++) {
			for (int j = i + 1; j < n; j++) {
				if (t[i] != t[j]) { 
					differ[i][j] = differ[j][i] = 1;
					q.push(make_pair(i, j));
				}
			}
			if (t[i] == 1) {
				differ[i][n] = differ[n][i] = 1;
				q.push(make_pair(i, n));
			}
		}

		set<char> alph;
		vector<vector<vector<int>>> rev_d(n + 1, vector<vector<int>>((int)('z' + 1)));
		vector<set<char>> where_is(n);
		for (int i = 0; i < n; i++) {
			for (pair<char, int> p : d[i]) {
				rev_d[p.second][p.first].push_back(i);
				where_is[i].insert(p.first);
				alph.insert(p.first);
			}
		}

		//rev_d[n]
		for (int i = 0; i < n; i++) {
			for (char z : alph) {
				if (where_is[i].count(z) == 0) {
					rev_d[n][z].push_back(i);
				}
			}
		}

		for (char z : alph) {
			rev_d[n][z].push_back(n);
		}

		while (!q.empty()) {
			pair<int, int> p = q.front();
			int i = p.first, j = p.second;
			q.pop();
			for (char z = 'a'; z <= 'z'; z++) {
				for (int pre_i : rev_d[i][z]) {
					for (int pre_j : rev_d[j][z]) {
						if (differ[pre_i][pre_j] == 0) {
							differ[pre_i][pre_j] = differ[pre_j][pre_i] = 1;
							q.push(make_pair(pre_i, pre_j));
						}
					}
				}
			}
		}

		vector<set<int>> ans;
		vector<int> where_v(n, -1);
		for (int i = 0; i < n; i++) {
			if (where_v[i] == -1) {
				ans.push_back(set<int>());
				ans.back().insert(i);
				where_v[i] = (int)ans.size() - 1;
			}

			for (int j = i + 1; j < n; j++) {
				if (differ[i][j] == 0) {
					ans[where_v[i]].insert(j);
					where_v[j] = where_v[i];
				}			
			}
		}
		return ans;
	}

	void exclude_unreachable(int v, vector<int>& color) {
		color[v] = BLACK;
		for (pair<char, int> p : d[v]) {
			if (color[p.second] == WHITE) {
				exclude_unreachable(p.second, color);
			}
		}
	}

	bool term_reachable(int v, vector<int>& color) {
		color[v] = BLACK;
		if (t[v] == 1) {
			return true;
		}

		for (pair<char, int> p : d[v]) {
			if (color[p.second] == WHITE) {
				if (term_reachable(p.second, color)) {
					return true;
				}
			}
		}

		return false;
	}

	void slow_minimize() {
		vector<int> color(n, WHITE);
		exclude_unreachable(0, color);
		for (int i = 0; i < n; i++) {
			if (color[i] == BLACK) {
				vector<int> newcolor(n, 0);
				if (!term_reachable(i, newcolor)) {
					color[i] = WHITE;
				}
			}
		}
		
		for (int i = 0; i < n; i++) {
			if (color[i] == WHITE) {
				d[i].clear();
				if (t[i] == 1) {
					t[i] = 0;
					k--;
				}
			}
		}

		for (int i = 0; i < n; i++) {
			vector<pair<char, int>> to_delete;
			for (pair<char, int> p : d[i]) {
				if (color[p.second] == WHITE) {
					to_delete.push_back(p);
				}
			}
			for (pair<char, int> p : to_delete) {
				d[i].erase(p.first);
			}
		}

		vector<set<int>> ready = get_equivalent();

		int shift = 0;
		for (int i = 0; i < (int)ready.size(); i++) {
			bool real = true;
			for (int pre_v : ready[i]) {
				if (color[pre_v] == WHITE) {
					real = false;
					break;
				}
			}
			if (real) {
				ready[i - shift] = ready[i];
			} else {
				shift++;
			}
		}

		for (int i = 0; i < shift; i++) {
			ready.pop_back();
		}

		vector<int> in_which_class(n, -1);

		for (int i = 0; i < (int)ready.size(); i++) {
			for (int pre_v : ready[i]) {
				in_which_class[pre_v] = i;
			}
		}

		// convertation
		n = (int)ready.size();
		m = 0; // to be calculated
		k = 0; // to be calculated
		vector<int> pre_t = t;
		t.resize(n);

		for (int i = 0; i < n; i++) {
			bool term = true;
			for (int state : ready[i]) {
				if (in_which_class[state] == -1 || pre_t[state] == 0) {
					term = false;
					break;
				}
			}
			t[i] = (term ? 1 : 0);
			k += t[i];
		}

		if (k == 0) {
			n = m = k = 0;
			d.clear();
			t.clear();
			return;
		}

		// m, d remaining
		vector<map<char, int>> pre_d = d;
		d.clear();
		d.resize(n);
		for (int i = 0; i < n; i++) {
			for (int old_v : ready[i]) {
				for (pair<char, int> p : pre_d[old_v]) {
					int pre_size = d[i].size();
					if (in_which_class[p.second] != -1) {
						d[i].insert(make_pair(p.first, in_which_class[p.second]));
						if (pre_size + 1 == (int)d[i].size()) {
							m++;
						}
					}
				}
			}
		}
	}
};

automaton read_automaton() {
	int n, m, k;
	cin >> n >> m >> k;
	automaton a(n, m, k);

	for (int i = 0; i < k; i++) {
		int x; cin >> x;
		a.t[x - 1] = 1;
	}
	for (int i = 0; i < m; i++) {
		int s, f; char c;
		cin >> s >> f >> c;
		a.insert_edge(s, f, c);
	}

	return a;
}

bool parallel_dfs(automaton const& a, automaton const& b, int v_a, int v_b, vector<int>& vis_a, vector<int>& vis_b) {

	vis_a[v_a] = BLACK;
	vis_b[v_b] = BLACK;

	if (a.t[v_a] != b.t[v_b]) {
		return false;
	}

	if (a.d[v_a].size() != b.d[v_b].size()) {
		return false;
	}

	bool result = true;
	auto it = b.d[v_b].begin();
	for (pair<char, int> p_a : a.d[v_a]) {
		pair<char, int> p_b = *it;

		if (p_a.first != p_b.first) {
			return false;
		}

		if (vis_a[p_a.second] != vis_b[p_b.second]) {
			return false;
		}

		if (vis_a[p_a.second] == WHITE) {
			result = result && parallel_dfs(a, b, p_a.second, p_b.second, vis_a, vis_b);
		}

		if (result == false) {
			break;
		}

		it++;		
	}

	return result;
}

bool isomorphic(automaton const& a, automaton const& b) {
	if (a.n != b.n || a.m != b.m || a.k != b.k) {
		return false;
	}

	vector<int> vis_a(a.n, WHITE), vis_b(b.n, WHITE);
	return parallel_dfs(a, b, 0, 0, vis_a, vis_b);
}

int main() {
	ios::sync_with_stdio(false);
	// freopen("input.in", "r", stdin);
	freopen("equivalence.in", "r", stdin);
	freopen("equivalence.out", "w", stdout);

	automaton a = read_automaton();
	a.slow_minimize();

	automaton b = read_automaton();
	b.slow_minimize();

	cout << (isomorphic(a, b) ? "YES" : "NO") << endl;
}