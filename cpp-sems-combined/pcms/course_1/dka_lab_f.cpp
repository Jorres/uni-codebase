#include <iostream>
#include <vector>
#include <set>

using namespace std;

enum {
	WHITE, 
	BLACK
};

struct automaton {
	int n, m, k;
	vector<set<pair<char, int>>> d;
	vector<int> t;

	automaton(int _n, int _m, int _k) : n(_n), m(_m), k(_k) {
		d.resize(n);
		t.assign(n, 0);
	}

	void insert_edge(int a, int b, char c) {
		d[a - 1].insert(make_pair(c, b - 1));
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

bool parallel_dfs(automaton const& a, automaton const& b, 
				  int v_a, int v_b, 
				  vector<int>& vis_a, vector<int>& vis_b) {

	vis_a[v_a] = BLACK;
	vis_b[v_b] = BLACK;

	if (a.t[v_a] != b.t[v_b]) {
		return false;
	}

	if (a.d[v_a].size() != b.d[v_b].size()) {
		return false;
	}

	bool result = true;
	set<pair<char, int>>::iterator it = b.d[v_b].begin();
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

bool solve() {
	automaton a = read_automaton();
	automaton b = read_automaton();

	if (a.n != b.n || a.m != b.m || a.k != b.k) {
		return false;
	}

	vector<int> vis_a(a.n, WHITE), vis_b(b.n, WHITE);
	return parallel_dfs(a, b, 0, 0, vis_a, vis_b);	
}

int main() {
	ios::sync_with_stdio(false);
	// freopen("input.in", "r", stdin);
	freopen("isomorphism.in", "r", stdin);
	freopen("isomorphism.out", "w", stdout);

	cout << (solve() ? "YES" : "NO") << endl;
}