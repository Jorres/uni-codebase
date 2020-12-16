#define _CRT_SECURE_NO_WARNINGS
#include <vector>
#include <iostream>
#include <algorithm>
#include <cassert>
#include <string>

using namespace std;

struct node {
	int key, num;
};

struct priority_queue {
	vector<node> a;
	vector<int> links;

	// pos_in_torrent -> pos_in_queue

	node extract_min() {
		node t = a[0];
		links[a[0].num] = -1;
		if ((int)a.size() > 1) {
			a[0] = a[a.size() - 1];
			links[a[0].num] = 0;
			a.pop_back();
			sift_down(0);
		} else {
			a.pop_back();
		}
		return t;
	}

	void decrease_key(int num, int new_key) {
		if (links[num] != -1) {
			a[links[num]].key = new_key;
			sift_up(links[num]);
		}
	}

	void push(node n) {
		a.push_back(n);
		links[n.num] = (int)a.size() - 1;
		sift_up((int)a.size() - 1);
	}

	void sift_down(int pos) {
		while (true) {
			int l = 2 * pos + 1, r = 2 * pos + 2, target = -1;

			if (l < (int)a.size() && a[pos].key > a[l].key) {
				target = l;
			}
			if (r < (int)a.size() && min(a[pos].key, a[l].key) > a[r].key) {
				target = r;
			}

			if (target == -1) {
				break;
			}

			links[a[target].num] = pos;
			links[a[pos].num] = target;
			swap(a[pos], a[target]);
			pos = target;
		}
	}

	void sift_up(int pos) {
		while (pos != 0 && a[(pos - 1) / 2].key > a[pos].key) {
			int p = (pos - 1) / 2;
			links[a[pos].num] = p;
			links[a[p].num] = pos;
			swap(a[p], a[pos]);
			pos = p;
		}
	}
};


int main() {
	ios::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);

	// freopen("input.txt", "r", stdin);

	priority_queue q;

	string s;
	int i = 0;
	while (cin >> s) {
		int a, d;
		q.links.push_back(-1);
		if (s == "push") {
			cin >> a;
			q.push({ a, i });
		} else if (s == "extract-min") {
			if (q.a.size() == 0) {
				cout << "*" << endl;
			} else {
				node cur = q.extract_min();
				cout << cur.key << " " << cur.num + 1 << endl;
			}
		} else if (s == "decrease-key") {
			cin >> a >> d;
			q.decrease_key(a - 1, d);
		} else {
			assert(false);
		}
		i++;

		/* for (int j = 0; j < q.a.size(); j++) {
			cout << q.a[j].key << " ";
		}
		cout << endl;
		for (int j = 0; j < q.a.size(); j++) {
			cout << q.a[j].num + 1 << " ";
		}
		cout << endl;
		cout << endl; */
	}
}
