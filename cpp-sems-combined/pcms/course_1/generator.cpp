#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <algorithm>

using namespace std;

typedef long long LL;

int main() {
	srand(time(NULL));

	int n = rand() % 100 + 2;
	cout << n << endl;
		
	vector<int> a(n, 0);
	for (int i = 0; i < n; i++) {
		a[i] = rand() % 200 - 100;
		cout << a[i] << " ";
	}
	cout << endl;

	int m = rand() % 100 + 20;
	for (int i = 0; i < m; i++) {
		int t = rand() % 3;
		int l = rand() % n + 1, r = rand() % n + 1;
		if (l > r) {
			swap(l, r);
		}
		if (t == 0) {
			cout << "min " << l << " " << r << endl;
		} else {
			LL val = ((LL)rand() * rand() * rand()) % 50000;
			if (t == 1) {
				cout << "set " << l << " " << r << " " << val << endl;
			} else {
				cout << "add " << l << " " << r << " " << val << endl;
			}
		}
	}
}