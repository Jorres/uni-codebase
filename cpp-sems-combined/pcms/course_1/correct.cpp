#include <iostream> 
#include <vector>
#include <climits>

using namespace std;

typedef long long LL;

int main() {
	int n;
	cin >> n;
	vector<LL> a(n);
	for (int i = 0; i < n; i++) {
		cin >> a[i];
	}

	string s;
	while (cin >> s) {
		int l, r;
		LL val;
		cin >> l >> r;
		l--; r--;
		if (s == "set") {
			cin >> val;
			for (int i = l; i <= r; i++) {
				a[i] = val;
			}
		} else if (s == "min") {
			LL tmin = LONG_MAX;
			for (int i = l; i <= r; i++) {
				tmin = min(tmin, a[i]);
			}
			cout << tmin << "\n";
		} else if (s == "add") {
			cin >> val;
			for (int i = l; i <= r; i++) {
				a[i] += val;
			}
		}
	}
}