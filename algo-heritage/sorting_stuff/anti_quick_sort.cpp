#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <iomanip>

using namespace std;

const int SIZE = 1000;
const int ALPH = 26;

int main() {
	ios::sync_with_stdio(false);
	// freopen("input.txt", "r", stdin);

	int n;
	cin >> n;
	vector<int> a(n + 1);
	if (n % 2 == 0) {
		for (int i = 0; i < (n - 1) / 2; i++) {
			a[i] = n - 1 - i;
		}
		a[(n - 1) / 2] = n;
		for (int i = n / 2; i < n; i++) {
			a[i] = n - i;
		}
	} else {
		n++;
		for (int i = 0; i < (n - 1) / 2; i++) {
			a[i] = n - 1 - i;
		}
		a[(n - 1) / 2] = n;
		for (int i = n / 2; i < n; i++) {
			a[i] = n - i;
		}
		swap(a[(n - 1) / 2], a[n - 1]);
		n--;
	}

	for (int i = 0; i < n; i++)
		cout << a[i] << " ";
}

