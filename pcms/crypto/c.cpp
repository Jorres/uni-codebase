#include <iostream>
#include <vector>
#include <cmath>
#include <cassert>

using namespace std;

int sieve(int n, int x) {
    vector<bool> divided(n / 2 + 1, false);
    int res = 2;
    bool canceled = false;

    int real = 3;
    for (int i = 2; real <= n; i++) {
        if (!divided[i]) {
            res = res * x + real;

            if (!canceled) {
                if (real * real <= n) {
                    for (int j = real * real; j <= n; j += real) {
                        if (j % 2 != 1) {
                            continue;
                        }
                        divided[(j + 1) / 2] = true;
                    }
                } else {
                    canceled = true;
                }
            }
        }
        real += 2;
    }
    return res;
}

int main() {
    ios::sync_with_stdio(false);
    // freopen("input.txt", "r", stdin);
    int n, x;
    cin >> n >> x;
    cout << sieve(n, x) << endl;
}
