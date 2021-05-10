#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

void sieve(vector<int>& min_div) {
    int n = min_div.size();
    vector<int> primes;
    for (int i = 2; i < n; i++) {
        if (min_div[i] == 0) {
            min_div[i] = i;
            primes.push_back(i);
        }

        for (auto prev_pr : primes) {
            if (prev_pr <= min_div[i] && i * prev_pr < n) {
                min_div[i * prev_pr] = prev_pr;
            } else {
                break;
            }
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    // freopen("input.txt", "r", stdin);
    cin.tie(0);
    cout.tie(0);

    int n;
    cin >> n;
    vector<int> reqs(n);
    int max_req = 1'000'000;

    vector<int> min_div(max_req + 1, 0);

    sieve(min_div);

    for (int i = 0; i < n; i++) {
        int v;
        cin >> v;
        vector<int> ans;
        while (v > 1) {
            ans.push_back(min_div[v]);
            v /= min_div[v];
        }

        for (auto a : ans) {
            cout << a << " ";
        }

        cout << '\n';
    }
}
