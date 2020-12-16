#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

using namespace std;

int main() {
    // freopen("input.in", "r", stdin);
    freopen("schedule.in", "r", stdin);
    freopen("schedule.out", "w", stdout);

    int n;
    cin >> n;

    vector<pair<int, int>> tasks(n);
    for (int i = 0; i < n; i++) {
        cin >> tasks[i].first >> tasks[i].second;
    }
    sort(tasks.begin(), tasks.end());

    long long ans = 0;
    multiset<int> s;
    for (int i = n - 1; i >= 0;) {
        int j = i;
        while (j >= 0 && tasks[j].first == tasks[i].first) {
            s.insert(tasks[j].second);
            j--;
        }

        int days = tasks[i].first - (j == -1 ? 0 : tasks[j].first);
        while (!s.empty() && days > 0) {
            s.erase(--s.end());
            days--;
        }
        i = j;
    }

    for (auto c : s) {
        ans += c;
    }
    cout << ans << endl;
}
