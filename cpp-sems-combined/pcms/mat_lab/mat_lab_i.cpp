#include <iostream>
#include <vector>
#include <string>
#include <set>
#include <map>

using namespace std;

long long deg(int n, int deg) {
    long long ans = 1;
    for (int i = 0; i < deg; i++) {
        ans *= static_cast<long long>(n);
    }
    return ans;
}

void find_euler_loop(vector<set<int>>& edges, vector<int>& euler_loop, vector<int>& degr) {
    vector<int> st;
    st.push_back(0);

    while (!st.empty()) {
        int v = st.back();
        if (degr[v] == 0) {
            euler_loop.push_back(v);
            st.pop_back();
        } else {
            int first_to = *(edges[v].begin());
            edges[v].erase(first_to);
            edges[first_to].erase(v);

            degr[v]--;
            degr[first_to]--;
            st.push_back(first_to);
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    freopen("input.in", "r", stdin);

    int d, k;
    cin >> d >> k;

    int amount = deg(d, k - 1);
    int amount_edges= deg(d, k);

    map<int, int> num_by_ver;
    vector<int> ver_by_num(amount);

    vector<int> degr(amount, 0);

    for (int i = 0; i < amount_edges; i++) {
        int v = i;
        int left = v % amount;
    }

    int mod = deg(d, k - 1);
    vector<set<int>> edges(amount);
    for (int i = 0; i < amount; i++) {
        int v = ver_by_num[i];
        int common_part = v % mod;
        for (int j = 0; j < d; j++) {
            int to = common_part * 10 + j;
            int num_to = num_by_ver[to];
            edges[i].insert(num_to);
            degr[i]++;
        }
    }

    vector<int> euler_loop;
    find_euler_loop(edges, euler_loop, degr);

    string ans = std::to_string(ver_by_num[0]);
    for (size_t i = 1; i < euler_loop.size(); i++) {
        ans += '0' + static_cast<char>(ver_by_num[i] % d);        
    }

    cout << ans << endl;
}
