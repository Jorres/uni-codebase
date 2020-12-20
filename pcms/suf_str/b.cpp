#include <iostream>
#include <string>
#include <vector>
#include <map>

using namespace std;

struct node {
    map<char, int> dst;
};

void add(vector<node>& trie, const string& s, int l, int r) {
    int st = 0;
    for (int i = l; i < r; i++) {
        if (!trie[st].dst.count(s[i])) {
            trie.push_back(node());
            trie[st].dst[s[i]] = trie.size() - 1;
        }
        st = trie[st].dst[s[i]];
    }
}

int main() {
    // freopen("input.txt", "r", stdin);
    string s;
    cin >> s;
    int n = s.size();
    vector<node> trie;
    trie.push_back(node());
    for (int i = 0; i < n; i++) {
        add(trie, s, i, n);
    }

    cout << trie.size() << " " << (int)trie.size() - 1 << endl;
    for (int i = 0; i < (int)trie.size(); i++) {
        for (auto p : trie[i].dst) {
            cout << i + 1 << " " << p.second + 1 << " " << p.first << endl;
        }
    }
}
