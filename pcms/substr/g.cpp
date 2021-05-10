#include <iostream>
#include <vector>
#include <map>

using namespace std;

const int ALPHABET_SIZE = 26;

struct node {
    int t_edges[ALPHABET_SIZE];
    int a_edges[ALPHABET_SIZE];
    int parent;
    char parent_symbol;
    int suflink;
    bool is_term;
    vector<int> which_ends;

    node(char c = '$', int _parent = -1) {
        suflink = -1;
        parent = _parent;
        is_term = false;
        parent_symbol = c;
        for (int i = 0; i < ALPHABET_SIZE; i++) {
            t_edges[i] = a_edges[i] = -1;
        }
    }
};

void add_word(vector<node>& trie, const string& cur, int input_num) {
    int cur_node = 0;
    for (char c : cur) {
        int conv = c - 'a';
        if (trie[cur_node].t_edges[conv] == -1) {
            trie.push_back(node(c, cur_node));
            trie[cur_node].t_edges[conv] = trie.size() - 1;
        }

        cur_node = trie[cur_node].t_edges[conv];
    }

    trie[cur_node].is_term = true;
    trie[cur_node].which_ends.push_back(input_num);
}

int go(vector<node>& trie, int from_node, int ch);

int get_link(vector<node>& trie, int from_node) {
    node& cur = trie[from_node];
    if (cur.suflink == -1) {
        if (from_node == 0 || cur.parent == 0) {
            cur.suflink = 0;
        } else {
            cur.suflink = go(trie, get_link(trie, cur.parent), (int)(cur.parent_symbol - 'a'));
        }
    }

    return cur.suflink;
}

int go(vector<node>& trie, int from_node, int ch) {
    node& cur = trie[from_node];
    if (cur.a_edges[ch] == -1) {
        if (cur.t_edges[ch] != -1) {
            cur.a_edges[ch] = cur.t_edges[ch];
        } else {
            cur.a_edges[ch] = (from_node == 0 ? 0 : go(trie, get_link(trie, from_node), ch));
        }
    }
    return cur.a_edges[ch];
}

int main() {
    // freopen("input.txt", "r", stdin);
    ios::sync_with_stdio(false);
    string text;
    cin >> text;

    int n;
    cin >> n;
    vector<node> trie;
    trie.push_back(node());
    vector<bool> ans(n, false);
    for (int i = 0; i < n; i++) {
        string cur;
        cin >> cur;
        add_word(trie, cur, i);
    }

    int cur = 0;
    vector<bool> visited(trie.size(), false);
    for (auto c : text) {
        cur = go(trie, cur, (int)(c - 'a'));

        int tmp = cur;
        while (tmp != 0 && !visited[tmp]) {
            visited[tmp] = true;
            if (trie[tmp].is_term) {
                for (auto v : trie[tmp].which_ends) {
                    ans[v] = true;
                }
            }
            tmp = get_link(trie, tmp);
        }
    }

    for (int i = 0; i < n; i++) {
        cout << (ans[i] ? "Yes" : "No") << '\n';
    }
}
