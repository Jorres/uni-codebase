#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <cctype>

using namespace std;

#define endl '\n'

struct node {
    node(int _start, int _end) : start(_start), end(_end) {
        suffix_link = 0;
    }

    map<char, int> dst;
    int suffix_link;
    int start;
    int end;  // for the upper edge
};

struct suf_tree {
    static const int LEAF_END = -1;
    string s;
    vector<node> trie;
    int last_new;
    int remaining;

    int active_node;
    int active_length;
    int active_edge;  // index of the character

    suf_tree()
        : s("")
        , remaining(0)
        , active_node(0)
        , active_length(0)
        , active_edge(-1) {
        trie.reserve(2 * s.size());
        trie.push_back(node(-1, -1));
    }

    void append(const string& _s) {
        int prev_size = s.size();
        s += _s;
        for (int i = prev_size; i < (int)s.size(); i++) {
            extend_tree(i);
        }
    }

    bool walk_down(int node, int string_len) {
        int calc_end;
        if (trie[node].end == LEAF_END) {
            calc_end = string_len;
        } else {
            calc_end = trie[node].end;
        }

        int edge_length = calc_end - trie[node].start + 1;
        if (active_length >= edge_length) {
            active_edge += edge_length;
            active_length -= edge_length;
            active_node = node;
            return true;
        }
        return false;
    }

    void extend_tree(int pos) {
        remaining++;
        last_new = -1;
        while (remaining > 0) {
            if (active_length == 0) {
                active_edge = pos;
            }

            char cur = s[active_edge];

            if (!trie[active_node].dst.count(cur)) {
                trie.push_back(node(pos, LEAF_END));
                trie[active_node].dst[cur] = (int)trie.size() - 1;
                if (last_new != -1) {
                    trie[last_new].suffix_link = active_node;
                    last_new = -1;
                }
            } else {
                int next_node = trie[active_node].dst[cur];
                if (walk_down(next_node, pos)) {  // pos +- 1
                    continue;
                }

                char pres_ch = s[trie[next_node].start + active_length];
                if (pres_ch == s[pos]) {
                    if (last_new != -1 && active_node != 0) {
                        trie[last_new].suffix_link = active_node;
                        last_new = -1;
                    }
                    active_length++;
                    break;
                }

                int split_end = trie[next_node].start + active_length - 1;
                trie.push_back(node(trie[next_node].start, split_end));
                int split_node = trie.size() - 1;
                trie[active_node].dst[cur] = split_node;

                trie.push_back(node(pos, LEAF_END));
                trie[next_node].start += active_length;
                trie[split_node].dst[pres_ch] = next_node;
                trie[split_node].dst[s[pos]] = (int)trie.size() - 1;

                if (last_new != -1) {
                    trie[last_new].suffix_link = split_node;
                }

                last_new = split_node;
            }
            remaining--;
            if (active_node == 0 && active_length > 0) {
                active_length--;
                active_edge = pos - remaining + 1;
            } else if (active_node != 0) {
                active_node = trie[active_node].suffix_link;
            }
        }
    }

    bool traverse(int v, int pos, const string& other) {
        if (trie[v].dst.count(other[pos]) == 0) {
            return false;
        }
        int to = trie[v].dst[other[pos]];
        int left = trie[to].start;
        int right = (trie[to].end == -1 ? s.size() : trie[to].end);
        for (int i = left; i <= right && pos < (int)other.size(); i++) {
            if (other[pos] != s[i]) {
                return false;
            }
            pos++;
        }
        if (pos == (int)other.size()) {
            return true;
        }
        return traverse(to, pos, other);
    }

    bool has(const string& other) {
        return traverse(0, 0, other);
    }
};

int main() {
    // freopen("input.txt", "r", stdin);
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);

    suf_tree st;
    string s;
    while (getline(cin, s)) {
        char c = s[0];
        string norm = s.substr(2);
        for (auto& st_c : norm) {
            st_c = (char)tolower(st_c);
        }
        if (c == 'A') {
            st.append(norm);
        } else {
            cout << (st.has(norm) ? "YES" : "NO") << endl;
        }
    }
}
