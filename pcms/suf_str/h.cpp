#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <climits>

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

    long long leaves;
    long long cur_substrings;

    suf_tree()
        : s("")
        , remaining(0)
        , active_node(0)
        , active_length(0)
        , active_edge(-1)
        , leaves(0)
        , cur_substrings(0) {
        trie.reserve(2 * s.size());
        trie.push_back(node(-1, -1));
    }

    void append(char c) {
        s += c;
        extend_tree(s.size() - 1);
    }

    pair<char, long long> resolve() {
        pair<char, long long> best = make_pair('0', LLONG_MAX);
        for (char c = 'a'; c <= 'z'; c++) {
            s += c;
            long long new_leaves = cur_substrings + fake_append(s.size() - 1);
            s.pop_back();
            if (new_leaves < best.second) {
                best = make_pair(c, new_leaves);
            }
        }
        s += best.first;
        extend_tree(s.size() - 1);
        return best;
    }

    int  f_remaining;
    int  f_active_length;
    int  f_active_edge;
    int  f_active_node;
    long long  f_leaves;
    // returns number of leaves in the tree after fake appending char c
    // fake - means it does not change tree in the process
    long long fake_append(int pos) {
        f_remaining = remaining;
        f_active_length = active_length;
        f_active_edge = active_edge;
        f_active_node = active_node;
        f_leaves = leaves;


        f_remaining++;

        while (f_remaining > 0) {
            if (f_active_length == 0) {
                f_active_edge = pos;
            }

            char cur = s[f_active_edge];

            if (!trie[f_active_node].dst.count(cur)) {
                f_leaves++;
            } else {
                int next_node = trie[f_active_node].dst[cur];
                if (f_walk_down(next_node, pos)) {
                    continue;
                }

                char pres_ch = s[trie[next_node].start + f_active_length];
                if (pres_ch == s[pos]) {
                    f_active_length++;
                    break;
                }

                f_leaves++;
            }
            f_remaining--;

            if (f_active_node == 0 && f_active_length > 0) {
                f_active_length--;
                f_active_edge = pos - f_remaining + 1;
            } else if (f_active_node != 0) {
                f_active_node = trie[f_active_node].suffix_link;
            }
        }
        return f_leaves;
    }

    bool f_walk_down(int node, int string_len) {
        int calc_end;
        if (trie[node].end == LEAF_END) {
            calc_end = string_len;
        } else {
            calc_end = trie[node].end;
        }

        int f_edge_length = calc_end - trie[node].start + 1;
        if (f_active_length >= f_edge_length) {
            f_active_edge += f_edge_length;
            f_active_length -= f_edge_length;
            f_active_node = node;
            return true;
        }
        return false;
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
                leaves++;
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

                leaves++;
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
        cur_substrings += leaves;
    }
};

int main() {
    // freopen("input.txt", "r", stdin);
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;
    suf_tree st;

    for (int i = 0; i < n; i++) {
        char c;
        cin >> c;
        if (c != '?') {
            st.append(c);
        } else {
            pair<char, long long> solution = st.resolve();
            cout << solution.first << " " << solution.second << endl;
        }
    }
}
