#include <iostream>
#include <string>
#include <vector>
#include <map>

using namespace std;

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

    suf_tree(const string& _s)
        : s(_s)
        , remaining(0)
        , active_node(0)
        , active_length(0)
        , active_edge(-1) {
        trie.reserve(2 * s.size());
        trie.push_back(node(-1, -1));
        for (int i = 0; i < (int)s.size(); i++) {
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

    void traverse(int node, int len, vector<int>& ans) {
        if (trie[node].dst.empty()) {
            ans.push_back((int)s.size() - (len - 1) - 1);
        }

        for (auto p : trie[node].dst) {
            int next = p.second;
            int left = trie[next].start;
            int right = (trie[next].end == LEAF_END) ? s.size() - 1 : trie[next].end;
            traverse(p.second, len + (right - left + 1), ans);
        }
    }
};

void count_lcp(const string& s, const vector<int>& suf, vector<int>& lcp) {
    int n = s.size();
    vector<int> rev_suf(n);
    for (int i = 0; i < n; i++) {
        rev_suf[suf[i]] = i;
    }

    int k = 0;
    for (int i = 0; i < n - 1; i++) {
        if (k > 0) {
            k--;
        }
        if (rev_suf[i] == n - 1) {
            lcp[n - 1] = -1;
            k = 0;
            continue;
        } else {
            int j = suf[rev_suf[i] + 1];
            while (max(i + k, j + k) < n && s[i + k] == s[j + k]) {
                k++;
            }
            lcp[rev_suf[i]] = k;
        }
    }
}

int main() {
    // freopen("input.txt", "r", stdin);
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);
    string s;
    cin >> s;
    s.push_back('#');
    suf_tree st = suf_tree(s);
    vector<int> ans;
    ans.reserve(s.size() + 1);
    st.traverse(0, 0, ans);
    for (int i = 1; i < (int)ans.size(); i++) {
        cout << ans[i] + 1 << " ";
    }
    cout << '\n';

    vector<int> lcp(s.size());
    count_lcp(s, ans, lcp);
    for (int i = 1; i < (int)s.size() - 1; i++) {
        cout << lcp[i] << " ";
    }
}
