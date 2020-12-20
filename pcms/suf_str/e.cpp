#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

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

void dfs(const suf_tree& st, vector<long long>& finishes, int num) {
    if (st.trie[num].dst.size() == 0) {
        finishes[num] = 1;
        return;
    }

    for (auto p : st.trie[num].dst) {
        int to = p.second;
        dfs(st, finishes, to);
        finishes[num] += finishes[to];
    }
}

// void print_dfs(const suf_tree& st, vector<long long>& finishes, int num) {
//     cout << "At num " << num << endl;
//     cout << "String is ";
//     int right = (st.trie[num].end == -1) ? st.s.size() - 1 : st.trie[num].end;
//     for (int i = st.trie[num].start; i <= right; i++) {
//         cout << st.s[i];
//     }
//     cout << endl;
//     cout << "Ways to finish " << finishes[num] << endl;
//     for (auto p : st.trie[num].dst) {
//         cout << p.second << " ";
//     }
//     cout << endl;
//
//     for (auto p : st.trie[num].dst) {
//         print_dfs(st, finishes, p.second);
//     }
// }

long long mult = -1;
long long maxlen = -1;

void update_ans(long long new_mult, long long new_len) {
    if (new_mult > mult || (new_mult == mult && new_len > maxlen)) {
        mult = new_mult;
        maxlen = new_len;
    }
}

int get_right(int actual, int finish) {
    return (actual == -1 ? finish : actual);
}

void calc_dfs(const suf_tree& st, const vector<long long>& finishes, int num, int len) {
    update_ans((long long)finishes[num] * len, (long long)len);

    for (auto p : st.trie[num].dst) {
        int to = p.second;
        int new_len = len;
        int right = get_right(st.trie[to].end, st.s.size() - 2);
        new_len += right - st.trie[to].start + 1;
        calc_dfs(st, finishes, p.second, new_len);
    }
}

string convert_char(char c) {
    if (c == (char)('0' + 10)) {
        return " 01";
    } else {
        return string{c};
    }
}

bool cont = true;
string ans_string;
void find_dfs(const suf_tree& st, const vector<long long>& finishes, int num, int len) {
    if ((long long)finishes[num] * len == mult && (long long)len == maxlen) {
        int left = st.trie[num].start;
        int right = get_right(st.trie[num].end, st.s.size() - 2);
        for (int i = right; i >= left; i--) {
            ans_string += convert_char(st.s[i]);
            ans_string += " ";
        }
        cont = false;
        return;
    }

    for (auto p : st.trie[num].dst) {
        int to = p.second;
        int new_len = len;
        int right = get_right(st.trie[to].end, st.s.size() - 2);
        new_len += right - st.trie[to].start + 1;

        if (cont) {
            find_dfs(st, finishes, p.second, new_len);
        }

        if (!cont && num != 0) {
            int left = st.trie[num].start;
            int right = get_right(st.trie[num].end, st.s.size() - 2);
            for (int i = right; i >= left; i--) {
                ans_string += convert_char(st.s[i]);
                ans_string += " ";
            }
            break;
        }
    }
}

int main() {
    // freopen("input.txt", "r", stdin);
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);

    int n, m;
    cin >> n >> m;
    string s;
    for (int i = 0; i < n; i++) {
        int a;
        cin >> a;
        s += (char)(a + '0');
    }
    s += '$';

    suf_tree st = suf_tree(s);
    vector<long long> finishes(st.trie.size(), 0);
    dfs(st, finishes, 0);

    calc_dfs(st, finishes, 0, 0);
    cout << mult << endl << maxlen << endl;
    find_dfs(st, finishes, 0, 0);
    reverse(ans_string.begin(), ans_string.end());
    cout << ans_string << endl;
}
