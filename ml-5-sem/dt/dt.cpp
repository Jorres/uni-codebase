#include <iostream>
#include <set>
#include <vector>
#include <algorithm>
#include <climits>

using namespace std;

const int UNDEFINED = INT_MAX;

struct node {
    node() {
        elems = set<int>();
        is_leaf = false;
        which = -1;
        b = 0;
        to_sml = to_lrg = -1;
        cls = -1;
    }

    node(const vector<int>& v) : node() {
        for (auto& elem : v) {
            elems.insert(elem);
        }
    }

    set<int> elems;

    bool is_leaf;

    int which;
    int b;
    int to_sml, to_lrg;
    int cls;
};

int sz = 0;
vector<node> tree;

void do_cut() {

}

int find_major_class(int k, const set<int>& s, const vector<int>& ys) {
    vector<int> classes(k, 0);
    int maxcl = -1;
    for (int id : s) {
        classes[ys[id]]++;
        if (maxcl == -1 || classes[maxcl] < classes[ys[id]])  {
            maxcl = ys[id];
        }
    }
    // cout << "Classes for node: ";
    // for (int i = 0; i < k; i++) {
    //     cout << classes[i] << ' ';
    // }
    // cout << endl;
    return maxcl;
}

void make_this_leaf(node& curnode, int k, const vector<int>& ys) {
    curnode.is_leaf = true;
    curnode.cls = find_major_class(k, curnode.elems, ys);
}

double calc_full_ginie(int k, const set<int>& elems, const vector<int>& ys) {
    vector<int> cnt(k, 0);
    for (auto elem : elems) {
        cnt[ys[elem]]++;
    }

    double nom = 0;
    for (auto cls : cnt) {
        nom += cls * cls;
    }

    int n = elems.size();
    return 1 - nom / (n * n);
}

double recalc_F(double f, double l, double r, int lsz, int fsz) {
    int rsz = fsz - lsz;
    return f - ((double)lsz / fsz) * l - ((double)rsz / fsz) * r;
}

void build_tree(int m, int k, int h, int n, const vector<vector<int>>& data, const vector<int>& ys, int curnode, int cur_height) {
    int cursize = tree[curnode].elems.size();

    if (cursize < 3 || cur_height == h) {
        make_this_leaf(tree[curnode], k, ys);
    } else {
        int best_prop = UNDEFINED;
        int best_const = UNDEFINED;
        double best_F = UNDEFINED;

        double full_ginie = calc_full_ginie(k, tree[curnode].elems, ys);

        for (int prop = 0; prop < m; prop++) { // select property
            vector<pair<int, int>> b_prop;
            for (int id : tree[curnode].elems) { // construct possible b's
                b_prop.push_back(make_pair(data[id][prop], id));
            }
            sort(b_prop.begin(), b_prop.end()); // x[prop] < b

            vector<int> classes_left(k, 0);
            vector<int> classes_right(k, 0);

            int st = 1;
            while (st < cursize && b_prop[0].first == b_prop[st].first) {
                st++;
            }

            for (int i = 0; i < b_prop.size(); i++) {
                int id = b_prop[i].second;
                if (i < st) {
                    classes_left[ys[id]]++;
                } else {
                    classes_right[ys[id]]++;
                }
            }

            double left_sq_sum = 0;
            double right_sq_sum = 0;
            for (int i = 0; i < k; i++) {
                right_sq_sum += classes_right[i] * classes_right[i];
                left_sq_sum += classes_left[i] * classes_left[i];
            }

            for (int left_size = st; left_size < cursize;) {
                int b  = b_prop[left_size].first;
                int right_size = cursize - left_size;

                double left_part  = (1 - (left_sq_sum)  / (left_size  * left_size));
                double right_part = (1 - (right_sq_sum) / (right_size * right_size));

                // cout << left_sq_sum << " " << right_sq_sum << endl;
                double cur_F = recalc_F(full_ginie, left_part, right_part, left_size, cursize);

                // cout << "size: " << left_size << " ";
                // cout << "prop: " << prop << " const " << b << " ginie " << cur_F << endl;

                if (best_F == UNDEFINED || (cur_F - 1e-8 > best_F)) {
                    best_prop = prop;
                    best_F = cur_F;
                    best_const = b;
                }

                int rep = left_size + 1;
                while (rep < cursize && b_prop[rep - 1].first == b_prop[rep].first) {
                    rep++;
                }

                for (int j = left_size; j < rep; j++) {
                    int id = b_prop[j].second;
                    double pr_l = classes_left[ys[id]];
                    double pr_r = classes_right[ys[id]];
                    double cur_l = pr_l + 1;
                    double cur_r = pr_r - 1;
                    left_sq_sum  = left_sq_sum  - (pr_l * pr_l) + (cur_l * cur_l);
                    right_sq_sum = right_sq_sum - (pr_r * pr_r) + (cur_r * cur_r);
                    classes_left[ys[id]]++;
                    classes_right[ys[id]]--;
                    left_size++;
                }
            }
        }

        // F = f (S) - f(s_l) - f(s_r)
        // if (best_F < 0.1) {
        //     make_this_leaf(tree[curnode], k, ys);
        // }

        int l = sz;
        tree[sz++] = node();
        int r = sz;
        tree[sz++] = node();

        tree[curnode].b = best_const;
        tree[curnode].which = best_prop;
        tree[curnode].to_sml = l;
        tree[curnode].to_lrg = r;

        for (int id : tree[curnode].elems) {
            if (data[id][best_prop] < best_const) {
                tree[l].elems.insert(id);
            } else {
                tree[r].elems.insert(id);
            }
        }

        build_tree(m, k, h, n, data, ys, l, cur_height + 1);
        build_tree(m, k, h, n, data, ys, r, cur_height + 1);
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(NULL);
    
    // freopen("input.txt", "r", stdin);

    int m, k, h;
    cin >> m >> k >> h;
    int n;
    cin >> n;
    vector<vector<int>> data(n, vector<int>(m));
    vector<int> ys(n);
    vector<int> ids(n);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            cin >> data[i][j];
        }
        cin >> ys[i];
        ys[i]--;
        ids[i] = i;
    }

    tree.resize(1 << 12);
    tree[0] = node(ids);
    sz = 1;
    build_tree(m, k, h, n, data, ys, 0, 0);
    cout << sz << endl;
    for (int i = 0; i < sz; i++) {
        const node& nd = tree[i];
        if (nd.is_leaf) {
            cout << "C " << nd.cls + 1 << "\n";
        } else {
            cout << "Q " << nd.which + 1 << " " << nd.b << " " << nd.to_sml + 1 << " " << nd.to_lrg + 1 << "\n";
        }
    }
}
