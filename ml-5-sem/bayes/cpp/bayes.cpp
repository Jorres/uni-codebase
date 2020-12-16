#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <iomanip>
#include <cmath>
#include <algorithm>
#include <climits>

using namespace std;
using calc = double;

int main() {
    int alpha, n;
    cin >> alpha >> n;

    set<string> allwords;
    vector<set<int>> met(n);

    vector<int> part(k, 0);
    vector<int> which(n);
    map<string, int> ids;
    for (int i = 0; i < n; i++) {
        int cl, wordsN;
        cin >> cl >> wordsN;
        cl--;
        part[cl]++;
        which[i] = cl;
        for (int j = 0; j < wordsN; j++) {
            string s; cin >> s;
            if (allwords.count(s) == 0) {
                ids[s] = allwords.size();
                allwords.insert(s);
            }
            met[i].insert(ids[s]);
        }
    }

    vector<vector<int>> count(ids.size(), vector<int>(k, 0));
    vector<vector<int>> count_neg(ids.size(), vector<int>(k, 0));

    for (int i = 0; i < n; i++) {
        for (auto const& p : ids) {
            int id = p.second;
            if (met[i].count(id)) {
                count[id][which[i]]++;
            } else {
                count_neg[id][which[i]]++;
            }
        }
    }

    int m;
    cin >> m;

    for (int i = 0; i < m; i++) {
        int l;
        cin >> l;
        set<int> message;
        for (int j = 0; j < l; j++) {
            string s;
            cin >> s;
            if (allwords.count(s) != 0) {
                message.insert(ids[s]);
            }
        }

        vector<calc> noms(k);
        calc best_mult = 1e18;
        for (int j = 0; j < k; j++) {
            if (part[j] == 0) {
                noms[j] = LLONG_MIN;
                continue;
            }

            noms[j] = log((calc)lambdas[j]) + log((calc)part[j] / n);
            for (auto const& s : ids) {
                int id = s.second;
                calc pos = count[id][j];
                calc neg = count_neg[id][j];
                calc top = (message.count(id) != 0) ? pos : neg;
                noms[j] += log(top + alpha) - log(pos + neg + 2 * alpha);
            }
            calc mult = 1 - noms[j];
            best_mult = min(mult, best_mult);
        }

        calc sum = 0;
        for (auto& nom : noms) {
            if (nom == LLONG_MIN) {
                continue;
            }
            nom = exp(nom + best_mult);
            sum += nom;
        }

        for (auto& nom : noms) {
            if (nom == LLONG_MIN) {
                cout << fixed << 0 << " ";
            } else {
                cout << fixed << nom / sum << " ";
            }
        }
        cout << endl;
    }
}
