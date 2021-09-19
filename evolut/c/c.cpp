#include <iostream>
#include <vector>
#include <cassert>
#include <ctime>

using namespace std;

enum state {
    CORRECT, INCORRECT
};

int global_n;

int grace() {
    int res;
    cin >> res;
    if (res == global_n) {
        exit(0);
    }
    return res;
}


// gracefully terminate when read fitness equal to n

// returns what was ith bit before calling the function
// and pos where should we continue calculations from
// and fills reverse_when_sure if it encounters bits that got INCORRECT from flipping
pair<state, int> resolve_chain(int i, int& f, vector<int>& reverse_when_sure) {
    int res;
    if (i == global_n) {
        cout << i << endl;
        cout.flush();
        res = grace();
        if (res == f + 1) {
            f += 1;
            return make_pair(INCORRECT, i + 1);
        } else {
            assert(res == f - 1);
            f -= 1;
            reverse_when_sure.push_back(i);
            return make_pair(CORRECT, i + 1);
        }
    }

    cout << i << " " << i + 1 << endl;
    cout.flush();
    res = grace();
    if (res == f + 2) {
        f += 2;
        return make_pair(INCORRECT, i + 2);
    } else if (res == f - 2) {
        f -= 2;
        reverse_when_sure.push_back(i);
        reverse_when_sure.push_back(i + 1);
        return make_pair(CORRECT, i + 2);
    } else {
        auto p = resolve_chain(i + 1, f, reverse_when_sure);
        if (p.first == CORRECT) { // p.first state RIGHT BEFORE i called second resolve chain
            // mine was INCORRECT before I called second resolve_chain
            // that means it was CORRECT before I called my instance
            // but I already flipped it once. So now it I need to flip it once more. 
            reverse_when_sure.push_back(i);
            return make_pair(CORRECT, p.second);
        } else {
            return make_pair(INCORRECT, p.second);
        }
    }
}

int main() {
    srand(time(NULL));

    int f;
    cin >> global_n >> f;

    if (f == global_n) {
        return 0;
    }

    vector<int> pre_shuffle = { 1 };
    for (int i = 2; i <= global_n; i++) {
        if (rand() % 2 == 0) {
            pre_shuffle.push_back(i);
        }
    }

    for (int j = 0; j < (int)pre_shuffle.size(); j++) {
        if (j != 0) {
            cout << " ";
        }
        cout << pre_shuffle[j];
    }
    cout << endl;
    cout.flush();
    f = grace();

    int i = 1;
    vector<int> reverse_when_sure;
    while (i <= global_n) {
        auto p = resolve_chain(i, f, reverse_when_sure);
        i = p.second;
    }

    if (!reverse_when_sure.empty()) {
        for (int j = 0; j < (int)reverse_when_sure.size(); j++) {
            if (j != 0) {
                cout << " ";
            }
            cout << reverse_when_sure[j];
        }
        cout << endl;
        cout.flush();
        int res; // could be grace() invocation
        cin >> res;
        assert(res == global_n);
    }
}
