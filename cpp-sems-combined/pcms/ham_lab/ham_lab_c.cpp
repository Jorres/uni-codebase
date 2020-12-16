#include <iostream>
#include <deque>
#include <ctime>

using namespace std;

string req(int a, int b) {
    cout << "1 " << a << " " << b << endl;
    string s;
    cin >> s;
    return s;
}

int main() {
    srand(time(NULL));
    int n;
    cin >> n;
    if (n == 1) {
        cout << "0 1" << endl;
        return 0;
    }

    deque<int> lamps(1, 1);

    if (req(1, 2) == "YES") {
        lamps.push_back(2);
    } else {
        lamps.push_front(2);
    }

    for (int i = 3; i <= n; i++) {
        int L = -1;
        int R = lamps.size();
        while (R - L > 1) {
            int M = (L + R) / 2; // rand() % (R - L - 1)  + L + 1;
            if (req(lamps[M], i) == "YES") {
                L = M;
            } else {
                R = M;
            }
        }
        if (L == -1) {
            lamps.push_front(i);
            continue;
        }
        if (R == lamps.size()) {
            lamps.push_back(i);
            continue;
        }
        lamps.insert(lamps.begin() + L + 1, i);
    }

    cout << "0";
    for (int l : lamps) {
        cout << " " << l;
    }
    cout << endl;
}
