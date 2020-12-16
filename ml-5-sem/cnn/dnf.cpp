#include <iostream>
#include <vector>
#include <cassert>
   
using namespace std;

int main() {
    // freopen("input.txt", "r", stdin);

    int m;
    cin >> m;

    vector<vector<double>> ws;
    vector<double> bs;

    for (int i = 0; i < (1 << m); i++) {
        int cur;
        cin >> cur;

        if (cur == 1) {
            ws.push_back(vector<double>());
            int num = i;
            double b = 0.5;
            for (int j = 0; j < m; j++) {
                if (num % 2 == 0) {
                    ws.back().push_back(-1);
                } else if (num % 2 == 1) {
                    ws.back().push_back(1);
                    b -= 1;
                }
                num /= 2;
            }

            bs.push_back(b);
        } 
    }

    cout << 2 << endl << ws.size() << " " << 1 << endl;
    for (int i = 0; i < ws.size(); i++) {
        for (auto& w : ws[i]) {
            cout << w << " ";
        }
        cout << bs[i] << endl;
    }
    for (int i = 0; i < ws.size(); i++) {
        cout << 1 << " ";
    }
    cout << -0.5 << endl;
}
