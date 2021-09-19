#include <iostream>
#include <vector>

using namespace std;

using calc = double;

struct point {
    calc x;
    calc y;
};

point read_point() {
    calc x, y;
    cin >> x >> y;
    return point {x, y};
}

int main() {
    vector<point> ps;
    for (int i = 0; i < 4; i++) {
        ps.push_back(read_point())
    }

}
