#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <cassert>
#include <cmath>

#include "checker.h"

using namespace std;

using calc = double;

const double PI = 3.14159265358979323846;

struct point {
    calc x;
    calc y;
};

pair<bool, calc> parse_response(string ans_data) {
    stringstream s;
    s << ans_data;
    pair<bool, calc> ans;
    string type;
    s >> type;
    assert(type == "inside" || type == "outside");
    ans.first = type == "inside";
    calc dist;
    s >> dist;
    ans.second = dist;
    return ans;
}

point on_circle(calc angle, calc R) {
    return point{R * cos(angle), R * sin(angle)};
}

string make_request(point p) {
    stringstream ans;
    ans << "activate " << p.x << " " << p.y;
    return respond(ans.str());
}

pair<calc, point> find_half_unsafe(calc lb, calc rb) {
    const calc R = 6000;
    calc l = lb, r = rb;
    calc last_dist;
    for (int i = 0; i < 100; i++) {
        calc lm = l + (r - l) * 1.0 / 3;
        calc rm = l + (r - l) * 2.0 / 3;
        auto respl = parse_response(make_request(on_circle(lm, R)));
        auto respr = parse_response(make_request(on_circle(rm, R)));
        assert (!respl.first && !respr.first);
        last_dist = min(respl.second, respr.second);
        if (respl.second < respr.second) {
            r = rm;
        } else {
            l = lm;
        }
    } 
    return make_pair(last_dist, on_circle(l, R));
}

pair<calc, point> find_on_circle_with_ternary() {
    auto down_half = find_half_unsafe(0, PI / 2);
    auto up_half = find_half_unsafe(PI / 2, PI);
    if (down_half.first < up_half.first) {
        return down_half;
    } else {
        return up_half;
    }
}

point find_corner(pair<calc, point> _p) {
    calc dist = _p.first;
    point p = _p.second;

    const 
    


    // calc dist = _p.first;
    // point p = _p.second;
    // calc furthest_angle = find_furthest_from_corner(p);
    // calc angle_side_1 = furthest_angle + 3.0 / 8 * PI;
    // calc angle_side_2 = angle_side_1 + PI / 4;


}

int main() {
    int m, n;
    cin >> m >> n;

    point corner = find_corner(
        find_on_circle_with_ternary()
    );

    cout << corner.x << " " << corner.y << endl;
}
