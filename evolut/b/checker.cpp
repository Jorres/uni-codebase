#include <iostream>
#include <string>
#include <sstream>
#include <cassert>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace std;

using calc = double;

struct point {
    calc x;
    calc y;
};

bool check_side(point a, point b, point c, point d) {
    calc xb1 = a.x;
    calc yb1 = a.y;
    calc xb2 = b.x;
    calc yb2 = b.y;
    calc xt = c.x;
    calc yt = c.y;
    calc xdot = d.x;
    calc ydot = d.y;

    if (xb1 - xb2 != 0) {
        calc k = (yb1 - yb2)/(xb1 - xb2);
        calc b = yb1 - k * xb1;
        if (yt - k * xt - b >= 0) {
            return ydot - k * xdot - b >= 0;
        } else {
            return ydot - k * xdot - b <= 0;
        }
    } else {
        if (xt >= xb1) {
            return xdot >= xb1;
        } else {
            return xdot <= xb1;
        }
    }
}

calc calc_dist2(point a, point b) {
    // cout << "Dist: " << a.x << " " << a.y << " " << b.x << " " << b.y << endl;
    calc dist = (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y);
    // cout << dist;
    return dist;
}

calc calc_dist(point a, point b) {
    return sqrt(calc_dist2(a, b));
}

calc dot(point a, point b) {
    return a.x * b.y - a.y * b.x;
}

calc calc_dist_to_segment(point p, point a, point b) {
    // cout << "Point: " << p.x << " " << p.y << endl;
    // cout << "Segment: " << a.x << " " << a.y << " " << b.x << " " << b.y<< endl;
    calc l2 = calc_dist2(a, b);
    calc t = ((p.x - a.x) * (b.x - a.x) + (p.y - a.y) * (b.y - a.y)) / l2;
    t = max(0.0, min(1.0, t));
    calc res = calc_dist(p, point{a.x + t * (b.x - a.x), a.y + t * (b.y - a.y)});
    // cout << res << endl;
    return res;
    // point v = b - a;
    // point w0 = a - p;
    // point w1 = b - p;
    // if (dot(w0, v) <= 0) {
    //     cout << "shortcut1" << endl;
    //     return calc_dist(a, p);
    // }
    // if (dot(w1, v) >= 0) {
    //     cout << "shortcut2" << endl;
    //     return calc_dist(b, p);
    // }
    // return abs(((a.y - b.y) * p.x 
    //         + (b.x - a.x) * p.y 
    //         + (a.x * b.y - b.x * a.y)))
    //     / (calc_dist(a, b));
}

calc calc_dist_to_walls(point p, const vector<point>& rect) {
    calc res1 = calc_dist_to_segment(p, rect[0], rect[1]);
    calc res2 = calc_dist_to_segment(p, rect[1], rect[2]);
    calc res3 = calc_dist_to_segment(p, rect[2], rect[3]);
    calc res4 = calc_dist_to_segment(p, rect[3], rect[0]);
    // cout << res1 << " " << res2 << " " << res3 << " " << res4 << endl;
    calc res = res1;
    res = min(res, res2);
    res = min(res, res3);   
    res = min(res, res4);   
    return res;
}

bool in_triangle(point a, point b, point c, point x) {
    return check_side(a, b, c, x) && check_side(a, c, b, x) && check_side(b, c, a, x);
}

bool in_rect(const vector<point>& rect, point p) {
    return in_triangle(rect[0], rect[1], rect[2], p) 
        || in_triangle(rect[2], rect[3], rect[0], p);
}

pair<string, calc> respond(string request_data, point target) {
    vector<point> rect = {
        point{0, 0}, point{20, 0}, point{20, 20}, point{0, 20}    
    };
    // vector<point> rect = {
    //     point{10, 11}, point{13, 10}, point{12, 7}, point{9, 8}    
    // };
    point treasure = point{16, 6};

    assert(request_data == "activate");

    if (in_rect(rect, target)) {
        return make_pair("inside", calc_dist(target, treasure));
    } else {
        return make_pair("outside", calc_dist_to_walls(target, rect));
    }
}
