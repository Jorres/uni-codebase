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

    point operator-(const point& other) {
        return point{x - other.x, y - other.y};
    }
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

calc calc_dist(point a, point b) {
    return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
}

calc dot(point a, point b) {
    return a.x * b.x + a.y * b.y;
}

calc calc_dist_to_segment(point p, point a, point b) {
    point v = b - a;
    point w0 = a - p;
    point w1 = b - p;
    if (dot(w0, v) <= 0) {
        return calc_dist(a, p);
    }
    if (dot(w1, v) >= 0) {
        return calc_dist(b, p);
    }
    return ((a.y - b.y) * p.x 
            + (a.x - b.x) * p.y 
            + (a.x * b.y - b.x * a.y)) 
        / (calc_dist(a, b));
}

calc calc_dist_to_walls(point p, const vector<point>& rect) {
    calc res = calc_dist_to_segment(p, rect[0], rect[1]);
    res = min(res, calc_dist_to_segment(p, rect[1], rect[2]));
    res = min(res, calc_dist_to_segment(p, rect[2], rect[3]));   
    res = min(res, calc_dist_to_segment(p, rect[3], rect[0]));   
    return res;
}

bool in_triangle(point a, point b, point c, point x) {
    return check_side(a, b, c, x) && check_side(a, c, b, x) && check_side(b, c, a, x);
}

bool in_rect(const vector<point>& rect, point p) {
    return in_triangle(rect[0], rect[1], rect[2], p) 
        || in_triangle(rect[2], rect[3], rect[0], p);
}

string respond(string request_data) {
    vector<point> rect = {
        point{1, 1}, point{-1, 1}, point{-1, -1}, point{1, -1}    
    };
    point treasure = point{0, 0};

    assert(rect.size() == 4);
    stringstream request;
    request << request_data;

    string type;
    request >> type;
    double x, y;
    request >> x >> y;
    point target = point{x, y};

    assert(type == "activate");

    stringstream ss;
    if (in_rect(rect, point {x, y})) {
        ss << "inside ";
        ss << calc_dist(target, treasure);
        ss << " active";
    } else {
        ss << "outside ";
        ss << calc_dist_to_walls(target, rect);
        ss << " active";
    }
    return ss.str();
}
