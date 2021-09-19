#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <cassert>
#include <cmath>
#include <iomanip>

#define NO_LOCAL

#ifdef LOCAL
#include "checker.h"
#endif

#ifndef LOCAL
using calc = double;

struct point {
    calc x;
    calc y;

    point operator+(const point& other) {
        return point{x + other.x, y + other.y};
    }

    point operator-(const point& other) {
        return point{x - other.x, y - other.y};
    }
};

#endif

using namespace std;

enum RESPONSE_STATE {
    INSIDE, OUTSIDE, BLOCKED
};

pair<RESPONSE_STATE, calc> parse_response(pair<string, calc> ans_data) {
    // assert(ans_data.first == "inside" || ans_data.first == "outside" || ans_data.first == "blocked");
    RESPONSE_STATE state;
    if (ans_data.first == "inside") {
        state = INSIDE;
    }
    if (ans_data.first == "outside") {
        state = OUTSIDE;
    }
    if (ans_data.first == "blocked") {
        state = BLOCKED;
    }
    return make_pair(state, ans_data.second);
}

bool cool = true;
pair<string, calc> make_request(point p) {
    assert(cool);
    cout << "activate " << p.x << " " << p.y << endl;
    string line;
    getline(cin, line);
    stringstream ss;
    ss << line;
    string type;
    ss >> type;
    calc dist;
    if (type == "blocked") {
        dist = 0;
    } else {
        ss >> dist;
    }
    string state;
    ss >> state;
    if (state == "iphone") {
        cool = false;
    }

    return make_pair(type, dist);

    // return respond("activate", p);
}

point find_corner(const string mode) {
    const calc MAX_DIST = 6000;
    calc l = -MAX_DIST;
    calc r = MAX_DIST;
    calc border;
    if (mode == "top") {
        border = MAX_DIST;
    } else {
        border = -MAX_DIST;
    }

    calc last_dist;
    for (int i = 0; i < 80; i++) {
        calc lm = l + (r - l) * 3.0 / 11;
        calc rm = l + (r - l) * 8.0 / 11;
        // calc lm = l + (r - l) * 3.0 / 11;
        // calc rm = l + (r - l) * 8.0 / 11;
        point cpl, cpr;
        if (mode == "top") {
            cpl = point{lm, border};
            cpr = point{rm, border};
        } else {
            cpl = point{border, lm};
            cpr = point{border, rm};
        }
        auto respl = parse_response(make_request(cpl));
        auto respr = parse_response(make_request(cpr));
        // assert (respl.first == OUTSIDE && respr.first == OUTSIDE);
        last_dist = min(respl.second, respr.second);
        // cout << lm << " " << rm << endl;
        // cout << "From resp " << respl.second << " " << respr.second << endl;
        if (respl.second < respr.second) {
            r = rm;
        } else {
            l = lm;
        }
    }
    
    calc b_l = -MAX_DIST;
    calc b_r = l;
    for (int i = 0; i < 80; i++) {
        calc m = (b_l + b_r) / 2;
        point cp;
        if (mode == "top") {
            cp = point{m, border};
        } else {
            cp = point{border, m};
        }
        auto resp = parse_response(make_request(cp));
        // assert(resp.first == OUTSIDE);
        if (resp.second > last_dist) {
            b_l = m;
        } else {
            b_r = m;
        }
    }

    if (mode == "top") {
        return point{b_l, border - last_dist};
    } else {
        return point{border + last_dist, b_l};
    }
}

#ifdef LOCAL
void test();
#endif

point normalize_with_10(const point p) {
    const calc len_single = 10;
    calc len = sqrt(p.x * p.x + p.y * p.y);
    return point{len_single * p.x / len, len_single * p.y / len};
}

point revert_by_90(const point p) {
    return point{p.y * -1, p.x};
}

int cnt = 0;
bool active_query(int cx, int cy) {
    cnt++;
    if (cnt > 50) {
        return true;
    }
    auto p = parse_response(make_request(point{(double)cx, (double)cy}));
    if (p.first == INSIDE && p.second < 0.1) {
        cout << "found " << cx << " " << cy << endl;
        return true;
    }
    return false;
}

void active_search(point i, calc dist) {
    // cout << "Found close to " << i.x << " " << i.y << endl;
    int x = floor(i.x);
    int y = floor(i.y);
    for (int r = 0; ; r++) {
        int cx = x, cy = r + y;
        if (active_query(cx, cy)) {
            return;
        }
        for (int i = 0; i < r; i++) {
            cx++;
            cy--;
            if (active_query(cx, cy)) {
                return;
            }
        }
        for (int i = 0; i < r; i++) {
            cx--;
            cy--;
            if (active_query(cx, cy)) {
                return;
            }
        }
        for (int i = 0; i < r; i++) {
            cx--;
            cy++;
            if (active_query(cx, cy)) {
                return;
            }
        }
        for (int i = 0; i < r; i++) {
            cx++;
            cy++;
            if (active_query(cx, cy)) {
                return;
            }
        }
    }
}

void go_down(point i, point step_left, point step_down) {
    int left_steps = 0;
    while (true) {
        auto p = parse_response(make_request(i));
        if (p.first == BLOCKED) {
            left_steps++;
            i = i + step_left;
        } else if (p.first == INSIDE) {
            if (p.second < 8) {
                active_search(i, p.second);
                return;
            }
            left_steps++;
            i = i + step_left;
        } else {
            // assert(p.first == OUTSIDE);
            // assert(left_steps > 0);
            i = i + step_down;
            while (left_steps > 0) {
                i = i - step_left;
                left_steps--;
            }
        }
    }
}

int main() {
    cout << fixed << setprecision(10);
#ifdef LOCAL
    test();
#endif

    int m, n;
    cin >> m >> n;

    point ctop = find_corner("top");
    point cleft = find_corner("left");

    point step_left = normalize_with_10(cleft - ctop);
    point step_down = revert_by_90(step_left);
    point init_pos = point{
        ctop.x + step_left.x / 2 + step_down.x / 2,
        ctop.y + step_left.y / 2 + step_down.y / 2
    };
    go_down(init_pos, step_left, step_down);
}

#ifdef LOCAL
void test() {
    vector<point> rect = {
        point{1, 1}, point{-1, 1}, point{-1, -1}, point{1, -1}    
    };
    point treasure = point{0, 0};

    assert(in_rect(rect, treasure));
    assert(in_rect(rect, point{0.9, 0.9}));
    assert(in_rect(rect, point{-0.9, -0.9}));
    assert(in_rect(rect, point{-0.9, 0.9}));
    assert(in_rect(rect, point{0.9, -0.9}));
    assert(!in_rect(rect, point{1.9, -0.9}));
    assert(!in_rect(rect, point{0.9, -1.9}));

    assert(abs(calc_dist_to_walls(point{1, 1}, rect)) < 1e-9);
    assert(abs(calc_dist_to_walls(point{2, 1}, rect) - 1) < 1e-9);
    assert(abs(calc_dist_to_walls(point{100, 1}, rect) - 99) < 1e-9);
    assert(abs(calc_dist_to_walls(point{-1, 2}, rect) - 1) < 1e-9);
    assert(abs(calc_dist_to_walls(point{1, 0}, rect)) < 1e-9);
    assert(abs(calc_dist_to_walls(point{1.5, 0}, rect) - 0.5) < 1e-9);
    assert(abs(calc_dist_to_walls(point{2, 2}, rect) - sqrt(2)) < 1e-9);

    assert(abs(calc_dist_to_walls(point{6000, 8}, rect) - 5999.004084) < 1e-4);
    assert(abs(calc_dist_to_walls(point{6000, 1}, rect) - 5999.000083) < 1e-4);
}
#endif
