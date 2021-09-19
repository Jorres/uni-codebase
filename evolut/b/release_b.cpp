#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <cassert>
#include <cmath>
#include <iomanip>

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

using namespace std;

enum RESPONSE_STATE {
    UNSET, INSIDE, OUTSIDE, BLOCKED
};

pair<RESPONSE_STATE, calc> parse_response(pair<string, calc> ans_data) {
    // assert(ans_data.first == "inside" || ans_data.first == "outside" || ans_data.first == "blocked");
    RESPONSE_STATE state = UNSET;
    if (ans_data.first == "inside") {
        state = INSIDE;
    }
    if (ans_data.first == "outside") {
        state = OUTSIDE;
    }
    if (ans_data.first == "blocked") {
        state = BLOCKED;
    }
    assert(state != UNSET);
    return make_pair(state, ans_data.second);
}

bool cool = true;
int queries = 0;
bool searching_phase = false;
pair<string, calc> make_request(point p) {
    if (!cool || queries >= 2967) {
        assert(searching_phase);
    }
    // assert(cool && searching_phase);
    // assert(queries < 2967 && searching_phase);
    queries++;

    cout << "activate " << p.x << " " << p.y << endl;
    string type;
    cin >> type;

    calc dist;
    if (type == "blocked") {
        dist = 0;
    } else {
        cin >> dist;
    }

    string state;
    cin >> state;
    if (state == "iphone") {
        cool = false;
    }
    assert(type == "blocked" || type == "inside" || type == "outside"); // FAILS

    return make_pair(type, dist);
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
    for (int i = 0; i < 60; i++) {
        calc lm = l + (r - l) * 1.0 / 3;
        calc rm = l + (r - l) * 2.0 / 3;
        point cpl, cpr;
        if (mode == "top") {
            cpl = point{lm, border};
            cpr = point{rm, border};
        } else {
            cpl = point{border, lm};
            cpr = point{border, rm};
        }
        auto request = make_request(cpl);
        auto respl = parse_response(request);
        request = make_request(cpr);
        auto respr = parse_response(request);
        // assert (respl.first == OUTSIDE && respr.first == OUTSIDE);
        last_dist = min(respl.second, respr.second);
        if (respl.second < respr.second) {
            r = rm;
        } else {
            l = lm;
        }
    }
    
    calc b_l = -MAX_DIST;
    calc b_r = l;
    for (int i = 0; i < 60; i++) {
        calc m = (b_l + b_r) / 2;
        point cp;
        if (mode == "top") {
            cp = point{m, border};
        } else {
            cp = point{border, m};
        }
        auto resp = parse_response(make_request(cp));
        assert(resp.first == OUTSIDE);
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

point normalize_with_10(const point p) {
    const calc len_single = 10;
    calc len = sqrt(p.x * p.x + p.y * p.y);
    return point{len_single * p.x / len, len_single * p.y / len};
}

point revert_by_90(const point p) {
    return point{p.y * -1, p.x};
}

bool active_query(int cx, int cy) {
    auto p = parse_response(make_request(point{(double)cx, (double)cy}));
    if (p.first == INSIDE && p.second < 0.1) {
        cout << "found " << cx << " " << cy << endl;
        return true;
    }
    return false;
}

bool try_closer_direction(int sx, int sy, int x, int y, calc& best) {
    auto resp = parse_response(make_request(point{(double) x + sx, (double)y + sy}));

    if (resp.first == INSIDE && resp.second < best) {
        best = resp.second;
        return true;
    }
    return false;
}

point try_closer(point i, calc dist) {
    int x = floor(i.x);
    int y = floor(i.y);
    while (true) {
        if (try_closer_direction(+1, 0, x, y, dist)) {
            x++;
        } else if (try_closer_direction(-1, 0, x, y, dist)) {
            x--;
        } else if (try_closer_direction(0, 1, x, y, dist)) {
            y++;
        } else if (try_closer_direction(0, -1, x, y, dist)) {
            y--;
        } else {
            break;
        }
    }
    return point{(double)x, (double)y};
}

void active_search(point i, calc dist) {
    searching_phase = true;
    i = try_closer(i, dist);
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
            assert(left_steps > 0);
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
    string verdict;
    cin >> verdict;
    assert(verdict == "OK");
}
