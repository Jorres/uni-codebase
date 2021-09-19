#ifndef CHECKER_H
#define CHECKER_H

#include <string>
#include <vector>

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

std::pair<std::string, calc> respond(std::string request, point p);

calc calc_dist(point a, point b);

calc dot(point a, point b);

calc calc_dist_to_segment(point p, point a, point b);

calc calc_dist_to_walls(point p, const std::vector<point>& rect);

bool in_triangle(point a, point b, point c, point x);

bool in_rect(const std::vector<point>& rect, point p);

#endif

