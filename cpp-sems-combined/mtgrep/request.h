#ifndef REQUEST_H
#define REQUEST_H

#include <string>

struct request {
    request(const std::string& substr_, const std::string& start_path_)
        : substr(substr_)
        , start_path(start_path_)
    {}

    std::string substr;
    std::string start_path;
};

struct location {
    location(const std::string& path_, int line_num_, int line_pos_)
        : path(path_)
        , line_num(line_num_)
        , line_pos(line_pos_)
        , success(true)
    {}
    location(const std::string& path_, bool failure_)
        : path(path_)
        , line_num(0)
        , line_pos(0)
        , success(failure_)
    {}

    std::string path;
    int line_num;
    int line_pos;
    bool success;
};

#endif // REQUEST_H
