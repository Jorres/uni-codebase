#include "stdlib_includes.h"
#include "epoll_and_sockets.h"

#include "network_exception.h"

std::string ltrim(std::string&& str, const std::string& chars = "\t\n\v\f\r ") {
    return str.erase(0, str.find_first_not_of(chars));
}

std::string rtrim(std::string& str, const std::string& chars = "\t\n\v\f\r ") {
    return str.erase(str.find_last_not_of(chars) + 1);
}

std::string trim(std::string str, const std::string& chars = "\t\n\v\f\r ") {
    return ltrim(rtrim(str, chars), chars);
}

void exception_wrap(int response_code) {
    if (response_code == -1) {
        std::string msg = gai_strerror(errno);
        msg.push_back('\n');
        THROW_NETWORK_EXCEPTION(msg);
    }
}
