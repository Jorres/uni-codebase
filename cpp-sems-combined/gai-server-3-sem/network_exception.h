#ifndef NETWORK_EXCEPTION_H
#define NETWORK_EXCEPTION_H

#include <stdexcept>
#include <string>

#define THROW_NETWORK_EXCEPTION(MSG) throw network::exception(std::string(MSG))

namespace network {

class exception : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
    using std::runtime_error::what;
};

}

#endif // NETWORK_EXCEPTION_H
