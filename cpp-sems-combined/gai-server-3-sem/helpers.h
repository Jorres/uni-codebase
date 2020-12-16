#include "stdlib_includes.h"
#include "config.h"

template<typename T>
void log(T elem) {
#ifdef LOG_ENABLED
    std::cout << elem << std::endl;
#endif
}

template<typename T, typename... Args>
void log(T elem, Args... args) {
#ifdef LOG_ENABLED
    std::cout << elem << " ";
    log(args...);
#endif
}

std::string ltrim(std::string&& str, const std::string& chars = "\t\n\v\f\r ");
std::string rtrim(std::string& str, const std::string& chars = "\t\n\v\f\r ");
std::string trim(std::string str, const std::string& chars = "\t\n\v\f\r ");

void exception_wrap(int response_code);
