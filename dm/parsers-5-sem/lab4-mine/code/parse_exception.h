#ifndef PARSE_EXCEPTION_H
#define PARSE_EXCEPTION_H

#include <cstring>
#include <exception>
#include <string>

using namespace std;

struct parse_exception : public exception {
  parse_exception(size_t pos, const string &msg) {
    this->err_pos = pos;
    this->msg = strdup(msg.c_str());
  }

  const char *what() const throw();
  size_t where() const throw();

  const char *msg;
  size_t err_pos;
};

#endif
