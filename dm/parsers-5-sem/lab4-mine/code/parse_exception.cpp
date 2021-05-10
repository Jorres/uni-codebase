#include "parse_exception.h"

const char *parse_exception::what() const throw() { return msg; }

size_t parse_exception::where() const throw() { return err_pos; }
