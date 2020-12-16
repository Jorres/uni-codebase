#ifndef EXCEPTION_H
#define EXCEPTION_H
struct custom_exception {
	custom_exception(std::string & s) : message(s) {}
	std::string message;
};

void throw_exception(std::string data);
#endif 