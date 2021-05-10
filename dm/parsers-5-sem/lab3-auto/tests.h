#ifndef TESTS_H
#define TESTS_H
#include <iostream>
#include <string>
#include <vector>

using namespace std;


int yyparse(void);

class t_tester {
public:
    void run_suite();
private:
    void run_single_test(const string& message, const string& data, const vector<int>& answers);
    void run_single_failing_test(const string& message, const string& data);
};

#endif
