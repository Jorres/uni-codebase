#include <cassert>
#include <fstream>

#include "tests.h"

extern vector<int> line_results;
extern int yylex_destroy(void);

void t_tester::run_suite() {
    run_single_test("Simplest calculation, no assignment", "2 + 2 * 2;", {6});
    run_single_test("Simplest statement", "a = 2;", {2});
    run_single_test("Using another variable in assignment", "a = 2;\nb = a;", {2, 2});
    run_single_test("Check +", "a = 2;\nb = a + 2;", {2, 4});
    run_single_test("Check *", "a = 2;\nb = a * 4;", {2, 8});
    run_single_test("Check /", "a = 2;\nb = a / a;", {2, 1});
    run_single_test("Check complex var name", "abc123abc = 2;", {2});

    run_single_test("Check power operator", "a = 2 ** 10;", {1024});
    run_single_test("Check power operator assoc", "a = 2 ** 3 ** 3;", {134217728});

    run_single_failing_test("No semicolon at the end", "a = 2");
    run_single_failing_test("Check / by zero", "a = 2;\nb = a / 0;");

    cout << "Alright!" << endl;
}

void t_tester::run_single_failing_test(const string& message, const string& data) {
    try {
        run_single_test(message, data, {});
        throw "Parsing should fail but it didn't";
    } catch (const char* s) {
        cout << s << endl;
    }
}

void t_tester::run_single_test(const string& message, const string& data, const vector<int>& expected) {
    extern int yylineno;
    yylineno = 0;

    cout << message << endl;

    fstream fs;
    fs.open("input.txt", fstream::out);
    fs << data;
    fs.close();

    freopen("input.txt", "r", stdin);
    int ret = yyparse();
    assert(ret == 0);
    assert(line_results == expected);
    line_results.clear();
    yylex_destroy();
}
