#include <iostream>     
#include <string>
#include <vector>
#include <cctype>
#include <cassert>
#include <map>
#include <set>
#include <cwchar>

using namespace std;

class parser {
public:
    parser() {
        s += '(';
        char c;
        while (cin >> c) {
            s += c;
        }
        s += ')';
        init_priorities();
        init_operations();
    }

    void parse() {
        for (int i = 0; i < (int)s.size(); i++) {
            char c = s[i];
            if (isspace(c)) {
                continue;
            } else {
                if (c == '(') {
                    ops.push_back(c);
                } else if (c == ')') {
                    while (ops.back() != '(') {
                        char op = ops.back();
                        ops.pop_back();
                        process_op(op);
                    }
                    ops.pop_back();
                } else if (is_operation(i)) {
                    char op = s[i];
                    if (s[i] == '>') {
                        op = '#';
                    }
                    while (!ops.empty() && ((left_assoc(op) && priorities[op] <= priorities[ops.back()]) ||
                                            (!left_assoc(op) && priorities[op] < priorities[ops.back()]))) {
                        char s_op = ops.back();
                        ops.pop_back();
                        process_op(s_op);
                    }
                    ops.push_back(op);
                } else if (isupper(c)) { // parse variable
                    string var = {s[i++]};
                    var += parse_variable(i); // i gets on last letter of var
                    vars.push_back(var);
                } else {
                    assert(false);
                }
            }
        }
    }

    void print_result() {
        cout << vars.back() << endl;
    }

private:
    bool is_unary(char c) {
        return c == '!';
    }

    bool left_assoc(char c) {
        return c != '#' && c != '!';
    }

    void process_op(char c) {
        if (is_unary(c)) {
            string last = vars.back();
            vars.pop_back();
            string tmp = {c};
            last = "(" + tmp  + last + ")";
            vars.push_back(last);
        } else {
            string r = vars.back(); vars.pop_back();
            string l = vars.back(); vars.pop_back();
            string op = {c};
            if (c == '#') {
                op = "->";
            }
            vars.push_back("(" + op + "," + l + "," + r + ")");
        }
    }

    string parse_variable(int& pos) {
        string res = "";
        while (pos < (int)s.size()) {
            char c = s[pos];
            if (!(isupper(c) || isdigit(c) || c == '\'')) {
               break;
            }
            res += c;
            pos++;
        }
        pos--;
        return res;
    }

    void init_priorities() {
        priorities['&'] = 10;
        priorities['#'] = 6;
        priorities['|'] = 8;
        priorities['!'] = 12;
        priorities['('] = 0;
    }

    void init_operations() {
        operations.insert('#');
        operations.insert('&');
        operations.insert('|');
        operations.insert('!');
    }

    bool is_operation(int& pos) {
        char op = s[pos];
        if (s[pos] == '-') {
            pos++;
            assert(pos < (int)s.size() && s[pos] == '>');
            op = '#';
        }
        return operations.count(op) > 0;
    }

    string s;
    string res;
    map<char, int> priorities;
    set<char> operations;
    vector<string> vars;
    vector<char> ops;
};

int main() {
    // freopen("input.txt", "r", stdin);
    parser p = parser();
    p.parse();
    p.print_result();
}
