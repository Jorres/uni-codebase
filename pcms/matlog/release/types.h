#ifndef TYPES_H
#define TYPES_H

#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <cassert>
#include <map>
#include <set>

using namespace std;

const int UNSELECTED = 55;
const int SELECTED_TRUE = 56;
const int SELECTED_FALSE = 57;
const int REMAINING_TRUE = 58;
const int REMAINING_FALSE = 59;
const int TRANSFERRED = 59;
const int UNDEFINED = 60;

const char IMPLICATION = '#';

#define endl '\n'

string selection_to_string(int value) {
    if (value == UNSELECTED) {
        return "UNSELECTED";
    }
    if (value == SELECTED_TRUE) {
        return "SELECTED_TRUE";
    }
    if (value == SELECTED_FALSE) {
        return "SELECTED_FALSE";
    }
    if (value == REMAINING_TRUE) {
        return "REMAINING_TRUE";
    }
    if (value == REMAINING_FALSE) {
        return "REMAINING_FALSE";
    }
    return "unknown";
}

struct node {
    static const char NEUTRAL_OP = '<';
    node()                             : l(nullptr), r(nullptr) {
        op = NEUTRAL_OP;
        is_op = false;
        value = false;
        node_class = -1;
    }
    node(const string& name)           : l(nullptr), r(nullptr), var_name_if_present(name) {
        op = NEUTRAL_OP;
        is_op = false;
        value = false;
        node_class = -1;
    }
    node(char _op, node* _l)           : l(_l), r(nullptr), op(_op), is_op(true) {
        node_class = -1;
        value = false;
    }
    node(char _op, node* _l, node* _r) : l(_l), r(_r),      op(_op), is_op(true) {
        node_class = -1;
        value = false;
    }
    node* l;
    node* r;
    char op;
    bool is_op;
    string var_name_if_present;
    int node_class;
    bool value;
};

struct origin {
    bool axiom;
    node* leftmp;
    origin() {

    }
    origin(bool a) {
        assert(a);
        axiom = true;
        leftmp = nullptr;
    }

    origin(bool a, node* p) {
        assert(!a);
        axiom = false;
        leftmp = p;
    }
};

struct signature {
    static const char DEFAULT_OP = '<';
    string name;
    char op;
    int class_left;
    int class_right;

    signature() {
        class_left = class_right = -1;
        op = DEFAULT_OP;
    }

    bool operator<(const signature& rhs) const {
        if (!name.empty()) {
            if (!rhs.name.empty()) {
                return name < rhs.name;
            }
            return true;
        } else {
            if (!rhs.name.empty()) {
                return false;
            }
            if (op != rhs.op) {
                return op < rhs.op;
            }

            if (class_left != rhs.class_left) {
                return class_left < rhs.class_left;
            }
            return class_right < rhs.class_right;
        }
    }
};

bool is_unary(char c) {
    return c == '!';
}

struct statements_storage {
    int detect(const node* v) {
        signature s;
        if (!v->is_op) {
            s.name = v->var_name_if_present;
        } else {
            s.op = v->op;
            s.class_left = v->l->node_class;
            if (!is_unary(v->op)) {
                s.class_right = v->r->node_class;
            }
        }
        auto it = classes.find(s);
        if (it != classes.end()) {
            return it->second;
        }
        int new_num = classes.size();
        classes.insert(make_pair(s, new_num));

        return new_num;
    }

    map<signature, int> classes;
};

#endif  // TYPES_H
