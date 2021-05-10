#ifndef TYPES_H
#define TYPES_H

#include <string>
#include <map>

using namespace std;

struct node {
    static const char NEUTRAL_OP = '<';
    node()                             : l(nullptr), r(nullptr) {
        op = NEUTRAL_OP;
        is_op = false;
        my_class = -1;
    }
    node(const string& name)           : l(nullptr), r(nullptr), val(name) {
        op = NEUTRAL_OP;
        is_op = false;
        my_class = -1;
    }

    node(char _op, node* _l)           : l(_l), r(nullptr), op(_op), is_op(true) {
        my_class = -1;
    }

    node(char _op, node* _l, node* _r) : l(_l), r(_r),      op(_op), is_op(true) {
        my_class = -1;
    }

    node* l;
    node* r;
    char op;
    bool is_op;
    string val;
    int my_class;
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

struct source {
    source() {
        modusponens = axiom = hypo = false;
        num_of = left = right = -100;
    }
    bool modusponens, axiom, hypo;
    int num_of;
    int left, right;
};

bool is_unary(char c) {
    return c == '!';
}

struct statements_storage {
    int detect(node* v) {
        signature s;
        if (!v->is_op) {
            s.name = v->val;
        } else {
            s.op = v->op;
            s.class_left = v->l->my_class;
            if (!is_unary(v->op)) {
                s.class_right = v->r->my_class;
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
