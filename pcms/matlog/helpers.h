#ifndef HELPERS_H
#define HELPERS_H

#include <string>
#include <map>
#include <cassert>
#include <vector>
#include <iostream>
#include "types.h"

using namespace std;

vector<node> nodes;

statements_storage database;

void append_node(node v) {
    nodes.push_back(v);
    node* p = &(nodes.back());
    p->node_class = database.detect(p);
}

node* last_node() {
    return &(nodes.back());
}


node* make_impl(node* a, node* b) {
    append_node(node(IMPLICATION, a, b));
    return last_node();
}

node* make_and(node* a, node* b) {
    append_node(node('&', a, b));
    return last_node();
}

node* make_or(node* a, node* b) {
    append_node(node('|', a, b));
    return last_node();
}

node* negat(node* a) {
    append_node(node('!', a));
    return last_node();
}

void print_op_string(char c, string& res) {
    if (c == IMPLICATION) {
        res += "->";
    } else {
        res += c;
    }
}

void traverse(node* val, string& res) {
    if (!val->is_op) {
        res += val->var_name_if_present;
    } else {
        res += '(';
        if (is_unary(val->op)) {
            print_op_string(val->op, res);
            traverse(val->l, res);
        } else {
            traverse(val->l, res);
            res += ' ';
            print_op_string(val->op, res);
            res += ' ';
            traverse(val->r, res);
        }
        res += ')';
    }
}

string partial_string(node* v) {
    string s = "";
    traverse(v, s);
    return s;
}

void print_node(node* v) {
    string s = "";
    traverse(v, s);
    cout << s << endl;
}


node* wrap_in_ax_1(node* a, node* b) {
    return make_impl(a, make_impl(b, a));
}

node* wrap_in_ax_2(node* a, node* b, node* c) {
    node* first = make_impl(a, b);
    node* second = make_impl(a, make_impl(b, c));
    node* third = make_impl(a, c);
    return make_impl(first, make_impl(second, third));
}

node* wrap_in_ax_or(node* a, node* b) {
    return make_impl(a, b);
}

node* wrap_in_ax_and(node* a, node* b) {
    return make_impl(a, make_impl(b, make_and(a, b)));
}

node* wrap_in_ax_8(node* a, node* b, node* c) {
    node* first = make_impl(a, c);
    node* second = make_impl(b, c);
    node* third = make_impl(make_or(a, b), c);
    return make_impl(first, make_impl(second, third));
}

node* wrap_in_ax_9(node* a, node* b) {
    node* first = make_impl(a, b);
    node* second = make_impl(a, negat(b));
    return make_impl(first, make_impl(second, negat(a)));
}

node* wrap_in_ax_10(node* a) {
    return make_impl(negat(negat(a)), a);
}


void generate_helper(node* a, node* fp) {
    node* b = make_or(fp, negat(fp));
    node* notb = negat(b);
    node* v = wrap_in_ax_9(a, b);
    print_node(v);
    print_node(make_impl(a, b));
    v = v->r;
    print_node(v);
    node* tmp1 = wrap_in_ax_1(notb, a);
    print_node(tmp1);
    node* tmp2 = make_impl(make_impl(a, notb), negat(a));
    v = wrap_in_ax_1(tmp2, notb);
    print_node(v);
    v = v->r;
    print_node(v);
    v = wrap_in_ax_2(notb, make_impl(a, notb), negat(a));
    print_node(v);
    v = v->r;
    print_node(v);
    v = v->r;
    print_node(v);
}

void generate_a_or_not_a(node* a) { // EYECHECKED, TESTED
    generate_helper(a, a);
    node* nota = negat(a);
    generate_helper(nota, a);
    node* aornota = make_or(a, nota);
    node* v = wrap_in_ax_9(negat(aornota), nota);
    print_node(v);
    v = v->r;
    print_node(v);
    v = v->r;
    print_node(v);
    v = wrap_in_ax_10(aornota);
    print_node(v);
    v = v->r;
    print_node(v);
}

void format_context(vector<string>& varnames, vector<int>& selection, const string& s) {
    bool first = true;
    for (int i = 0; i < (int)selection.size(); i++) {
        if (selection[i] == SELECTED_TRUE) {
            if (!first) {
                cout << ", ";
            }

            first = false;
            cout << varnames[i];
        }
        if (selection[i] == SELECTED_FALSE) {
            if (!first) {
                cout << ", ";
            }

            first = false;
            cout << "!" + varnames[i];
        }
    }
    cout << " |- " << s << endl;
}

void pretty_print_selection(const vector<int>& selection) {
    for (auto v : selection) {
        cout << selection_to_string(v) << " ";
    }
    cout << endl;
}

#endif
