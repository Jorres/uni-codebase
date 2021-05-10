#pragma once

#include "help.h"

struct proof_info {
    int is_proof;
    int type;
    int n1, n2;

    proof_info() : is_proof(0) {}

    proof_info(const proof_info& other) = default;

    proof_info(int type1, int n11, int n21) : is_proof(1), type(type1), n1(n11), n2(n21) {}
};


struct test_node {
    test_node* le;
    test_node* r;

    int type;
    int vis;
}; 

test_node test_nodes[100];
int test_nodes_pt = 0;

void reset_test() {
    test_nodes_pt = 0;    
}

test_node* new_var_test() {
    test_node* t = test_nodes + (test_nodes_pt++);
    t -> type = 4;
    t -> vis = -1;
    return t;
}

test_node* new_not_test(test_node* t1) {
    test_node* t2 = test_nodes + (test_nodes_pt++);
    t2 -> type = 3;
    t2 -> le = t1;
    return t2;
}

test_node* new_bi_test(int type, test_node* le, test_node* r) {
    test_node* t = test_nodes + (test_nodes_pt++);
    t -> type = type;
    t -> le = le;
    t -> r = r;
    return t;
}

int check_node(node* n, test_node* t) {
    if (t -> type == 4) {
        if (t -> vis == -1) {
            t -> vis = n -> code;
            return 1;
        }
        return t -> vis == n -> code;
    }
    if (t -> type != n -> type) {
        return 0;
    }
    if (t -> type == 3) {
        node_not* nn = dynamic_cast<node_not*>(n);
        return check_node(nn -> n, t -> le);
    }
    bi_node* nb = dynamic_cast<bi_node*>(n);
    return check_node(nb -> le, t -> le) && check_node(nb -> r, t -> r);
}



proof_info is_ax(int code) {
    node* n = code_to_node[code];
    {
        reset_test();
        test_node* a = new_var_test();
        test_node* b = new_var_test();
        if (check_node(n, new_bi_test(0, a, new_bi_test(0, b, a)))) {
            return proof_info(0, 1, 0);
        }
    }
    {
        reset_test();
        test_node* a = new_var_test();
        test_node* b = new_var_test();
        test_node* c = new_var_test();
        if (check_node(n, new_bi_test(0, 
            new_bi_test(0, a, b),
            new_bi_test(0,
                new_bi_test(0, a, new_bi_test(0, b, c)),
                new_bi_test(0, a, c))))) {
            return proof_info(0, 2, 0);
        }
    }
    {
        reset_test();
        test_node* a = new_var_test();
        test_node* b = new_var_test();
        if (check_node(n, new_bi_test(0, 
            new_bi_test(2, a, b), a))) {
            return proof_info(0, 3, 0);
        }
    }
    {
        reset_test();
        test_node* a = new_var_test();
        test_node* b = new_var_test();
        if (check_node(n, new_bi_test(0, 
            new_bi_test(2, a, b), b))) {
            return proof_info(0, 4, 0);
        }
    }
    {
        reset_test();
        test_node* a = new_var_test();
        test_node* b = new_var_test();
        if (check_node(n, new_bi_test(0, 
            a, new_bi_test(0, b,
                new_bi_test(2, a, b))))) {
            return proof_info(0, 5, 0);
        }
    }
    {
        reset_test();
        test_node* a = new_var_test();
        test_node* b = new_var_test();
        if (check_node(n, new_bi_test(0, 
            a, new_bi_test(1, a, b)))) {
            return proof_info(0, 6, 0);
        }
    }
    {
        reset_test();
        test_node* a = new_var_test();
        test_node* b = new_var_test();
        if (check_node(n, new_bi_test(0, 
            b, new_bi_test(1, a, b)))) {
            return proof_info(0, 7, 0);
        }
    }
    {
        reset_test();
        test_node* a = new_var_test();
        test_node* b = new_var_test();
        test_node* c = new_var_test();
        if (check_node(n, new_bi_test(0,
            new_bi_test(0, a, c), 
            new_bi_test(0, 
                new_bi_test(0, b, c),
                new_bi_test(0, new_bi_test(1, a, b), c))))) {
            return proof_info(0, 8, 0);
        }
    }
    {
        reset_test();
        test_node* a = new_var_test();
        test_node* b = new_var_test();
        if (check_node(n, new_bi_test(0,
            new_bi_test(0, a, b), 
            new_bi_test(0, 
                new_bi_test(0, a, new_not_test(b)),
                new_not_test(a))))) {
            return proof_info(0, 9, 0);
        }
    }
    {
        reset_test();
        test_node* a = new_var_test();
        if (check_node(n, new_bi_test(0,
            new_not_test(new_not_test(a)),
            a))) {
            return proof_info(0, 10, 0);
        }
    }
    return proof_info();
}

