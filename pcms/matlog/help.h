#pragma once

typedef std::pair<int, std::pair<int, int> > piii;

piii make_piii(int a, int b, int c) {
    return std::make_pair(a, std::make_pair(b, c));
}

int next_code = 0;
std::map<piii, int> codes;
std::map<std::string, int> var_codes;

struct node {
    int type;
    int code;
    virtual void print() = 0;

    node(int type1) : type(type1) {};
};

std::map<int, node*> code_to_node;

template <typename U, typename T>
int add_node(U t, T* n, std::map<U, int>& codes_map) {
    if (codes_map.count(t)) {
        return codes_map[t];
    }
    codes_map[t] = next_code;
    code_to_node[next_code] = dynamic_cast<node*>(n);
    return next_code++;
}

struct bi_node : public node {
    node* le, *r;

    bi_node(node* le1, node* r1, int type1) : node(type1), le(le1), r(r1) {
        code = add_node(make_piii(type, le -> code, r -> code), this, codes);
    }
    
    void help_print(const std::string& s) {
        std::cout << "(";
        le->print();
        std::cout << " " << s << " ";
        r->print();
        std::cout << ")";    
    }
};

struct node_and : public bi_node {
    node_and(node* le1, node* r1) : bi_node(le1, r1, 2) {}

    void print() {
        help_print("&");
    }
};

struct node_or : public bi_node {
    node_or(node* le1, node* r1) : bi_node(le1, r1, 1) {}

    void print() {
        help_print("|");
    }
};

struct node_arr : public bi_node {
    node_arr(node* le1, node* r1) : bi_node(le1, r1, 0) {}

    void print() {
        help_print("->");
    }
};

struct node_not : public node {
    node* n;
    
    node_not(node* n1) : node(3), n(n1) {
        code = add_node(make_piii(type, n1 -> code, 0), this, codes);
    }

    void print() {
        std::cout << "!";
        n->print();
    }
};

struct node_var : public node {
    std::string var_name;

    node_var(const std::string& name1) : node(4), var_name(name1) {
        code = add_node(name1, this, var_codes);
    }

    void print() {
        std::cout << var_name;
    }
};





template <typename T>
node* do_cast(std::list<T>& v) {
    return dynamic_cast<node*>(&v.back());
}

template <typename T>
node* get_next_bi(std::list<T>& v, node* le, node* r) {
    v.emplace_back(le, r);
    return do_cast(v);    
}

std::list<node_and> vand;
std::list<node_or> vor;
std::list<node_arr> varr;
std::list<node_not> vnot;
std::list<node_var> vvar;

void skip_space(const std::string& s, int &p) {
    while (s[p] == ' ') {
        ++p;
    }
}

node* do_parse(const std::string& s, int &p, int lvl) {
    node* n;
    skip_space(s, p);
    if (s[p] >= 'A' && s[p] <= 'Z') {
        std::string var_name;
        while ((s[p] >= 'A' && s[p] <= 'Z') || (s[p] >= '0' && s[p] <= '9') || s[p] == '\'') {
            var_name.push_back(s[p++]);
        }
        vvar.emplace_back(var_name);
        n = do_cast(vvar); 
    }
    if (s[p] == '!') {
        ++p;
        vnot.emplace_back(do_parse(s, p, 3));
        n = do_cast(vnot);
    }
    if (s[p] == '(') {
        ++p;
        n = do_parse(s, p, 0);
        ++p;
    }

    if (lvl == 3) {
        return n;
    }

    while (true) {
        skip_space(s, p);
        if (s[p] == ')') {
            return n;
        }
       
        if (s[p] == '-') {
            if (lvl == 0) {
                p += 2;
                return get_next_bi(varr, n, do_parse(s, p, 0));
            }
            return n;
        }

        int cur_lvl;
        if (s[p] == '|') {
            cur_lvl = 1;
        } else if (s[p] == '&') {
            cur_lvl = 2;
        }
       
        if (cur_lvl <= lvl) {
            return n;
        }
        ++p;
        node* r = do_parse(s, p, cur_lvl);
        if (cur_lvl == 1) {
            n = get_next_bi(vor, n, r);
        } else {
            n = get_next_bi(vand, n, r);
        }
    }
}

node* parse(const std::string& s) {
    int p = 0;
    return do_parse("(" + s + ")", p, 3);
}
