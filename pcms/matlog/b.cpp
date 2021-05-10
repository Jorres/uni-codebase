#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <cassert>
#include <map>
#include <set>
#include <cwchar>

#include "b_types.h"

using namespace std;
#define endl '\n'
const char IMPLICATION = '#';

statements_storage database;

class parser {
public:
    parser(const string& _s) {
        s = "(" + _s + ")";
        init_priorities();
        init_operations();
        success = false;
        parse();
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
                    if (s[i] == '-') {
                        i++;
                        op = IMPLICATION;
                    }
                    while (!ops.empty() && ((left_assoc(op) && priorities[op] <= priorities[ops.back()]) ||
                                            (!left_assoc(op) && priorities[op] < priorities[ops.back()]))) {
                        char s_op = ops.back();
                        ops.pop_back();
                        process_op(s_op);
                    }
                    ops.push_back(op);
                } else if (isupper(c)) {
                    string var = parse_variable(i);
                    vars.push_back(new node(var));
                    vars.back()->my_class = database.detect(vars.back());
                } else {
                    assert(false);
                }
            }
        }
        assert(vars.size() == 1);
        assert(ops.size() == 0);
        success = true;
    }

    bool failed() const {
        return !success;
    }

    node* get_result() {
        if (!success) {
            return nullptr;
        }
        return vars.back();
    }

    void print_result() const {
        // traverse(vars.back());
        cout << s;
    }

    void print_nodes() const {
        traverse(vars.back());
    }

    void print_op_string(char c) const {
        if (c == IMPLICATION) {
            cout << "->";
        } else {
            cout << c;
        }
    }

    void traverse(node* val) const {
        if (!val->is_op) {
            cout << val->val;
            return;
        } else {
            cout << "(";
            print_op_string(val->op);
            if (is_unary(val->op)) {
                traverse(val->l);
                cout << ")";
            } else {
                cout << ",";
                traverse(val->l);
                cout << ",";
                traverse(val->r);
                cout << ")";
            }
        }
    }

private:
    bool left_assoc(char c) {
        return c != IMPLICATION && c != '!';
    }

    void process_op(char c) {
        node* cur;
        if (is_unary(c)) {
            node* last = vars.back(); vars.pop_back();
            cur = new node(c, last);
            vars.push_back(cur);
        } else {
            node* r = vars.back(); vars.pop_back();
            node* l = vars.back(); vars.pop_back();
            cur = new node(c, l, r);
            vars.push_back(cur);
        }
        cur->my_class = database.detect(cur);
    }

    string parse_variable(int& pos) {
        string res = "";
        while (pos < (int)s.size()) {
            char c = s[pos];
            // first is always upper, by call where isupper(c)
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
        priorities[IMPLICATION] = 6;
        priorities['|'] = 8;
        priorities['!'] = 12;
        priorities['('] = 0;
    }

    void init_operations() {
        operations.insert(IMPLICATION);
        operations.insert('&');
        operations.insert('|');
        operations.insert('!');
    }

    bool is_operation(int pos) {
        char op = s[pos];
        if (s[pos] == '-') {
            pos++;
            assert(pos < (int)s.size() && s[pos] == '>');
            op = IMPLICATION;
        }
        return operations.count(op) > 0;
    }

    bool success;
    string s;
    map<char, int> priorities;
    set<char> operations;
    vector<node*> vars;
    vector<char> ops;
};

map<int, int> old_to_new;
map<int, int> hypotheses;
map<int, int> class_to_string;
map<int, vector<pair<int, int>>> to_mp;
set<int> valid_classes;

string parse_context() {
    string context;
    char c;
    bool still_context = true;
    while (still_context && cin >> c) {
        if (c == '|') {
            cin >> c;
            if (c == '-') {
                still_context = false;
            } else {
                context.push_back('|');
                context.push_back(c);
            }
        } else {
            context.push_back(c);
        }
    }
    return context;
}

bool check_op(node* v, char c) {
    return v->is_op && v->op == c;
}

bool check_ax_1(parser& p) {
    node* v = p.get_result();
    if (!check_op(v, IMPLICATION) || !check_op(v->r, IMPLICATION)) {
        return false;
    }
    int left = v->l->my_class;
    int right = v->r->r->my_class;
    return left == right;
}

bool check_ax_2(parser& p) {
    node* v = p.get_result();
    if (!check_op(v, IMPLICATION)) {
        return false;
    }
    node* L = v->l;
    node* R = v->r;
    if (!check_op(L, IMPLICATION) || !check_op(R, IMPLICATION)) {
        return false;
    }
    node* RL = R->l;
    node* RR = R->r;
    if (!check_op(RL, IMPLICATION) || !check_op(RR, IMPLICATION)) {
        return false;
    }
    node* F = RL->r;
    if (!check_op(F, IMPLICATION)) {
        return false;
    }
    int one = L->l->my_class;
    int two = L->r->my_class;
    int three = RL->l->my_class;
    int four = F->l->my_class;
    int five = F->r->my_class;
    int six = RR->l->my_class;
    int seven = RR->r->my_class;
    return one == three && one == six && two == four && five == seven;
}

bool check_ax_4(parser& p) {
    node* v = p.get_result();
    if (!check_op(v, IMPLICATION) || !check_op(v->l, '&')) {
        return false;
    }

    return v->l->l->my_class == v->r->my_class;
}

bool check_ax_5(parser& p) {
    node* v = p.get_result();
    if (!check_op(v, IMPLICATION) || !check_op(v->l, '&')) {
        return false;
    }

    return v->l->r->my_class == v->r->my_class;
}

bool check_ax_3(parser& p) {
    node* v = p.get_result();
    if (!check_op(v, IMPLICATION)) {
        return false;
    }
    node* s = v->r;
    int one = v->l->my_class;
    if (!check_op(s, IMPLICATION)) {
        return false;
    }
    int two = s->l->my_class;
    node* t = s->r;
    if (!check_op(t, '&')) {
        return false;
    }
    int three = t->l->my_class;
    int four = t->r->my_class;
    return one == three && two == four;
}

bool check_ax_6(parser& p) {
    node* v = p.get_result();
    if (!check_op(v, IMPLICATION)) {
        return false;
    }
    node* s = v->r;
    if (!check_op(s, '|')) {
        return false;
    }
    int left = v->l->my_class;
    int right = s->l->my_class;
    return left == right;
}

bool check_ax_7(parser& p) {
    node* v = p.get_result();
    if (!check_op(v, IMPLICATION)) {
        return false;
    }
    node* s = v->r;
    if (!check_op(s, '|')) {
        return false;
    }
    int left = v->l->my_class;
    int right = s->r->my_class;
    return left == right;
}

bool check_ax_8(parser& p) {
    node* v = p.get_result();
    if (!check_op(v, IMPLICATION)) {
        return false;
    }
    node* L = v->l;
    node* R = v->r;
    if (!check_op(L, IMPLICATION) || !check_op(R, IMPLICATION)) {
        return false;
    }
    node* RL = R->l;
    node* RR = R->r;
    if (!check_op(RL, IMPLICATION) || !check_op(RR, IMPLICATION)) {
        return false;
    }
    node* F = RR->l;
    if (!check_op(F, '|')) {
        return false;
    }
    int one = L->l->my_class;
    int two = L->r->my_class;
    int three = RL->l->my_class;
    int four = RL->r->my_class;
    int five = F->l->my_class;
    int six = F->r->my_class;
    int seven = RR->r->my_class;
    return one == five && two == four && two == seven && three == six;
}

bool check_ax_9(parser& p) {
    node* v = p.get_result();
    if (v->is_op && v->op == IMPLICATION) {
        node* s = v->l;
        if (!check_op(s, IMPLICATION)) {
            return false;
        }
        int one = s->l->my_class;
        int two = s->r->my_class;
        node* F = v->r;
        if (!check_op(F, IMPLICATION)) {
            return false;
        }
        node* L = F->l;
        node* R = F->r;
        if (!check_op(L, IMPLICATION)) {
            return false;
        }
        int three = L->l->my_class;
        if (!check_op(L->r, '!')) {
            return false;
        }
        int four = L->r->l->my_class;
        if (!check_op(R, '!')) {
            return false;
        }
        int five = R->l->my_class;
        return one == three && two == four && one == five;
    }
    return false;
}

bool check_ax_10(parser& p) {
    node* v = p.get_result();
    if (!check_op(v, IMPLICATION)) {
        return false;
    }
    node* s = v->l;
    int right = v->r->my_class;
    if (!check_op(s, '!')) {
        return false;
    }
    node* t = s->l;
    if (!check_op(t, '!')) {
        return false;
    }
    int left = t->l->my_class;
    return left == right;
}

bool validate(parser& p, source& ann, int string_num) {
    if (p.failed()) {
        return false;
    }

    node* root = p.get_result();
    int cur_class = root->my_class;
    class_to_string[cur_class] = string_num;
    if (hypotheses.count(cur_class)) {
        // cout << "hypo found " << string_num  + 1 << endl;
        ann.hypo = true;
        ann.num_of = hypotheses[cur_class];
        valid_classes.insert(cur_class);
    } else {
        // cout << "check for ax line " << string_num  + 1 << endl;
        ann.num_of = -1;
        if (check_ax_1(p))  {
            ann.num_of = 1;
        } else if (check_ax_2(p))  {
            ann.num_of = 2;
        } else if (check_ax_3(p))  {
            ann.num_of = 3;
        } else if (check_ax_4(p))  {
            ann.num_of = 4;
        } else if (check_ax_5(p))  {
            ann.num_of = 5;
        } else if (check_ax_6(p))  {
            ann.num_of = 6;
        } else if (check_ax_7(p))  {
            ann.num_of = 7;
        } else if (check_ax_8(p))  {
            ann.num_of = 8;
        } else if (check_ax_9(p))  {
            ann.num_of = 9;
        } else if (check_ax_10(p))  {
            ann.num_of = 10;
        }
        ann.axiom = ann.num_of != -1;
        // cout << "ax is " << ann.num_of << endl;
        if (ann.num_of != -1) {
            valid_classes.insert(cur_class);
        } else { // if I am someone's MP
            for (auto it : to_mp[cur_class]) {
                if (valid_classes.count(it.first) && valid_classes.count(it.second)) {
                    ann.modusponens = true;
                    // hack to reverse MP's 
                    ann.right = class_to_string[it.first];
                    ann.left = class_to_string[it.second];
                    valid_classes.insert(cur_class);
                    break;
                }
            }
            if (!ann.modusponens) {
                return false;
            }
        }
    }

    if (root->is_op && root->op == IMPLICATION) {
        to_mp[root->r->my_class].push_back(make_pair(root->l->my_class, root->my_class));
    }

    return true;
}

void write_annotation(source& s, int num, int old_line) {
    cout << "[";
    cout << num << ". ";
    old_to_new[old_line] = num;
    if (s.hypo || s.axiom) {
        if (s.hypo) {
            cout << "Hypothesis ";
            cout << s.num_of;
        } else {
            cout << "Ax. sch. ";
            cout << s.num_of;
        }
    } else {
        assert(s.modusponens);
        cout << "M.P. " << old_to_new[s.left] << ", " << old_to_new[s.right];
    }
    cout << "] ";
}

void write_scheme(const parser& p) {
    p.print_result();
    cout << endl;
}

void create_frontline(const string& context, const parser& p) {
    cout << context;
    if (!context.empty()) {
        cout << " ";
    }
    cout << "|- ";
    p.print_result();
    cout << endl;
}

void store_hypotheses(const string& context) {
    string tmp;
    int hypo_num = 1;
    for (int i = 0; i < (int)context.size(); i++) {
        if (context[i] == ',') {
            parser p(tmp);
            // cout << tmp << " " << endl;
            // cout << p.get_result()->my_class;
            // cout << " " << hypo_num << endl;
            int top_class = p.get_result()->my_class;
            hypotheses[top_class] = hypo_num++;
            tmp = "";
        } else {
            if (!isspace(context[i])) {
                tmp += context[i];
            }
        }
    }
    if (!tmp.empty()) {
        parser p(tmp);
        // cout << tmp << " " << endl;
        // cout << p.get_result()->my_class;
        // cout << " " << hypo_num << endl;
        int top_class = p.get_result()->my_class;
        hypotheses[top_class] = hypo_num++;
    }
}

int main() {
    // freopen(".txt", "r", stdin);
    

    string context = parse_context();
    store_hypotheses(context);
   
    string tmp;
    getline(cin, tmp);
    parser statement = parser(tmp);

    vector<parser> parsers;
    int ab = 0;
    while (getline(cin, tmp)) {
        parsers.push_back(parser(tmp));
    }

    vector<int> sp;
    set<int> present;
    for (int i = 0; i < (int)parsers.size(); i++) {
        int curclass = parsers[i].get_result()->my_class;
        if (!present.count(curclass)) {
            present.insert(curclass);
            sp.push_back(i);
        }
    }

    int n = sp.size();
    vector<bool> valid(n);
    vector<source> annotations(n);
    int needed = statement.get_result()->my_class;
    if (parsers.back().get_result()->my_class != needed) {
        cout << "Proof is incorrect, proving wrong statement" << endl;
        return 0;
    } 

    int last = -1;
    int all_correct = -1;
    for (int i = 0; i < n; i++) {
        valid[i] = validate(parsers[sp[i]], annotations[i], i + 1);
        if (!valid[i]) {
            all_correct = i + 2;
            break;
        }
        if (last == -1 && parsers[sp[i]].get_result()->my_class == needed) {
            last = i;
        }
    }

    if (all_correct != -1) {
        parsers[all_correct - 2].print_result();
        cout << endl;
        cout << "statement " << all_correct  << " incorrect "<< endl;
        return 0;
    }

    if (last == -1) {
        cout << "Proof is incorrect, needed simply not found" << endl;
        return 0;
    }

    if (!valid[last]) {
        cout << "Last one is not valid" << endl;
        return 0;
    }

    vector<bool> required(n, false);
    required[last] = true;
    for (int i = last; i >= 0; i--) {
        if (!required[i]) {
            continue;
        }
        // validity check is redundant
        if (annotations[i].modusponens) {
            required[annotations[i].left - 1] = true;
            required[annotations[i].right - 1] = true;
        }
    }

    create_frontline(context, statement);
    int string_num = 1;
    for (int i = 0; i < n; i++) {
        if (required[i]) {
            write_annotation(annotations[i], string_num++, class_to_string[parsers[sp[i]].get_result()->my_class]);
            write_scheme(parsers[sp[i]]);
        }
    }
}
