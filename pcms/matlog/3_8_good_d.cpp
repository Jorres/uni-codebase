#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <cassert>
#include <map>
#include <set>

#include "types.h"

using namespace std;
#define endl '\n'
const char IMPLICATION = '#';

statements_storage database;

bool implicate(bool a, bool b) {
    bool ans = !a || b;
    return ans;
}

bool check_op(node* v, char c) {
    return v->is_op && v->op == c;
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
        return;
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

string wrap_in_ax_1(const string& a, const string& b) {
    return "(" + a + ") -> (" + b + ") -> (" + a + ")";
}

string wrap_in_ax_2(const string& a, const string& b, const string& c) {
    return "((" + a + ") -> (" + b + ")) -> ((" + a + ") -> (" + b + ") -> (" + c + ")) -> ((" + a + ") -> ( " + c + " ))";
}

string wrap_in_ax_or(const string& a, const string& b) {
    return "(" + a + ") -> " + b;
}

string wrap_in_ax_and(const string& a, const string& b) {
    return "(" + a + ") -> (" + b + " )  -> (" + a + " & " + b + ")";
}

string wrap_in_ax_8(const string& a, const string& b, const string& c) {
    return "((" + a + ") -> (" + c + ")) -> ((" + b + ") -> (" + c + ")) -> ((" + a + ") | (" + b + ") -> (" + c + "))";
}

string wrap_in_ax_9(const string& a, const string& b) {
    return "((" + a + ") -> (" + b + ")) -> ((" + a +") -> (!(" + b + "))) -> (!(" + a + "))";
}

string wrap_in_ax_10(const string& a) {
    return "(!(!(" + a + "))) -> (" + a + ")";
}

vector<node> nodes;

bool evaluate_node(node* v, vector<string>& var_names, vector<bool>& values) {
    if (v->is_op) {
        if (v->op == '!') {
            return v->value = !evaluate_node(v->l, var_names, values);
        } else if (v->op == IMPLICATION) {
            return v->value = implicate(evaluate_node(v->l, var_names, values), evaluate_node(v->r, var_names, values));
        } else if (v->op == '&') {
            return v->value = evaluate_node(v->l, var_names, values) && evaluate_node(v->r, var_names, values);
        } else if (v->op == '|') {
            return v->value = evaluate_node(v->l, var_names, values) || evaluate_node(v->r, var_names, values);
        }
        assert(false);
        return false;
    } else {
        for (int i = 0; i < (int)values.size(); i++) {
            if (v->var_name_if_present == var_names[i]) {
                return v->value = values[i];
            }
        }
        assert(false);
        return false;
    }
}

class parser {
public:
    parser(const string& _s) {
        s = "(" + _s + ")";
        init_priorities();
        init_operations();
        success = false;
        modusponens = make_pair(-1, -1);
        parse();
    }

    parser(const string& _s, pair<int, int> mp) {
        s = "(" + _s + ")";
        init_priorities();
        init_operations();
        success = false;
        modusponens = mp;
        parse();
    }

    void append_for_all(int l, int r, vector<vector<int>>& v, int a) {
        for (int i = l; i < r; i++) {
            v[i].push_back(a);
        }
    }

    void double_up(vector<vector<int>>& v) {
        int start_size = v.size();
        for (int i = 0; i < start_size; i++) {
            v.push_back(v[i]);
        }
    }

    void format_context(vector<int>& selection) {
        bool first = true;
        for (int i = 0; i < (int)selection.size(); i++) {
            if (selection[i] == SELECTED_TRUE) {
                if (!first) {
                    cout << ", ";
                }

                first = false;
                cout << different_variables[i];
            }
            if (selection[i] == SELECTED_FALSE){
                if (!first) {
                    cout << ", ";
                }

                first = false;
                cout << "!" + different_variables[i];
            }
        }
        cout << " |- " << s << endl;
    }

    vector<node*> convert_to_assumptions(const vector<int>& selection) {
        vector<node*> _assumptions;
        for (int i = 0; i < (int)selection.size(); i++) {
            int v = selection[i];

            nodes.push_back(node(different_variables[i]));
            nodes.back().value = (v == SELECTED_TRUE || v == REMAINING_TRUE);
            node* cur = &(nodes.back());
            cur->node_class = database.detect(cur);

            if (v == SELECTED_TRUE || v == REMAINING_TRUE) {
                _assumptions.push_back(cur);
            } else if (v == SELECTED_FALSE || v == REMAINING_FALSE) {
                nodes.push_back(node('!', cur));
                nodes.back().value = true;
                node* cur2 = &(nodes.back());
                cur2->node_class = database.detect(cur2);
                _assumptions.push_back(cur2);
            } else {
                assert(false);
            }
        }
        return _assumptions;
    }

    void prove(vector<int>& selection) {
        format_context(selection);

        vector<vector<int>> prefixes = vector<vector<int>>(1, vector<int>());

        for (int i = 0; i < (int)selection.size(); i++) { 
            int psz = prefixes.size();
            if (selection[i] == SELECTED_TRUE || selection[i] == SELECTED_FALSE) {
                append_for_all(0, psz, prefixes,  selection[i]);
            } else if (selection[i] == UNSELECTED) {
                double_up(prefixes);
                append_for_all(0, psz,  prefixes, REMAINING_TRUE);
                append_for_all(psz, 2 * psz,  prefixes, REMAINING_FALSE);
            } else {
                assert(false);
            }
        }

        vector<parser> parsers;
        vector<string> final_statements;
        for (auto data : prefixes) {
            parsers.push_back(parser(s));
            final_statements.push_back(parsers.back().generate_proof(data));
        }
        combine_proofs(final_statements, selection);
    }

    void combine_proofs(vector<string>& prefixes, const vector<int>& selection) {
        for (int step = (int)selection.size() - 1; step >= 0; step--) { // order
            string left_var_name = different_variables[step];
            if (selection[step] == SELECTED_TRUE || selection[step] == SELECTED_FALSE) {
                for (auto& prefix : prefixes) {
                    parser p = parser(prefix);
                    cout << partial_string(p.vars.back()->l) << endl;
                    prefix = partial_string(p.vars.back()->r);
                    cout << prefix << endl;
                }
            } else {
                int pref_size = prefixes.size();
                for (int i = 0; i < pref_size / 2; i++) {
                    prefixes[i] = collapse_hardcoded_a_or_not_a(left_var_name, prefixes[i]);
                    prefixes.pop_back();
                }
            }
        }
        assert(prefixes.size() == 1);
    }

    vector<bool> drop_classes(const vector<int>& v) {
        vector<bool> ans;
        for (auto val : v) {
            ans.push_back(val == SELECTED_TRUE || val == REMAINING_TRUE);
        }
        return ans;
    }

    string generate_proof(const vector<int>& selection) {
        vector<bool> var_values = drop_classes(selection);
        vector<node*> assumptions = convert_to_assumptions(selection);

        evaluate_node(vars.back(), different_variables, var_values);

        vector<parser> parsers;

        generate_recursively(vars.back(), parsers, assumptions);

        for (int cv = 0; cv < (int)different_variables.size(); cv++) {
            parsers = deduction_transfer(parsers, different_variables[cv], cv, selection);
        }

        for (auto& p : parsers) {
            p.print_result();
        }

        return partial_string(parsers.back().vars.back());
    }

    void generate_a_implies_a(vector<parser>& parsers, const string& var) {
        string impl = "(" + var +  " -> " + var + ")";
        int cur_sz = parsers.size();
        parser tmp1 = parser(wrap_in_ax_1(var, var));
        parser tmp2 = parser(wrap_in_ax_2(var, impl, var));
        parser tmp3 = parser(partial_string(tmp2.vars.back()->r), make_pair(cur_sz, cur_sz + 1));
        parser tmp4 = parser(wrap_in_ax_1(var, impl)); 
        parser tmp5 = parser(impl, make_pair(cur_sz + 3, cur_sz + 2));
        parsers.push_back(tmp1);
        parsers.push_back(tmp2);
        parsers.push_back(tmp3);
        parsers.push_back(tmp4);
        parsers.push_back(tmp5);
    }

    void append_proof_single_line(const vector<parser>& parsers, parser& line, vector<parser>& new_parsers, 
            const string& add_var, const string& add_impl,
            const vector<node*>& assumptions, const vector<int>& mapper) {

        string right = partial_string(line.vars.back());

        if (equal_parts(parser(add_impl))) { // (!A) -> (!A)
            generate_a_implies_a(new_parsers, partial_string(parser(add_impl).vars.back()->l));
        } else if (check_if_ax(line) || has_as_a_hypo(line, assumptions)) {
            parser tmp1 = line; // right
            parser tmp2 = parser(wrap_in_ax_1(right, add_var)); // (right) -> A -> (right)
            new_parsers.push_back(tmp1);
            new_parsers.push_back(tmp2);
            int new_sz = new_parsers.size();
            new_parsers.push_back(parser(add_impl, make_pair(new_sz - 2, new_sz - 1)));
        } else { // modus ponens
            pair<int, int> mp = line.modusponens;
            assert(mp != make_pair(-1, -1));
            int small = mp.first;
            int big = mp.second;

            int cur_sz = new_parsers.size();

            parser tmp1 = parser(wrap_in_ax_2(add_var, partial_string(parsers[small].vars.back()), right));

            parser tmp2 = parser(partial_string(tmp1.vars.back()->r), make_pair(mapper[small], cur_sz));

            parser tmp3 = parser(partial_string(tmp2.vars.back()->r), make_pair(mapper[big], cur_sz + 1));

            new_parsers.push_back(tmp1);
            new_parsers.push_back(tmp2);
            new_parsers.push_back(tmp3);
        }
    }

    vector<parser> deduction_transfer(vector<parser>& parsers, const string& varname, int h_num, const vector<int>& selection) {
        vector<parser> new_parsers;
        vector<int> mapper_from_old_to_new;

        vector<bool> current_selection_bool = drop_classes(selection);
        vector<node*> assumptions = convert_to_assumptions(selection);

        for (int i = 0; i < (int)parsers.size(); i++) {

            bool selection_state = current_selection_bool[h_num];


            string right = partial_string(parsers[i].vars.back());
            string _tmp = varname;
            string add_var = selection_state ? "(" + _tmp + ")" :  "(!(" + _tmp + "))";
            string add_impl = add_var + " -> " + "(" + right + ")";

            append_proof_single_line(parsers, parsers[i], new_parsers, add_var, add_impl, assumptions, mapper_from_old_to_new);

            mapper_from_old_to_new.push_back(new_parsers.size() - 1);
        }
        return new_parsers;
    }

    bool equal_parts(const parser& p) {
        node* v = p.vars.back();
        assert(check_op(v, IMPLICATION));
        return v->l->node_class == v->r->node_class;
    }

    bool has_as_a_hypo(const parser& p, const vector<node*>& assumptions) {
        node* v = p.vars.back();
        for (node* hypo : assumptions) {
            if (hypo->node_class == v->node_class) {
                return true;
            }
        }
        return false;
    }

    void generate_recursively(node* v, vector<parser>& parsers, const vector<node*>& selection) {
        bool res = v->value;
        if (!v->is_op) {
            string name = v->var_name_if_present;
            if (res) {
                for (node* hypo : selection) {
                    if (v->node_class == hypo->node_class) {
                        parsers.push_back(parser(name));
                        return;
                    }
                }
                assert(false);
            } else {
                for (node* hypo : selection) {
                    if (check_op(hypo, '!') && v->node_class == hypo->l->node_class) {
                        parsers.push_back(parser("(!(" + name + "))"));
                        return;
                    }
                }
                assert(false);
            }
        } else {
            if (v->op == '|') {
                if (res) {
                    generate_or_true(v, parsers, selection);
                } else {
                    generate_or_false(v, parsers, selection);
                }
            } else if (v->op == '&') {
                if (res) {           
                    generate_and_true(v, parsers, selection);
                } else {
                    generate_and_false(v, parsers, selection);
                }
            } else if (v->op == IMPLICATION) {
                if (res) {            
                    generate_impl_true(v, parsers, selection);
                } else {
                    generate_impl_false(v, parsers, selection);
                }
            } else if (v->op == '!') {
                if (res) {             
                    generate_no_true(v, parsers, selection);
                } else {
                    generate_no_false(v, parsers, selection);
                }
            }                                                                           
        }
    }

    void generate_or_true(node* v, vector<parser>& parsers, const vector<node*>& selection) {
        string left;
        string or_part = "(" + partial_string(v->l) + " | "  + partial_string(v->r) + ")";
        if (v->l->value) {
            generate_recursively(v->l, parsers, selection);            
            left = partial_string(v->l);
        } else if (v->r->value) {
            generate_recursively(v->r, parsers, selection);            
            left = partial_string(v->r);
        } else {
            assert(false);
        }

        parsers.push_back(parser(wrap_in_ax_or(left, or_part)));
        int cur_size = parsers.size();
        auto mp = make_pair(cur_size - 2, cur_size - 1);
        parsers.push_back(parser(partial_string(v), mp));
    }

    void generate_and_true(node* v, vector<parser>& parsers, const vector<node*>& selection) {
        generate_recursively(v->l, parsers, selection);
        string l = partial_string(v->l);
        int l_num = parsers.size() - 1;
        generate_recursively(v->r, parsers, selection);
        string r = partial_string(v->r);
        int r_num = parsers.size() - 1;

        parser p = parser(wrap_in_ax_and(l, r));
        parsers.push_back(p);
        parser s = parser(partial_string(p.vars.back()->r), make_pair(l_num, parsers.size() - 1));
        parsers.push_back(s);
        parser t = parser(partial_string(s.vars.back()->r), make_pair(r_num, parsers.size() - 1));
        parsers.push_back(t);
    }

    void generate_impl_true(node* v, vector<parser>& parsers, const vector<node*>& selection) {
        assert((v->r->value) || (!v->l->value));
        if (v->r->value) {
            string a = partial_string(v->l);
            string b = partial_string(v->r);
            generate_recursively(v->r, parsers, selection);
            int sz = parsers.size();
            parser p0 = parser(wrap_in_ax_1(b, a));
            parser p1 = parser("(" + a + " -> " + b + ")", make_pair(sz - 1, sz));
        } else {
            // ???
        }
    }

    void generate_no_true(node* v, vector<parser>& parsers, const vector<node*>& selection) {

    }

    void generate_or_false(node* v, vector<parser>& parsers, const vector<node*>& selection) {

    }

    void generate_impl_false(node* v, vector<parser>& parsers, const vector<node*>& selection) { // UNCHECKED
        assert(v->l->value);
        assert(!v->r->value);
        
        generate_recursively(v->l, parsers, selection);
        int l_num = parsers.size();
        string a = partial_string(parsers.back().vars.back());
        generate_recursively(v->r, parsers, selection);
        int r_num = parsers.size();
        string notb = partial_string(parsers.back().vars.back());
        int sz = parsers.size();
        string b = partial_string(parser(notb).vars.back()->l);
        string aib = "(" + a + " -> " + b + ")";

        parser p0 = parser(wrap_in_ax_1(notb, aib)); // !b -> (a -> b) -> !b
        parser p1 = parser(partial_string(p0.vars.back()->r), make_pair(r_num, sz + 0)); // (a -> b) -> !b
        parser p2 = parser(wrap_in_ax_8(aib, a, b)); // ((a -> b) -> a) -> ((a -> b) -> a -> b) -> ((a -> b) -> b)
        parser p3 = parser(wrap_in_ax_1(a, aib)); // a -> (a -> b) -> a
        parser p4 = parser(partial_string(p3.vars.back()->r), make_pair(l_num, sz + 3)); // (a -> b) -> a
        parser p5 = parser(partial_string(p2.vars.back()->r), make_pair(sz + 4, sz + 2)); // ((a -> b) -> a -> b) -> ((a -> b) -> b) 
        
        parsers.push_back(p0); parsers.push_back(p1); parsers.push_back(p2); 
        parsers.push_back(p3); parsers.push_back(p4); parsers.push_back(p5);

        generate_a_implies_a(parsers, aib); // (a -> b) -> (a -> b)
        int sz2 = parsers.size();
        parser p6 = parser(partial_string(p5.vars.back()->r), make_pair(sz2 - 1, sz + 5)); // (a -> b) -> b
        parser p7 = parser(wrap_in_ax_9(aib, b)); // ((a -> b) -> b) -> ((a -> b) -> !b) -> !(a -> b)
        parser p8 = parser(partial_string(p7.vars.back()->r), make_pair(sz2, sz2 + 1)); // ((a -> b) -> !b) -> !(a -> b) 
        parser p9 = parser(partial_string(p8.vars.back()->r), make_pair(sz + 1, sz2 + 2)); // !(a -> b) 
        parsers.push_back(p6);
        parsers.push_back(p7);
        parsers.push_back(p8);
        parsers.push_back(p9);
    }

    void generate_and_false(node* v, vector<parser>& parsers, const vector<node*>& selection) {

    }

    void generate_no_false(node* v, vector<parser>& parsers, const vector<node*>& selection) {

    }

    string collapse_hardcoded_a_or_not_a(const string& var_name, const string& s) {
        generate_a_or_not_a(var_name);

        string s_chipped = partial_string(parser(s).vars.back()->r);

        parser p = parser(wrap_in_ax_8(var_name, "!(" + var_name + ")", s_chipped));
        p.print_result();
        p.downgrade_node();
        p.print_result();
        p.downgrade_node();
        p.print_result();
        p.downgrade_node();
        p.print_result();
        return s_chipped;
    }


    void generate_helper(const string& a, const string& fp) {
        string b =  fp + " | (!" + fp + ")";

        parser p1 = parser(wrap_in_ax_9(a, b));
        p1.print_result();
        cout << "(" << a << ") -> (" << b << ")" << endl;
        p1.downgrade_node();
        p1.print_result();
        string tmp1 = wrap_in_ax_1("!(" + b + ")", a);
        cout << tmp1 << endl;
        string tmp2 = "(" + a + " -> (!(" + b + "))) -> (!(" + a + "))";
        parser p2 = parser(wrap_in_ax_1(tmp2, "!(" + b + ")"));
        p2.print_result();
        p2.downgrade_node();
        p2.print_result();
        parser p3 = wrap_in_ax_2("!(" + b + ")", "( " + a + " -> (!( " + b + ")))", "!(" + a + ")");
        p3.print_result();
        p3.downgrade_node();
        p3.print_result();
        p3.downgrade_node();
        p3.print_result();
    }
    
    void generate_a_or_not_a(const string& a) {
        generate_helper(a, a);
        generate_helper("!(" + a + ")", a);
        string aornota =  a + " | (!" + a + ")";
        parser p = parser(wrap_in_ax_9("!(" + a + " | (!" + a + "))", "!(" + a + ")"));
        p.print_result();
        p.downgrade_node();
        p.print_result();
        p.downgrade_node();
        p.print_result();
        parser pp = wrap_in_ax_10(aornota);
        pp.print_result();
        pp.downgrade_node();
        pp.print_result();
    }

    void downgrade_node() {
        node* t = vars.back(); 
        vars.pop_back();
        vars.push_back(t->r);
    }

    bool check_if_true(vector<int>& values) {
        vector<bool> class_dropped;
        for (auto val : values) {
            class_dropped.push_back(val == SELECTED_TRUE || val == REMAINING_TRUE);
        }
        return evaluate_node(vars.back(), different_variables, class_dropped);
   }

    void parse() {
        set<string> different_variables_set;

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
                    different_variables_set.insert(var);
                    nodes.push_back(node(var));
                    vars.push_back(&(nodes.back()));
                    vars.back()->node_class = database.detect(vars.back());
                } else {
                    assert(false);
                }
            }
        }
        assert(vars.size() == 1);
        assert(ops.size() == 0);
        success = true;
        for (auto s : different_variables_set) {
            different_variables.push_back(s);
        }
    }

    bool check_ax_1(parser& p) {
        node* v = p.get_result();
        if (!check_op(v, IMPLICATION) || !check_op(v->r, IMPLICATION)) {
            return false;
        }
        int left = v->l->node_class;
        int right = v->r->r->node_class;
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
        int one = L->l->node_class;
        int two = L->r->node_class;
        int three = RL->l->node_class;
        int four = F->l->node_class;
        int five = F->r->node_class;
        int six = RR->l->node_class;
        int seven = RR->r->node_class;
        return one == three && one == six && two == four && five == seven;
    }
    bool check_ax_4(parser& p) {
        node* v = p.get_result();
        if (!check_op(v, IMPLICATION) || !check_op(v->l, '&')) {
            return false;
        }

        return v->l->l->node_class == v->r->node_class;
    }
    bool check_ax_5(parser& p) {
        node* v = p.get_result();
        if (!check_op(v, IMPLICATION) || !check_op(v->l, '&')) {
            return false;
        }

        return v->l->r->node_class == v->r->node_class;
    }
    bool check_ax_3(parser& p) {
        node* v = p.get_result();
        if (!check_op(v, IMPLICATION)) {
            return false;
        }
        node* s = v->r;
        int one = v->l->node_class;
        if (!check_op(s, IMPLICATION)) {
            return false;
        }
        int two = s->l->node_class;
        node* t = s->r;
        if (!check_op(t, '&')) {
            return false;
        }
        int three = t->l->node_class;
        int four = t->r->node_class;
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
        int left = v->l->node_class;
        int right = s->l->node_class;
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
        int left = v->l->node_class;
        int right = s->r->node_class;
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
        int one = L->l->node_class;
        int two = L->r->node_class;
        int three = RL->l->node_class;
        int four = RL->r->node_class;
        int five = F->l->node_class;
        int six = F->r->node_class;
        int seven = RR->r->node_class;
        return one == five && two == four && two == seven && three == six;
    }
    bool check_ax_9(parser& p) {
        node* v = p.get_result();
        if (v->is_op && v->op == IMPLICATION) {
            node* s = v->l;
            if (!check_op(s, IMPLICATION)) {
                return false;
            }
            int one = s->l->node_class;
            int two = s->r->node_class;
            node* F = v->r;
            if (!check_op(F, IMPLICATION)) {
                return false;
            }
            node* L = F->l;
            node* R = F->r;
            if (!check_op(L, IMPLICATION)) {
                return false;
            }
            int three = L->l->node_class;
            if (!check_op(L->r, '!')) {
                return false;
            }
            int four = L->r->l->node_class;
            if (!check_op(R, '!')) {
                return false;
            }
            int five = R->l->node_class;
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
        int right = v->r->node_class;
        if (!check_op(s, '!')) {
            return false;
        }
        node* t = s->l;
        if (!check_op(t, '!')) {
            return false;
        }
        int left = t->l->node_class;
        return left == right;
    }


    bool check_if_ax(parser& p) {
        int num_of = -1;
        if (check_ax_1(p))  {
            num_of = 1;
        } else if (check_ax_2(p))  {
            num_of = 2;
        } else if (check_ax_3(p))  {
            num_of = 3;
        } else if (check_ax_4(p))  {
            num_of = 4;
        } else if (check_ax_5(p))  {
            num_of = 5;
        } else if (check_ax_6(p))  {
            num_of = 6;
        } else if (check_ax_7(p))  {
            num_of = 7;
        } else if (check_ax_8(p))  {
            num_of = 8;
        } else if (check_ax_9(p))  {
            num_of = 9;
        } else if (check_ax_10(p))  {
            num_of = 10;
        }
        return num_of != -1;
    }

    node* get_result() {
        if (!success) {
            return nullptr;
        }
        return vars.back();
    }

    void print_result() const {
        string tmp = "";
        traverse(vars.back(), tmp);
        cout << tmp << endl;
    }

    vector<string> variables_involved() {
        return different_variables;
    }
private:
    bool left_assoc(char c) {
        return c != IMPLICATION && c != '!';
    }

    void process_op(char c) {
        node* cur;
        if (is_unary(c)) {
            node* last = vars.back(); vars.pop_back();
            nodes.push_back(node(c, last));
            cur = &(nodes.back());
            vars.push_back(cur);
        } else {
            node* r = vars.back(); vars.pop_back();
            node* l = vars.back(); vars.pop_back();
            nodes.push_back(node(c, l, r));
            cur = &(nodes.back());
            vars.push_back(cur);
        }
        cur->node_class = database.detect(cur);
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

    vector<string> different_variables;
    pair<int, int> modusponens;
};

pair<vector<int>, bool> try_select(parser& p, bool selection_value) {
    vector<string> variables = p.variables_involved();

    int total_vars = variables.size();

    int minimal_size = total_vars + 1;
    vector<int> best_selection;

    bool found = false;
    for (int i = 0; i < (1 << total_vars); i++) {
        vector<int> selected_as_ones;
        vector<int> combined_values(total_vars, UNSELECTED);

        for (int j = 0; j < total_vars; j++) { // select which ones to set to one
            int res = (i & (1 << j));
            if (res > 0) {
                selected_as_ones.push_back(j);
            }
            combined_values[j] = (res > 0) ? (selection_value ? SELECTED_TRUE : SELECTED_FALSE) : UNSELECTED;
        }

        // cout << "Attempt:" << endl;
        // for (auto selected : selected_as_ones) {
        //     cout << selected << " ";
        // }
        // cout << endl;

        bool holds = true;
        for (int j = 0; j < (1 << total_vars); j++) { // select values for remaining
            vector<int> spoilable_values = combined_values;
            for (int k = 0; k < (int)combined_values.size(); k++) {
                if (combined_values[k] == UNSELECTED) {
                    spoilable_values[k] = (j & (1 << k)) > 0 ? REMAINING_TRUE : REMAINING_FALSE;
                }
            }
            // for (auto val : spoilable_values) {
            //     cout << selection_to_string(val) << " ";
            // }
            // cout << endl;
            bool holds_for_this_set = p.check_if_true(spoilable_values);
            if (!holds_for_this_set) {
                holds = false; 
                break;
            }
        }
        if (holds) {
            if (minimal_size > (int)selected_as_ones.size()) {
                minimal_size = selected_as_ones.size();
                best_selection = combined_values;
                found = true;
            }
        }
    }
    return make_pair(best_selection, found);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);
    freopen("input.txt", "r", stdin);

    nodes.reserve(100000);

    string statement;
    getline(cin, statement);
    parser p = parser(statement);

    pair<vector<int>, bool> required_selection = try_select(p, true);
    if (!required_selection.second) {
        string negated = "!( " + statement + ")";
        p = parser(negated);
        required_selection = try_select(p, false);
    }

    if (!required_selection.second) {
        cout << ":(" << endl;
        return 0;
    }

    p.prove(required_selection.first);
}

// g++ -O3 -o a d.cpp types.h
