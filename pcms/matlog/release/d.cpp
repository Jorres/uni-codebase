#include "types.h"
#include "helpers.h"

using namespace std;

bool middle_deduction = true;
int middle_deduction_iterations = 3;
bool should_combine = true;
int interested = 2;

int out_lines = 0;

bool evaluate_node(node* v, const vector<node*>& assumptions) {
    if (v->is_op) {
        bool left = evaluate_node(v->l, assumptions);
        bool right = v->op == '!' ? false : evaluate_node(v->r, assumptions);
        if (v->op == '!') {
            v->value = !left;
        } else if (v->op == IMPLICATION) {
            v->value = !left || right;
        } else if (v->op == '&') {
            v->value = left && right;
        } else if (v->op == '|') {
            v->value = left || right;
        } else {
            assert(false);
        }
        return v->value;
    } else {
        for (node* assumption : assumptions) {
            if (v->node_class == assumption->node_class) {
                v->value = true;
                return v->value;
            } else if (negat(v)->node_class == assumption->node_class) {
                v->value = false;
                return v->value;
            }
        }
        assert(false);
    }
}

class parser {
public:
    parser(const string& _s) {
        s = "(" + _s + ")";
        init_priorities();
        init_operations();
        success = false;
        orig = origin(false, nullptr);
        parse();
    }

    parser(node* v) {
        success = false;
        orig = origin(true);
        vars.push_back(v);
    }

    parser(node* v, node* from) {
        orig = origin(false, from);
        vars.push_back(v);
        success = false;
    }

    parser(node* v, bool ax) {
        assert(!ax);
        orig = origin(false, nullptr);
        vars.push_back(v);
        success = false;
    }

    parser chop_first_impl(parser& p) {
        return parser(p.top()->r, p.top()->l);
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


    vector<node*> convert_to_assumptions(const vector<int>& selection) {
        vector<node*> assumptions;
        int i = 0;
        for (int v : selection) {
            append_node(node(different_variables[i++]));
            last_node()->value = (v == SELECTED_TRUE || v == SELECTED_FALSE);

            if (v == SELECTED_TRUE || v == REMAINING_TRUE) {
                assumptions.push_back(last_node());
            } else if (v == SELECTED_FALSE || v == REMAINING_FALSE) {
                append_node(node('!', last_node()));
                last_node()->value = true;
                assumptions.push_back(last_node());
            }
        }
        return assumptions;
    }

    void prove(vector<int>& selection) {
        format_context(different_variables, selection, s);

        vector<vector<int>> prefixes = vector<vector<int>>(1, vector<int>());

        for (int v : selection) { 
            int psz = prefixes.size();
            if (v == SELECTED_TRUE || v == SELECTED_FALSE) {
                append_for_all(0, psz, prefixes, v);
            } else if (v == UNSELECTED) {
                double_up(prefixes);
                append_for_all(0, psz,  prefixes, REMAINING_TRUE);
                append_for_all(psz, 2 * psz,  prefixes, REMAINING_FALSE);
            } else {
                assert(false);
            }
        } 

        vector<node*> final_statements;
        parser p(s);
        int cur = 0;
        for (auto& data : prefixes) {
            // if (interested == cur || should_combine) {
                final_statements.push_back(p.generate_proof(data));
            // }
            // cur++;
        }

        if (should_combine) {
            combine_proofs(final_statements, selection);
        }
    }

    void combine_proofs(vector<node*>& finals, const vector<int>& selection) {
        for (int step = 0; step < (int)selection.size(); step++) {
            string left_var_name = different_variables[step];
            if (selection[step] == SELECTED_TRUE || selection[step] == SELECTED_FALSE) {
                for (auto& statement : finals) {
                    if (statement != nullptr) {
                        print_node(statement->l);
                        print_node(statement->r);
                        statement = statement->r;
                    }
                }
            } else {
                for (auto& v : finals) {
                    if (v != nullptr && !v->l->is_op) {
                        v = collapse_hardcoded_a_or_not_a(left_var_name, v);
                    } else {
                        v = nullptr;
                    }
                }
            }
        }
    }

    node* generate_proof(const vector<int>& selection) {
        vector<node*> assumptions = convert_to_assumptions(selection);

        evaluate_node(top(), assumptions);

        vector<parser> lines;
        for (node* v : assumptions) {
            lines.push_back(parser(v, false));
        }

        generate_recursively(top(), lines, assumptions);

        if (middle_deduction) {
            int asize = assumptions.size();
            for (int i = asize - 1; i >= 0; i--) {
                middle_deduction_iterations--;
                node* assumption = assumptions[i];
                assumptions.pop_back();
                lines = deduction_transfer(lines, assumption, assumptions, true);
            }
        }

        for (auto& l : lines) {
            l.print_result();
            out_lines++;
        }

        return lines.back().top();
    }

    void generate_a_implies_a(vector<parser>& parsers, node* var) {
        node* impl = make_impl(var, var);
        impl->value = true;

        parser tmp1(wrap_in_ax_1(var, var));
        parser tmp2(wrap_in_ax_2(var, impl, var));
        parser tmp3 = chop_first_impl(tmp2);
        parser tmp4(wrap_in_ax_1(var, impl)); 
        parser tmp5 = chop_first_impl(tmp3);

        parsers.push_back(tmp1);
        parsers.push_back(tmp2);
        parsers.push_back(tmp3);
        parsers.push_back(tmp4);
        parsers.push_back(tmp5);
    }

    bool check_if_ax(parser& p) {
        return p.orig.axiom;
    }

    void append_proof_single_line(parser& line, vector<parser>& new_parsers, 
            node* add_var, node* add_impl, const vector<node*>& assumptions) {
        node* right = line.top();

        if (equal_parts(add_impl)) { // A -> A
            generate_a_implies_a(new_parsers, right);
        } else if (check_if_ax(line) || has_as_a_hypo(line, assumptions)) { // A -> B
            if (check_if_ax(line)) {
                parser tmp1 = line; 
                new_parsers.push_back(tmp1);
            }
            parser tmp2 = parser(wrap_in_ax_1(right, add_var));
            parser tmp3 = chop_first_impl(tmp2);
            new_parsers.push_back(tmp2);
            new_parsers.push_back(tmp3);
        } else { // modus ponens
            origin& orig = line.orig;
            assert(orig.axiom == false);
            parser tmp1(wrap_in_ax_2(add_var, orig.leftmp, right));
            parser tmp2 = chop_first_impl(tmp1);
            parser tmp3 = chop_first_impl(tmp2);

            new_parsers.push_back(tmp1);
            new_parsers.push_back(tmp2);
            new_parsers.push_back(tmp3);
        }
    }

    vector<parser> deduction_transfer(vector<parser>& parsers, node* left, const vector<node*>& assumptions, bool should_write) {
        vector<parser> new_parsers;

        if (should_write) {
            for (node* hypo : assumptions) {
                new_parsers.push_back(parser(hypo));
            }
        }

        for (auto& p : parsers) {
            node* right = p.top();
            node* whole = make_impl(left, right); // WARNING
            // whole->value = true; non-needed
            append_proof_single_line(p, new_parsers, left, whole, assumptions);
        }
        return new_parsers;
    }

    bool equal_parts(const node* v) {
        return v->l->node_class == v->r->node_class;
    }

    bool has_as_a_hypo(const parser& p, const vector<node*>& assumptions) {
        node* v = p.top();
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
                        parsers.push_back(parser(v));
                        return;
                    }
                }
                assert(false);
                // cerr << "unexpected" << endl;
                // cerr << res << endl;
                // cerr << name << endl;
                // for (node* hypo : selection) {
                //     cerr << partial_string(hypo) << endl;
                // }
                // exit(0);
            } else {
                for (node* hypo : selection) {
                    if (hypo->is_op && hypo->op == '!' && v->node_class == hypo->l->node_class) {
                        parsers.push_back(parser(negat(v)));
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
        node* left;
        node* or_part = make_or(v->l, v->r);
        or_part->value = true; // WARNING
        if (v->l->value) {
            generate_recursively(v->l, parsers, selection);            
            left = v->l;
        } else if (v->r->value) {
            generate_recursively(v->r, parsers, selection);            
            left = v->r;
        } else {
            assert(false);
        }

        parser p0(wrap_in_ax_or(left, or_part));
        parser p1 = chop_first_impl(p0);
        parsers.push_back(p0);
        parsers.push_back(p1);
    }

    void generate_and_true(node* v, vector<parser>& parsers, const vector<node*>& selection) {
        assert(v->l->value);
        assert(v->r->value);
        generate_recursively(v->l, parsers, selection);
        generate_recursively(v->r, parsers, selection);

        parser p0(wrap_in_ax_and(v->l, v->r));
        parser p1 = chop_first_impl(p0);
        parser p2 = chop_first_impl(p1);
        parsers.push_back(p0);
        parsers.push_back(p1);
        parsers.push_back(p2);
    }

    void generate_impl_true(node* v, vector<parser>& parsers, const vector<node*>& selection) { // UNCHECKED
        assert((v->r->value) || (!v->l->value));
        if (v->r->value) {
            generate_recursively(v->r, parsers, selection);
            parser p0 = parser(wrap_in_ax_1(v->r, v->l));
            parser p1 = chop_first_impl(p0);
            parsers.push_back(p0);
            parsers.push_back(p1);
        } else { 
            generate_recursively(v->l, parsers, selection); 
            // A -> B
            node* a = v->l;
            node* b = v->r;
            node* nota = negat(a);
            nota->value = !a->value;
            node* notb = negat(b);

            vector<parser> no_deducted;


            parser p0(nota, false);
            parser p1(a, false);
            parser p2(wrap_in_ax_1(nota, notb)); 
            parser p3 = chop_first_impl(p2);
            parser p4(wrap_in_ax_1(a, notb));
            parser p5 = chop_first_impl(p4);
            parser p6(wrap_in_ax_9(notb, a));
            parser p7 = chop_first_impl(p6);
            parser p8 = chop_first_impl(p7);
            parser p9(wrap_in_ax_10(b));
            parser p10 = chop_first_impl(p9);


            no_deducted.push_back(p0);
            no_deducted.push_back(p1);
            no_deducted.push_back(p2); no_deducted.push_back(p3); no_deducted.push_back(p4); 
            no_deducted.push_back(p5); no_deducted.push_back(p6); no_deducted.push_back(p7); 
            no_deducted.push_back(p8); no_deducted.push_back(p9); no_deducted.push_back(p10);


            vector<node*> m_selection = selection;
            m_selection.push_back(nota);

            vector<parser> deducted = deduction_transfer(no_deducted, a, m_selection, false);

            for (auto& p : deducted) {
                parsers.push_back(p);
            }
        }
    }
    
    void generate_no_true(node* v, vector<parser>& parsers, const vector<node*>& selection) {
        generate_recursively(v->l, parsers, selection);
    }

    void generate_or_false(node* v, vector<parser>& parsers, const vector<node*>& selection) {
        assert(v->l->value == 0 && v->r->value == 0);

        generate_recursively(v->l, parsers, selection);
        node* nota = parsers.back().top();
        generate_recursively(v->r, parsers, selection);
        node* notb = parsers.back().top();

        node* a = nota->l;
        node* b = notb->l;

        parser p0 = parser(wrap_in_ax_1(nota, v));
        parser p1 = chop_first_impl(p0);

        node* impl = make_impl(b, a);
        // impl->value = true;
        evaluate_node(impl, selection);
        generate_recursively(impl, parsers, selection);

        generate_a_implies_a(parsers, a);

        parser p2 = parser(wrap_in_ax_8(a, b, a));
        parser p3 = chop_first_impl(p2);
        parser p4 = chop_first_impl(p3);
        parser p5 = parser(wrap_in_ax_9(v, a));
        parser p6 = chop_first_impl(p5);
        parser p7 = chop_first_impl(p6);

        parsers.push_back(p0);
        parsers.push_back(p1);
        parsers.push_back(p2);
        parsers.push_back(p3);
        parsers.push_back(p4);
        parsers.push_back(p5);
        parsers.push_back(p6);
        parsers.push_back(p7);
    }

    void generate_impl_false(node* v, vector<parser>& parsers, const vector<node*>& selection) { // UNCHECKED
        // cout << v->value << endl;
        // print_node(v->l);
        // print_node(v);
        // for (node* sel : selection) {
        //     print_node(sel);
        // }
        // exit(0);
        assert(v->l->value);
        assert(!v->r->value);
        
        generate_recursively(v->l, parsers, selection);
        node* a = parsers.back().top();
        generate_recursively(v->r, parsers, selection);
        node* notb = parsers.back().top();

        node* b = notb->l;
        node* aib = make_impl(a, b);
        aib->value = true; // WARNING

        parser p0(wrap_in_ax_1(notb, aib));
        parser p1 = chop_first_impl(p0);
        parser p2(wrap_in_ax_2(aib, a, b)); 
        parser p3(wrap_in_ax_1(a, aib));
        parser p4 = chop_first_impl(p3);
        parser p5 = chop_first_impl(p2);
        
        parsers.push_back(p0); parsers.push_back(p1); parsers.push_back(p2);
        parsers.push_back(p3); parsers.push_back(p4); parsers.push_back(p5);

        generate_a_implies_a(parsers, aib); // (a -> b) -> (a -> b)

        parser p6 = chop_first_impl(p5); // (a -> b) -> b
        parser p7(wrap_in_ax_9(aib, b)); // ((a -> b) -> b) -> ((a -> b) -> !b) -> !(a -> b)
        parser p8 = chop_first_impl(p7); // ((a -> b) -> !b) -> !(a -> b) 
        parser p9 = chop_first_impl(p8); // !(a -> b) 
        parsers.push_back(p6);
        parsers.push_back(p7);
        parsers.push_back(p8);
        parsers.push_back(p9);
    }

    void generate_and_false(node* v, vector<parser>& parsers, const vector<node*>& selection) {
        assert(!v->l->value || !v->r->value);

        if (!v->l->value) {
            generate_recursively(v->l, parsers, selection);
        } else {
            generate_recursively(v->r, parsers, selection);
        }

        node* left = parsers.back().top();
        node* true_left = left->l;

        parser p0(wrap_in_ax_1(left, v));
        parser p1 = chop_first_impl(p0);
        node* impl = make_impl(v, true_left);
        impl->value = true; // WARNING
        parser p2(impl); // possibly ?..
        parser p3(wrap_in_ax_9(v, true_left));
        parser p4 = chop_first_impl(p3);
        parser p5 = chop_first_impl(p4);
        parsers.push_back(p0);
        parsers.push_back(p1);
        parsers.push_back(p2);
        parsers.push_back(p3);
        parsers.push_back(p4);
        parsers.push_back(p5);
    }                       

    void generate_no_false(node* nota, vector<parser>& parsers, const vector<node*>& selection) {
        assert(!nota->value);
        assert(nota->l->value);
        node* a = nota->l;

        generate_a_implies_a(parsers, nota); // !A -> !A

        evaluate_node(a, selection);
        generate_recursively(a, parsers, selection); // A

        parser p0 = parser(wrap_in_ax_1(a, nota)); 
        parser p1 = chop_first_impl(p0);
        parser p2 = parser(wrap_in_ax_9(nota, a)); 
        parser p3 = chop_first_impl(p2);
        parser p4 = chop_first_impl(p3);

        parsers.push_back(p0);
        parsers.push_back(p1);
        parsers.push_back(p2);
        parsers.push_back(p3);
        parsers.push_back(p4);
    }

    node* collapse_hardcoded_a_or_not_a(const string& var_name, const node* s) {
        append_node(node(var_name));
        node* cur = last_node();

        generate_a_or_not_a(cur);

        node* v = wrap_in_ax_8(cur, negat(cur), s->r);
        print_node(v);
        v = v->r;
        print_node(v);
        v = v->r;
        print_node(v);
        v = v->r;
        print_node(v);
        return s->r;
    }

    bool check_if_true(vector<int>& selection) {
        vector<node*> assumptions = convert_to_assumptions(selection);
        return evaluate_node(vars.back(), assumptions);
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
                    append_node(node(var));
                    vars.push_back(last_node());
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

    void print_result() const {
        string tmp = "";
        traverse(vars.back(), tmp);
        cout << tmp << endl;
    }

    vector<string> variables_involved() {
        return different_variables;
    }

    node* top() const {
        return vars.back();
    }

private:
    bool left_assoc(char c) {
        return c != IMPLICATION && c != '!';
    }

    void process_op(char c) {
        if (is_unary(c)) {
            node* last = vars.back(); vars.pop_back();
            append_node(node(c, last));
        } else {
            node* r = vars.back(); vars.pop_back();
            node* l = vars.back(); vars.pop_back();
            append_node(node(c, l, r));
        }
        vars.push_back(last_node());
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
    origin orig;
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

        bool holds = true;
        for (int j = 0; j < (1 << total_vars); j++) { // select values for remaining
            vector<int> spoilable_values = combined_values;
            for (int k = 0; k < (int)combined_values.size(); k++) {
                if (combined_values[k] == UNSELECTED) {
                    spoilable_values[k] = ((j & (1 << k)) > 0) ? REMAINING_TRUE : REMAINING_FALSE;
                }
            }
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

void test() {
    parser a("A");
    parser b("!B");
    vector<node*> assumptions;
    assumptions.push_back(a.top());
    assumptions.push_back(b.top());
    parser p("A & B");
    vector<parser> doc;
    evaluate_node(p.top(), assumptions);
    p.generate_and_false(p.top(), doc, assumptions);
    for (int i = 0; i < (int)doc.size(); i++) {
        doc[i].print_result();
    }
    exit(0);
}

int main() {
    ios::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    
    nodes.reserve(100000);

    string statement;
    getline(cin, statement);
    parser p = parser(statement);

    pair<vector<int>, bool> required_selection = try_select(p, true);
    if (!required_selection.second) {
        string negatd = "!( " + statement + ")";
        p = parser(negatd);
        required_selection = try_select(p, false);
    }

    if (!required_selection.second) {
        cout << ":(" << endl;
    } else {
        p.prove(required_selection.first);
    }
}
