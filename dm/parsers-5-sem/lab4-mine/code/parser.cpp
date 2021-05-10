#include <cassert>
#include <fstream>
#include <map>
#include <sstream>

#include "parse_exception.h"
#include "parser.h"

vector<t_rule> t_parser::make_grammar() {
  vector<t_rule> rules;

  rules.push_back(t_rule(S, rruleside{
                                F,
                                N,
                                LPAREN,
                                A,
                                RPAREN,
                                P,
                            }));
  rules.push_back(t_rule(A, rruleside{
                                EPS,
                            }));
  rules.push_back(t_rule(A, rruleside{
                                O,
                                T,
                            }));
  rules.push_back(t_rule(T, rruleside{
                                EPS,
                            }));
  rules.push_back(t_rule(T, rruleside{
                                COMMA,
                                O,
                                T,
                            }));
  rules.push_back(t_rule(O, rruleside{
                                N,
                                COLON,
                                N,
                            }));
  rules.push_back(t_rule(P, rruleside{
                                EPS,
                            }));
  rules.push_back(t_rule(P, rruleside{
                                COLON,
                                N,
                            }));
  rules.push_back(t_rule(F, rruleside{
                                FUN,
                            }));
  rules.push_back(t_rule(N, rruleside{
                                NAME,
                            }));

  return rules;
}

bool is_token(const variant<t_nterm, t_token> &rule_part) {
  return holds_alternative<t_token>(rule_part);
}

set<t_token> t_parser::calc_simple_FIRST(first_res &fst, const rruleside &to,
                                         int from = 0) {
  if (static_cast<size_t>(from) == to.size()) {
    return {EPS};
  }

  if (is_token(to[from])) {
    auto token = get<t_token>(to[from]);
    return {token};
  } else {
    auto nterm = get<t_nterm>(to[from]);
    set<t_token> ans = fst.at(nterm);
    auto it = ans.find(EPS);
    if (it != ans.end()) {
      ans.erase(it);
      set<t_token> other = calc_simple_FIRST(fst, to, from + 1);
      ans.insert(other.begin(), other.end());
    }
    return ans;
  }
}

first_res t_parser::calc_FIRST() {
  first_res ans;
  for (const auto e : t_parser::all_nterms) {
    ans.insert(make_pair(e, set<t_token>()));
  }
  bool changes = true;

  while (changes) {
    changes = false;
    for (const auto &rule : grammar) {
      auto first_from_right = calc_simple_FIRST(ans, rule.to, 0);
      size_t before = ans[rule.from].size();
      ans[rule.from].insert(first_from_right.begin(), first_from_right.end());
      size_t after = ans[rule.from].size();
      if (after > before) {
        changes = true;
      }
    }
  }
  return ans;
}

follow_res t_parser::calc_FOLLOW() {
  follow_res ans;

  for (const auto e : t_parser::all_nterms) {
    ans.insert(make_pair(e, set<t_token>()));
  }
  ans[S].insert(ENDOFSTREAM);

  bool changes = true;
  while (changes) {
    changes = false;
    for (const auto &rule : grammar) {
      for (size_t i = 0; i < rule.to.size(); i++) {
        auto var = rule.to[i];
        if (holds_alternative<t_nterm>(var)) {
          auto eta = calc_simple_FIRST(FIRST, rule.to, i + 1);
          auto it = eta.find(EPS);
          auto cur_nterm = get<t_nterm>(var);
          size_t before = ans[cur_nterm].size();
          if (it != eta.end() || i == rule.to.size() - 1) {
            if (it != eta.end()) {
              eta.erase(it);
            }
            auto follow_from = ans[rule.from];
            ans[cur_nterm].insert(follow_from.begin(), follow_from.end());
          }
          ans[cur_nterm].insert(eta.begin(), eta.end());
          if (i == rule.to.size() - 1 && rule.from == S) {
            ans[cur_nterm].insert(ENDOFSTREAM);
          }
          size_t after = ans[cur_nterm].size();
          if (before < after) {
            changes = true;
          }
        }
      }
    }
  }
  return ans;
}

node *t_parser::parse(const string &data, const t_node_data &st) {
  cout << "Parsing `" << data << "`" << endl;
  lexer = t_lexer(data);
  grammar = make_grammar();
  FIRST = calc_FIRST();
  FOLLOW = calc_FOLLOW();
  return parse_node_S(st);
}

t_parser::t_parser() : lexer("") {}

void print_tabs(int tabs) {
  for (int i = 0; i < tabs; i++) {
    cout << " ";
  }
}

string t_parser::get_name(node *v, int &node_num) {
  string name;
  if (holds_alternative<t_token>(v->data)) {
    name = code_to_text.at(get<t_token>(v->data));
  } else {
    name = nterm_to_text.at(get<t_nterm>(v->data));
  }
  name += "_" + to_string(node_num++);
  return name;
}

void t_parser::traverse(node *v, stringstream &fs, int test_num,
                        int &node_num) {
  if (node_num == 1) {
    v->name = get_name(v, node_num);
  }

  for (node *to : v->children) {
    to->name = get_name(to, node_num);
    fs << v->name << " -> " << to->name << endl;
    traverse(to, fs, test_num, node_num);
  }

  delete v;
}

void t_parser::print_tree(node *root, int test_num) {
  return;

  stringstream fs;
  // const char* test_file = ("pics/tree_" + to_string(test_num) +
  // ".gv").c_str(); fs.open(test_file, fstream::out);
  fs << "digraph {" << endl;

  int node_num = 1;
  traverse(root, fs, test_num, node_num);

  fs << "}" << endl;
  // fs.close();
  // cout << fs.str() << endl;
  // cout << "Parse tree written to pics/tree_" + to_string(test_num) + ".gv" <<
  // endl;
}

int test_num = 0;
node *test(t_parser &p, const string &s, const string &msg) {
  t_node_data nd;
  cout << "Test case: " << msg << endl;
  auto root = p.parse(s, nd);
  p.print_tree(root, test_num++);
  return root;
}

void test_exception(t_parser &p, const string &s, const string &msg) {
  bool caught = false;
  try {
    test(p, s, msg);
  } catch (const parse_exception &e) {
    cout << "Success, error caught at pos " << e.where() << endl << endl;
    caught = true;
  }
  if (!caught) {
    cout << "Error not caught, aborting further tests..." << endl;
    exit(0);
  }
}

void tests_KOTLIN() {
  t_parser parser;
  cout << "Running KOTLIN tests..." << endl;
  test(parser, "fun a()", "minimal possible function definition");
  test(parser, "fun name(): Unit", "testing return value");
  test(parser, "fun name(arg: Int): Unit", "testing single argument");
  test(parser, "fun name(arg: Int, arg2: Int): Unit",
       "testing multiple arguments");
  test(parser, "fun   name(  arg  :  Int  ,    arg2:  Int ):  Unit",
       "arbitrary amount of whitespaces");
  test(parser, "fun NaMe123(myARG2020: myTYPE2020): RESULT00TYPE",
       "testing complex identifiers with numbers and uppercase");

  test_exception(parser, "wrong a(a: Unit): Unit",
                 "starts not with the `fun` keyword");
  test_exception(parser, "fun 111(a: Unit): Unit",
                 "wrong Kotlin identifier syntax");
  test_exception(parser, "fun name(: Unit): Unit",
                 "missing first argument name");
  test_exception(parser, "fun name(a: Unit,): Unit",
                 "missing argument after comma");
  test_exception(parser, "fun name(,a: Unit): Unit",
                 "missing argument before comma");
  test_exception(parser, "fun name(a: Unit, :Unit): Unit",
                 "missing second argument name");
  test_exception(parser, "fun name(a: Unit, b): Unit",
                 "missing second argument type");
  test_exception(parser, "fun name(a: Unit, b Unit): Unit",
                 "missing colon in argument type");
  test_exception(parser, "fun name(a: Unit) Unit",
                 "missing colon in result type");
  test_exception(parser, "FuN a()", "Kotlin is case-dependent");
}

void test_assert(t_parser &p, const string &data, const string &msg,
                 int result) {
  node *root;
  root = test(p, data, msg);
  assert(root->node_data.result == result);
}

void tests_CALC() {
  cout << "Running CALC tests..." << endl;
  t_parser parser;
  test_assert(parser, "2;", "testing simple int value", 2);
  test_assert(parser, "2 + 2;", "testing simple sum expression", 4);
  test_assert(parser, "2 + 2 + 2 + 2;", "testing long sum expression", 8);
  test_assert(parser, "2 - 2 - 2 - 2;", "testing long subtract expression", -4);
  test_assert(parser, "-2 - (-2);", "testing unary minus", 0);
  test_assert(parser, "((((2))) + (((3))));", "testing deep nested parentheses",
              5);

  test_exception(parser, "(((2))(;", "fail on mismatched parentheses");
  test_exception(parser, "2 ++ 2;", "fail on unknown operation");
  test_exception(parser, "2 + 2 2 + 2", "fail on mismatched lines delimeter");
}

int main() {
  tests_KOTLIN();

  cout << "Starting REPL session..." << endl;

  t_parser parser;
  string s;
  while (getline(cin, s)) {
    if (s == "exit") {
      return 0;
    }
    t_node_data st; // allow user to generate starting values
    try {
      parser.print_tree(parser.parse(s, st), test_num++);
    } catch (const parse_exception &e) {
      cout << e.what() << endl;
    }
  }
}