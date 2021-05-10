#ifndef PARSER_H
#define PARSER_H

#include <iostream>
#include <set>
#include <sstream>
#include <variant>
#include <vector>

#include "lexer.h"

using namespace std;

enum t_nterm { T, S, P, A, O, N, F };

using rruleside = vector<variant<t_nterm, t_token>>;
using first_res = map<t_nterm, set<t_token>>;
using follow_res = map<t_nterm, set<t_token>>;

struct t_rule {
  t_rule(t_nterm from, rruleside to) : from(from), to(to) {}

  t_nterm from;
  rruleside to;
};

struct t_node_data {
  string name;
  int result;
};

struct node {
  node(t_token term) : data(term) {}
  node(t_nterm term) : data(term) {}

  variant<t_nterm, t_token> data;
  vector<node *> children;
  string name;
  t_node_data node_data;
};

class t_parser {
public:
  t_parser();
  static constexpr t_nterm all_nterms[] = {T, S, P, A, O, N, F};

  node *parse(const string &data, const t_node_data &inh);
  void print_tree(node *v, int test_num);

  const map<t_token, string> code_to_text = {
      {UNKNOWN_TOKEN, "UNKNOWN_TOKEN"},
      {EPS, "EPS"},
      {ENDOFSTREAM, "ENDOFSTREAM"},
      {FUN, "FUN"},
      {COLON, "COLON"},
      {COMMA, "COMMA"},
      {LPAREN, "LPAREN"},
      {RPAREN, "RPAREN"},
      {NAME, "NAME"},
  };

  map<t_nterm, string> nterm_to_text = {{T, "T"}, {S, "S"}, {P, "P"}, {A, "A"},
                                        {O, "O"}, {N, "N"}, {F, "F"}};

private:
  vector<t_rule> make_grammar();
  first_res calc_FIRST();
  set<t_token> calc_simple_FIRST(first_res &fst, const rruleside &to, int from);
  follow_res calc_FOLLOW();

  set<t_token> g_first(t_nterm from, const rruleside &to);
  void print_children(node *cur);
  void traverse(node *v, stringstream &fs, int test_num, int &node_num);
  string get_name(node *v, int &node_num);

  t_lexer lexer;
  vector<t_rule> grammar;
  first_res FIRST;
  follow_res FOLLOW;

  node *parse_node_T(const t_node_data &inh);
  node *parse_node_S(const t_node_data &inh);
  node *parse_node_P(const t_node_data &inh);
  node *parse_node_A(const t_node_data &inh);
  node *parse_node_O(const t_node_data &inh);
  node *parse_node_N(const t_node_data &inh);
  node *parse_node_F(const t_node_data &inh);

  void assertEndOfStream();
  void ambiguous_fail(const set<t_token> &possible);
  t_node_data process_token(node *cur, t_token expected);
};

#endif
