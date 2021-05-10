#include <cassert>

#include <map>
#include <set>

#include <vector>

#include "parse_exception.h"
#include "parser.h"

map<string, set<string>> storage;

void push_to_storage(const string &name, const string &type) {
  storage[type].insert(name);
}

void print_storage() {
  for (const auto &p : storage) {
    cout << p.first << " : ";
    for (const auto &arg : p.second) {
      cout << arg << ", ";
    }
    cout << endl;
  }
}

t_node_data t_parser::process_token(node *cur, t_token expected) {
  auto ptoken = lexer.get_token();

  auto token = ptoken.first;
  auto matched = ptoken.second;

  stringstream ss;
  if (token != expected) {
    ss << "Parsing failed at pos " << lexer.last_pos << endl;
    ss << "Currently parsing " << nterm_to_text.at(get<t_nterm>(cur->data))
       << endl;
    ss << "Found: " << code_to_text.at(token) << endl;
    ss << "Expected: " << code_to_text.at(expected) << endl;

    throw parse_exception(lexer.last_pos, ss.str());
  }
  node *term = new node(token);

  if (token == NAME) {
    term->node_data.name = matched;
  }

  cur->children.push_back(term);
  lexer.next_token();
  return term->node_data;
}

void t_parser::ambiguous_fail(const set<t_token> &possible) {
  stringstream ss;
  ss << "Parsing failed at pos " << lexer.last_pos << endl;
  ss << "Found: " << code_to_text.at(lexer.get_only_token()) << endl;
  ss << "Expected one of the following: " << endl;
  for (auto &possible_token : possible) {
    ss << code_to_text.at(possible_token) << endl;
  }
  throw parse_exception(lexer.last_pos, ss.str());
}

t_node_data safe_append(node *v, node *res) {
  if (res->children.size() > 0) {
    v->children.push_back(res);
  }
  return res->node_data;
}

set<t_token> t_parser::g_first(t_nterm n, const rruleside &to) {
  auto to_set = calc_simple_FIRST(FIRST, to, 0);
  auto it = to_set.find(EPS);
  if (it != to_set.end()) {
    to_set.erase(it);
    auto follow_from = FOLLOW[n];
    to_set.insert(follow_from.begin(), follow_from.end());
  }
  return to_set;
}

bool in(t_token t, const set<t_token> &firsts) {
  return firsts.find(t) != firsts.end();
}

void t_parser::print_children(node *cur) {
  cout << nterm_to_text[get<t_nterm>(cur->data)] << " : ";
  for (auto &v : cur->children) {
    if (holds_alternative<t_nterm>(v->data)) {
      cout << nterm_to_text.at(get<t_nterm>(v->data)) << " ";
    } else {
      cout << code_to_text.at(get<t_token>(v->data)) << " ";
    }
  }
  cout << endl;
}

void t_parser::assertEndOfStream() {
  t_token last = lexer.get_only_token();
  if (last != ENDOFSTREAM) {
    throw parse_exception(lexer.last_pos, "End of input expected" +
                                              code_to_text.at(last) +
                                              " found.");
  }
}

node *t_parser::parse_node_T(const t_node_data &inh) {
  node *cur = new node(T);
  auto ptoken = lexer.get_token();
  auto token = ptoken.first;
  auto matched = ptoken.second;
  vector<t_node_data> inh_next(100, inh);
  vector<t_node_data> sin_next(100, t_node_data{});
  if (in(token, g_first(T, rruleside{
                               EPS,
                           }))) {
    cur->node_data = t_node_data{};

  } else if (in(token, g_first(T, rruleside{
                                      COMMA,
                                      O,
                                      T,
                                  }))) {
    cur->node_data = t_node_data{};

    sin_next[1] = process_token(cur, COMMA);

    sin_next[2] = safe_append(cur, parse_node_O(inh_next[2]));

    sin_next[3] = safe_append(cur, parse_node_T(inh_next[3]));

  } else {
    ambiguous_fail(FIRST.at(T));
  }
  return cur;
}
node *t_parser::parse_node_S(const t_node_data &inh) {
  node *cur = new node(S);
  auto ptoken = lexer.get_token();
  auto token = ptoken.first;
  auto matched = ptoken.second;
  vector<t_node_data> inh_next(100, inh);
  vector<t_node_data> sin_next(100, t_node_data{});
  if (in(token, g_first(S, rruleside{
                               F,
                               N,
                               LPAREN,
                               A,
                               RPAREN,
                               P,
                           }))) {
    cur->node_data = t_node_data{};
    { storage.clear(); }
    sin_next[1] = safe_append(cur, parse_node_F(inh_next[1]));

    sin_next[2] = safe_append(cur, parse_node_N(inh_next[2]));

    sin_next[3] = process_token(cur, LPAREN);

    sin_next[4] = safe_append(cur, parse_node_A(inh_next[4]));

    sin_next[5] = process_token(cur, RPAREN);

    sin_next[6] = safe_append(cur, parse_node_P(inh_next[6]));
    { print_storage(); }
  } else {
    ambiguous_fail(FIRST.at(S));
  }
  return cur;
}
node *t_parser::parse_node_P(const t_node_data &inh) {
  node *cur = new node(P);
  auto ptoken = lexer.get_token();
  auto token = ptoken.first;
  auto matched = ptoken.second;
  vector<t_node_data> inh_next(100, inh);
  vector<t_node_data> sin_next(100, t_node_data{});
  if (in(token, g_first(P, rruleside{
                               EPS,
                           }))) {
    cur->node_data = t_node_data{};

  } else if (in(token, g_first(P, rruleside{
                                      COLON,
                                      N,
                                  }))) {
    cur->node_data = t_node_data{};

    sin_next[1] = process_token(cur, COLON);

    sin_next[2] = safe_append(cur, parse_node_N(inh_next[2]));

  } else {
    ambiguous_fail(FIRST.at(P));
  }
  return cur;
}
node *t_parser::parse_node_A(const t_node_data &inh) {
  node *cur = new node(A);
  auto ptoken = lexer.get_token();
  auto token = ptoken.first;
  auto matched = ptoken.second;
  vector<t_node_data> inh_next(100, inh);
  vector<t_node_data> sin_next(100, t_node_data{});
  if (in(token, g_first(A, rruleside{
                               EPS,
                           }))) {
    cur->node_data = t_node_data{};

  } else if (in(token, g_first(A, rruleside{
                                      O,
                                      T,
                                  }))) {
    cur->node_data = t_node_data{};

    sin_next[1] = safe_append(cur, parse_node_O(inh_next[1]));

    sin_next[2] = safe_append(cur, parse_node_T(inh_next[2]));

  } else {
    ambiguous_fail(FIRST.at(A));
  }
  return cur;
}
node *t_parser::parse_node_O(const t_node_data &inh) {
  node *cur = new node(O);
  auto ptoken = lexer.get_token();
  auto token = ptoken.first;
  auto matched = ptoken.second;
  vector<t_node_data> inh_next(100, inh);
  vector<t_node_data> sin_next(100, t_node_data{});
  if (in(token, g_first(O, rruleside{
                               N,
                               COLON,
                               N,
                           }))) {
    cur->node_data = t_node_data{};

    sin_next[1] = safe_append(cur, parse_node_N(inh_next[1]));

    sin_next[2] = process_token(cur, COLON);

    sin_next[3] = safe_append(cur, parse_node_N(inh_next[3]));
    { push_to_storage(sin_next[1].name, sin_next[3].name); }
  } else {
    ambiguous_fail(FIRST.at(O));
  }
  return cur;
}
node *t_parser::parse_node_N(const t_node_data &inh) {
  node *cur = new node(N);
  auto ptoken = lexer.get_token();
  auto token = ptoken.first;
  auto matched = ptoken.second;
  vector<t_node_data> inh_next(100, inh);
  vector<t_node_data> sin_next(100, t_node_data{});
  if (in(token, g_first(N, rruleside{
                               NAME,
                           }))) {
    cur->node_data = t_node_data{};

    sin_next[1] = process_token(cur, NAME);
    { cur->node_data.name = sin_next[1].name; }
  } else {
    ambiguous_fail(FIRST.at(N));
  }
  return cur;
}
node *t_parser::parse_node_F(const t_node_data &inh) {
  node *cur = new node(F);
  auto ptoken = lexer.get_token();
  auto token = ptoken.first;
  auto matched = ptoken.second;
  vector<t_node_data> inh_next(100, inh);
  vector<t_node_data> sin_next(100, t_node_data{});
  if (in(token, g_first(F, rruleside{
                               FUN,
                           }))) {
    cur->node_data = t_node_data{};

    sin_next[1] = process_token(cur, FUN);

  } else {
    ambiguous_fail(FIRST.at(F));
  }
  return cur;
}
