#include "lexer.h"
#include <iostream>
#include <map>

t_lexer::t_lexer(string data) : last_pos(-1), data(data), cur_pos(0) {
  this->next_token();
}

pair<t_token, string> t_lexer::get_token() const {
  return make_pair(cur_token, cur_matched);
}

t_token t_lexer::get_only_token() const { return cur_token; }

void t_lexer::skip_spaces() {
  auto wsregex = regex(R"rgx(^[\s]+)rgx");
  smatch match;
  const string &cref = data;
  if (regex_search(cref.begin() + cur_pos, cref.end(), match, wsregex)) {
    cur_pos += match[0].str().size();
  }
}

pair<t_token, string> t_lexer::next_token() {
  last_pos = cur_pos;
  if (cur_pos >= data.size()) {
    cur_token = ENDOFSTREAM;
    return make_pair(cur_token, "");
  }

  skip_spaces();

  vector<pair<const regex, t_token>> tokens = {
      {regex(R"rgx(^fun)rgx"), FUN},   {regex(R"rgx(^:)rgx"), COLON},
      {regex(R"rgx(^,)rgx"), COMMA},   {regex(R"rgx(^\()rgx"), LPAREN},
      {regex(R"rgx(^\))rgx"), RPAREN}, {regex(R"rgx(^[a-zA-Z][\w]*)rgx"), NAME},
  };

  smatch match;
  for (auto &p : tokens) {
    const string &cref = data;
    if (regex_search(cref.begin() + cur_pos, cref.end(), match, p.first)) {
      cur_matched = match[0].str();
      cur_pos += cur_matched.size();
      cur_token = p.second;
      return make_pair(cur_token, cur_matched);
    }
  }
  cur_token = UNKNOWN_TOKEN;
  return make_pair(cur_token, "");
}

// int main_lexer() {
//     const map<t_token, string> code_to_text = {
//         {FUN, "FUN"},
//         {COLON, "COLON"},
//         {COMMA, "COMMA"},
//         {NAME, "NAME"},
//         // {INT, "INT"},
//         // {PLUS, "PLUS"},
//         // {MINUS, "MINUS"},
//         // {MULT, "MULT"},
//         // {DIV, "DIV"},
//         {LPAREN, "LPAREN"},
//         {RPAREN, "RPAREN"},
//         {ENDOFSTREAM, "ENDOFSTREAM"},
//         {EPS, "EPS"},
//         {UNKNOWN_TOKEN, "UNKNOWN_TOKEN"},
//     };
//
//     // string s = "+ - (+ - - * / 234 63456 75675 2334) - *345 -3 -3";
//     string s = "fun 111(a: Unit): Unit";
//     t_lexer lexer(s);
//     int it = 0;
//     while (lexer.get_token() != ENDOFSTREAM && it < 20) {
//         cout << code_to_text.at(lexer.get_token()) << endl;
//         lexer.next_token();
//         it++;
//     }
// }
