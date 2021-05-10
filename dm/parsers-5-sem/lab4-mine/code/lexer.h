#ifndef LEXER_H
#define LEXER_H
#include <cctype>
#include <iostream>
#include <regex>
#include <string>
#include <vector>

using namespace std;

enum t_token {
  FUN,
  COLON,
  COMMA,
  LPAREN,
  RPAREN,
  NAME,
  ENDOFSTREAM,
  EPS,
  UNKNOWN_TOKEN,
};

class t_lexer {
public:
  t_lexer(string data);

  pair<t_token, string> next_token();
  pair<t_token, string> get_token() const;
  t_token get_only_token() const;
  size_t last_pos;

private:
  void skip_spaces();

  t_token cur_token;
  string cur_matched;
  string data;
  size_t cur_pos;
};
#endif
