%{
#include <map>
#include <string>
#include <cstring>
#include <sstream>
#include <cmath>

#include "tests.h"

using namespace std;

int get_var_value(const char*);
void put_var_value(const char*, int);
void log_line_result(int);

int yylex(void);
int yyerror(char*);
string format(int, const string&);

%}

%union{
  int		int_val;
  char*         op_val;
  char*         name;
  int           final_result;
}

%start	stmts 
%type	<final_result>	stmts

%token                  SEMICOLON
%token                  EQUALS
%token                  LPAREN
%token                  RPAREN

%token	<int_val>	INTEGER_LITERAL

%type	<int_val>	expE
%type	<int_val>	expM
%type	<int_val>	expV
%type	<int_val>	expV2
%type	<int_val>	stmt
%type   <int_val>       expP
/* %type   <name>          st_start */

%token  <name>           IDENTIFIER

%token	PLUS MINUS
%token  MULT DIV
%token  POW
/* implicit default action : $$ = $1 */

%%

stmts:	          stmt SEMICOLON stmts {}
                | /* empty */ {}
                ;

stmt:		  IDENTIFIER EQUALS expE 
                    { 
                        $$ = $3; 
                        put_var_value($1, $3);
                        cout << $1 << " = " << get_var_value($1) << endl; 
                        log_line_result($3);
                    }
                | expE 
                    {
                        cout << $1 << endl; 
                        log_line_result($1);
                    }

expE:             expE PLUS expP          { $$ = $1 + $3; }
                | expE MINUS expP         { $$ = $1 - $3; }
                | expP

expP:             expM POW expP           { $$ = pow($1, $3); }
                | expM

expM:             expM MULT expV2	{ $$ = $1 * $3; }
		| expM DIV  expV2	
                    { 
                        if ($3)  {
                            $$ = $1 / $3;
                        } else {
                            yyerror(strdup(format(@3.first_column, "division by zero").c_str()));
                        }
                    }
                | expV2

expV2:            MINUS expV %prec MULT { $$ = $2 * (-1); }
                | expV

expV:             INTEGER_LITERAL       { $$ = $1; }
                | IDENTIFIER            { $$ = get_var_value($1); }
                | LPAREN expE RPAREN    { $$ = $2; }

%%

int yylex(void);

map<string, int> variables;
vector<int> line_results;

int get_var_value(const char* _s) {
    auto s = string(_s);
    return variables.at(s);
}

void put_var_value(const char* _s, int value) {
    auto s = string(_s);
    variables[s] = value;
}

void log_line_result(int value) {
    line_results.push_back(value);
}

int yyerror(char* s) {
    extern int yylineno;
    extern char *yytext;
    stringstream ss;
    ss << "ERROR: " << s << " at symbol \"" << yytext;
    ss << "\" on line " << yylineno << endl;
    throw strdup(ss.str().c_str());
}

string format(int c, const string& message) {
    return message + " at pos " + to_string(c);
}

int main() {
    t_tester t;
    t.run_suite();
}
