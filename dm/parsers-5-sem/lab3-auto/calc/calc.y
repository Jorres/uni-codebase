%{
#include <iostream>
using namespace std;
int yylex(void);
int yyerror(char* s);
%}

%union{
  int		int_val;
  char*         op_val;
}

%start	input 

%token	<int_val>	INTEGER_LITERAL
%type	<int_val>	exp

%left	PLUS
%left	MULT

%%

input:		/* empty */
		| exp	{ cout << "Result: " << $1 << endl; }
		;

exp:		INTEGER_LITERAL	{ $$ = $1; }
		| exp PLUS exp	{ $$ = $1 + $3; }
		| exp MULT exp	{ $$ = $1 * $3; }
		;

%%

int yylex(void);
int yyparse(void);

int yyerror(char* s)
{
  /* extern int yylineno;	// defined and maintained in lex.c */
  /* extern char *yytext;	// defined and maintained in lex.c */
  /* cerr << "ERROR: " << s << " at symbol \"" << yytext; */
  /* cerr << "\" on line " << yylineno << endl; */
  /* exit(1); */
  cout << "yyerror" << endl;
}

int main() {
    /* freopen("input.txt", "r", stdin); */
    yyparse();
    return 0;
}
