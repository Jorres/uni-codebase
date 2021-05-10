%option noyywrap
%{
#include <cstring>
#include "calc.tab.h"
int yyerror(char *s);
%}

digit		[0-9]
int_const	{digit}+
id              ([a-zA-z])([a-zA-Z0-9]*)

%%

{id}            { yylval.name = strdup(yytext); return IDENTIFIER; }
{int_const}	{ yylval.int_val = atoi(yytext); return INTEGER_LITERAL; }

"+"		{ yylval.op_val = strdup("+"); return PLUS; }
"*"		{ yylval.op_val = strdup("*"); return MULT; }
"="             { return EQUALS; }
";"             { return SEMICOLON; }
"("             { return LPAREN; }
")"             { return RPAREN; }
"/"             { return DIV; }
"-"             { return MINUS; }

"**"		{ yylval.op_val = strdup("**"); return POW; }

[ \t]*		{}
[\n]		{ yylineno++;	}

.		{ yyerror(". met");	}

%%
