%option noyywrap
%{
#include "calc.tab.h"
int yyerror(char *s);
%}

digit		[0-9]
int_const	{digit}+

%%

{int_const}	{ yylval.int_val = atoi(yytext); return INTEGER_LITERAL; }
"+"		{ yylval.op_val = "+"; return PLUS; }
"*"		{ yylval.op_val = "*"; return MULT; }

[ \t]*		{}
[\n]		{ yylineno++;	}

.		{ yyerror("");	}

%%
