

%{
	#include<stdio.h>
	#include <string.h>
	#include "1807063.tab.h"
	#include<stdlib.h>
	

%}

Datatype "INT"|"FLOAT"| "CHAR" | "DOUBLE"
identifiers [a-zA-Z]([a-zA-Z0-9])*
arithmatic_operator "ADD"|"SUB"|"MUL"|"DIV"|"POW"|"FACT"
unary_operator "INC"|"DEC"|"NOT"
relational_operator "GT"|"LT"|"LE"|"GE"|"EQ"|"NEQ"
parenthesis "{{"|"}}"|"(("|"))"
condition "IF"|"ELIF"|"ELSE"
loop "FOR"|"LOOP"
assignment_operator "="
integer (-|+)?[0-9]+
float [+-]?[0-9]+[.][0-9]+|[+-]?[.][0-9]+|[1-9]+e[+-][0-9]+
double [+-]?[0-9]+[.][0-9]+{6,12}|[+-]?[.][0-9]+|[1-9]+e[+-][0-9]+{6,12}
char [A-Za-z]+
single_line_comment [$][$].*
multiple_line_comment [ ]*[$][<][a-zA-Z0-9!@#*(){}_+-,.:\|?><\n\t ]*[>][$]
switch "SWITCH"|"CASE"|"DEFAULT"
libraryfunction "SHOW" | "MIN" | "MAX"

%%


{single_line_comment} {
	printf("single line comment\n");

}
{multiple_line_comment} {
	printf("multiple_line_comment\n");
}
"SHOW" {return SHOW;}
"INC"   {return INC;}
"DEC"	{return DEC;}
"MIN"   {return MIN;}
"MAX"   {return MAX;}
"NOT"	{return NOT;}
"{{"	 { return LB; }
"}}"	  { return RB; }
"BREAK"    {   return BREAK;     }
"CASE"      { return CASE;      }
"SWITCH"      { return SWITCH;	}
"DEFAULT"     {  return DEFAULT; 	}
"LOOP" { return WHILE;}
"INT"	{ return INT; }
"FLOAT"	{ return FLOAT; }
"DOUBLE"	{return DOUBLE;}
"CHAR"	{ return CHAR; }
":"     { return COL;}
[0-9]+	{ 
		yylval.number = atoi(yytext);
		return NUM;
	}
"FOR"   { return FOR;}	
"(("	{ return LP; }
"))"	{ return RP; }
";"	    { return CM; }
",,"	{ return SM; }
"ADD"	{ return PLUS;}
"SUB"	{ return MINUS; }
"MUL"	{ return MULT; }
"DIV"	{ return DIV; }
"POW"	{ return POW; }
"MOD"
"FACT"	{ return FACT; }
"AND"   { return AND; }
"OR"    { return OR;}
"XOR"	{ return XOR; }

"="		{ return ASSIGN;  }
"GT"	{ return GT; }
"LT"	{ return LT; }
"GE"    {return GE}
"LE"   { return LE}
"EQ"  { return EQ}

"IF"		{ return IF;}
"ELSE"		{ return ELSE;}
"ELIF "	{ return ELIF; }

{identifiers} {
	 yylval.string=strdup(yytext);
	 return VAR;
	}


"MAIN "[A-Za-z]+	{ return MAIN; }
"DEF "[A-Za-z]+	{return DEF;}

[ \t\n]*	

.	{
		yyerror("Unknown Character.\n");
	}
%%

/* user subroutines */

int main()
{
	yyin = freopen("in.txt","r",stdin);
	yyout = freopen("out.txt","w",stdout);
	yyparse();
	
}

