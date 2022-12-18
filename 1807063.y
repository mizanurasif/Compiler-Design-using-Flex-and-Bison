

%{
	#include<stdio.h>
	#include <math.h>
	int cnt=1,cntt=0,val,track=0;
	typedef struct entry {
    	char *str;
    	int n;
	}storage;
	storage store[1000],sym[1000];
	void insert (storage *p, char *s, int n);
	int cnt2=1; 
	void insert2 (storage *p, char *s, int n);
	#define pi  3.1416
	
%}
%union 
{
        int number;
        char *string;
}

%token <number> NUM
%token <string> VAR 
%token <string> IF ELIF ELSE MAIN INT FLOAT DOUBLE CHAR LP RP LB RB CM SM PLUS MINUS MULT DIV POW FACT ASSIGN FOR COL WHILE BREAK COLON DEFAULT CASE SWITCH INC DEC NOT DEF SHOW
%token <string> MIN MAX MOD AND OR XOR GE LE EQ
%type <string> statement
%type <number> expression
%type <number> switch_expr
%nonassoc IFX
%nonassoc ELIFX
%nonassoc ELSE
%left LT GT GE LE EQ
%left PLUS MINUS
%left MULT DIV
%left FACT
%right POW

/* Simple grammar rules */

%%

program: MAIN LP RP LB cstatement RB { printf("\n[Successful compilation]\n"); }
	 ;

cstatement: /* empty */

	| cstatement statement
	
	| cdeclaration
	;

cdeclaration:	TYPE ID1 SM	{ printf("\n[valid declaration]\n"); }
   
			;
			
TYPE : INT

     | FLOAT

     | CHAR
     ;

ID1  : ID1 CM VAR	{
						if(search_for_var($3))
						{
							printf("[%s is already declared]\n", $3 );
						}
						else
						{
							insert(&store[cnt],$3, cnt);
							cnt++;
							
						}
			}

     |VAR	{
				if(search_for_var($1))
				{
					printf("[%s is already declared]\n", $1 );
				}
				else
				{
					insert(&store[cnt],$1, cnt);
							cnt++;
				}
			}
     ;

statement: SM
	| SWITCH LP switch_expr RP LB BASE RB    {printf("[SWITCH CASE]\n");} 
	| SHOW LP VAR RP SM                 {if(search_for_var($3))
										 {
											int i= search_for_varindex($3);
											
											printf("\n[%s --> Value of the variable: %d]\t\n",$3,sym[i].n);
										 }
										 else
										 {
											printf("[%s not declared yet]\n",$3);
										 }
										} 
	| expression SM 			{ printf("\n[value of expression: %d]\n", ($1)); }
	

        | VAR ASSIGN expression SM 		{
							if(search_for_var($1)){
								int i = search_for_varindex($1);
								if (!i){
									insert(&sym[cntt], $1, $3);
									cntt++;
								}
								else
								sym[i].n = $3;
								printf("\n[%s---> Value of the variable: %d]\t\n",$1,$3);
							}
							else {
								printf("[%s not declared yet]\n",$1);
							}
							
						}

	| IF LP expression RP LB expression SM RB %prec IFX {
								if($3)
								{
									printf("\n[value of expression in IF: %d]\n",($6));
								}
								else
								{
									printf("\n[condition value zero in IF block]\n");
								}
							}

	| IF LP expression RP LB expression SM RB ELSE LB expression SM RB {
								 	if($3)
									{
										printf("\n[value of expression in IF: %d]\n",$6);
									}
									else
									{
										printf("\n[value of expression in ELSE: %d]\n",$11);
									}
								   }
	| IF LP expression RP LB IF LP expression RP LB expression SM RB ELSE LB expression SM RB expression SM RB ELSE LB expression SM RB %prec IFX {
								 	if($3)
									{
										if($8)
											printf("\n[value of expression middle IF: %d]\n",$11);
										else
											printf("\n[value of expression middle ELSE: %d]\n",$16);
										printf("\n[value of expression in first IF: %d]\n",$19);
									}
									else
									{
										printf("\n[value of expression in else: %d]\n",$24);
									}
								   }
	| IF LP expression RP LB expression SM RB ELIF LP expression RP LB expression SM RB ELSE LB expression SM RB {
								 	if($3)
									{
										printf("\n[value of expression in IF: %d]\n",$6);
									}
									else if($11)
									{
										printf("\n[value of expression in ELIF: %d]\n",$14);
									}
									else
									{
										printf("\n[value of expression in ELSE: %d]\n",$19);
									}
								   }							   
	| FOR LP NUM COL NUM RP LB expression RB     {
	   if($3 <= $5)
	   {
	   int i=0;
	   printf("[FOR LOOP FROM %d to %d]\n",$3,$5);
	   for(i=$3;i<$5;i++){
	   printf("%d\n",i);
	   }
	   printf("\n");
	   printf("[value of the expression: %d]\n",$8);
	   }
	   else
	   {
	   int i=0;
	   printf("[FOR LOOP FROM %d to %d]\n",$3,$5);
	   for(i=$3;i>$5;i--){
	   printf("%d\n",i);
	   }
	   printf("\n");
	   printf("[value of the expression: %d]\n",$8);
	   }
	}
	| FOR LP NUM COL NUM COL NUM RP LB expression RB     {
	   if($3<=$5)
	   {
	   int i=0;
	   int x=$7;
	   printf("\n[FOR LOOP FROM %d to %d INCREMENT BY %d]\n",$3,$5,$7);
	   for(i=$3;i<$5;i=i+x){
	   printf("%d\n",i);
	   }
	   printf("\n");
	   printf("[value of the expression: %d]\n",$10);
	}
	else
	{
	   int i=0;
	   int x=$7;
	   printf("\n[FOR LOOP FROM %d to %d DECREMENT BY %d]\n",$3,$5,$7);
	   for(i=$3;i>$5;i=i-x){
	   printf("%d\n",i);
	   }
	   printf("\n");
	   printf("[value of the expression: %d]\n",$10);
	}
}
	| WHILE LP NUM GT NUM RP LB expression RB   {
										int i;
										printf("[While LOOP FROM %d to %d]\n",$3,$5);
										for(i=$3;i<=$5;i++)
										{
											printf("%d\n",i);
										}
										printf("\n");
										printf("[value of the expression: %d]\n",$8);

	}
/*------function begin-----------*/

	| DEF func
	;

			func : COL TYPE LP RP LB statement RB
							{
								printf("[Function Declared]\n");
							}
				;


/*-------function end------------*/
	
///////////////////////
	
			BASE : Bas   
				 | Bas Dflt 
				 ;

			Bas   : /*NULL*/
				 | Bas Cs     
				 ;

			Cs    : CASE NUM COL expression SM   {
						
						if(val==$2){
							  track=1;
							  printf("\nCase No : %d  and Result :  %d\n",$2,$4);
						}
					}
				 ;

			Dflt    : DEFAULT COL expression SM    {
						if(track!=1){
							printf("\nResult in default Value is :  %d\n",$3);
						}
						track=0;
					}
				 ;    
	/////////////////////////////
	
	
expression: NUM				{ $$ = $1; 	}

	| VAR				{
	                    if(search_for_var($1))
										 {
											int i= search_for_varindex($1);
											if(i==0)
											{
												printf("[%s not declared yet]\n",$1);
												$$=0;
											}
											else
											$$ = sym[i].n;
										 }
										 else
										 {
											printf("[%s not declared yet]\n",$1);
											$$=0;
										 }
						 }

	| expression PLUS expression	{ $$ = $1 + $3; }

	| expression MINUS expression	{ $$ = $1 - $3; }

	| expression MULT expression	{ $$ = $1 * $3; }

	| expression DIV expression	{ 	if($3) 
				  		{
				     			$$ = $1 / $3;
				  		}
				  		else
				  		{
							$$ = 0;
							printf("\n[division by zero]\n");
				  		} 	
				    	}
	| expression AND expression	{ $$ = $1 && $3; }
	| expression OR expression	{ $$ = $1 || $3; }
	| expression XOR expression	{ $$ = $1 ^ $3; }			
	| expression MOD expression	{ $$ = $1 % $3; }
	| POW LP expression CM  expression RP { $$ = pow($3,$5); }
	| MAX LP expression CM  expression RP { if($3>=$5)
	                              {
									$$=$3;
									printf("[MAX NUM is %d]\n",$3);
								  }
								  else
								  {
									$$=$5;
									printf("[MAX NUM is %d]\n",$5);
								  }
								  }
	| MIN LP expression CM  expression RP {  if($3<=$5)
	                              {
									$$=$3;
									printf("[Min NUM is %d]\n",$3);
								  }
								  else
								  {
									$$=$5;
									printf("[Min NUM is %d]\n",$5);
								  } 
								}

	| expression FACT {
						int mult=1 ,i;
						for(i=$1;i>0;i--)
						{
							mult=mult*i;
						}
						$$=mult;
						
					 }	

	| expression LT expression	{ $$ = $1 < $3; }

	| expression GT expression	{ 
		                         $$ = $1 > $3; }
	| expression GE expression	{ if($1>=$3){
		$$ = 1;
	}
	else{
		$$ = 0;
	} }
	| expression LE expression	{ if($1<=$3){
		$$ = 1;
	}
	else{
		$$ = 0;
	} }
	| expression EQ expression	{ if($1==$3){
									$$ = 1;
									}
									else{
										$$ = 0;
									} 
	}

	| LP expression RP		{ $$ = $2;	}
	
	| expression INC         { $$=$1+1; printf("[INC: %d]\n",$$);}

	| expression DEC         { $$=$1-1; printf("[DEC: %d\n]",$$);}

	| expression NOT {
						if($1 != 0)
						{
							$$ = 0; printf("[NOT: %d]\n",$$);
						}
						else{
							$$ = 1 ; printf("[NOT: %d]\n",$$);
						}
					}
	;


///////////////////////
 switch_expr: NUM				{ $$ = $1; val = $$;	}

	| VAR				{ $$ = search_for_varindex($1); val = $$;}

	| switch_expr PLUS switch_expr	{ $$ = $1 + $3; val = $$; }

	| switch_expr MINUS switch_expr	{ $$ = $1 - $3; val = $$; }

	| switch_expr MULT switch_expr	{ $$ = $1 * $3;  val = $$;}

	| switch_expr DIV switch_expr	{ 	if($3) 
				  		{
				     			$$ = $1 / $3; val = $$;
				  		}
				  		else
				  		{
							$$ = 0;
							 val = $$;
				  		} 	
				    	}
	| switch_expr POW switch_expr { $$ = pow($1,$3);  val = $$;}

	| switch_expr FACT {
						int mult=1 ,i;
						for(i=$1;i>0;i--)
						{
							mult=mult*i;
						}
						$$=mult; val = $$;
						
					 }	

	| switch_expr LT switch_expr	{ $$ = $1 < $3; val = $$; }

	| switch_expr GT switch_expr	{ $$ = $1 > $3;  val = $$;}

	| LP switch_expr RP		{ $$ = $2;	 val = $$;}
	
	| switch_expr INC         { $$=$1+1; printf("[INC: %d]\n",$$); val = $$;}

	| switch_expr DEC         { $$=$1-1; printf("[DEC: %d]\n",$$); val = $$;}

	| switch_expr NOT {
						if($1 != 0)
						{
							$$ = 0; val = $$;
						}
						else{
							$$ = 1 ; val = $$;
						}
					}
	;
%%

//////////////////////////
void insert(storage *p, char *s, int n)
{
  p->str = s;
  p->n = n;
}

int search_for_var(char *key)
{
    int i = 1;
    char *name = store[i].str;
    while (name) {
        if (strcmp(name, key) == 0)
            return store[i].n;
        name = store[++i].str;
    }
    return 0;
}
/////////////////////////
void insert2 (storage *p, char *s, int n)
{
  p->str = s;
  p->n = n;
  
}

int search_for_varindex(char *key)
{
    int i = 1;
    char *name = sym[i].str;
    while (name) {
        if (strcmp(name, key) == 0)
            return i;
        name = sym[++i].str;
    }
    return 0;
}

///////////////////////////


int yywrap()
{
return 1;
}


yyerror(char *s){
	printf( "%s\n", s);
}

