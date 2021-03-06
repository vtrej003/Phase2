        /* cs152-miniL phase2 */
%{
#include <stdio.h>
#include <stdlib.h>
int yylex(void);
void yyerror(const char *msg);
extern int currLine;
extern int currPos;
extern const char* yytext;
extern FILE * yyin;
%}

%union{
int num_val;
char* ident_val;
}

%error-verbose
%locations
%start program
%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY ENUM OF IF THEN ENDIF ELSE WHILE DO BEGINLOOP ENDLOOP CONTINUE READ WRITE TRUE FALSE RETURN SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN OR AND NOT LT LTE GT GTE EQ NEQ ADD SUB MULT DIV MOD
%token <ident_val> IDENT
%token <num_val> NUMBER
%right ASSIGN
%left OR
%left AND
%right NOT
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MULT DIV MOD

%%
program: /*empty*/ {printf("program -> epsilon\n");} 
        | functions {printf("prog_start -> functions \n");}
        ;
      

functions: /*empty*/ {printf("functions -> epsilon\n");}
	      | function functions {printf("functions -> function functions\n");} 
        ;

function: FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
        {printf("function -> FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n");}
        ;

declarations: /*empty*/ {printf("declarations -> epsilon\n");}
        | declaration SEMICOLON declarations {printf("declarations -> declaration SEMICOLON declarations\n");}
        ;

declaration: identifiers COLON INTEGER {printf("declaration -> identifiers COLON INTEGER\n");}
        | identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
        | identifiers COLON ENUM L_PAREN identifiers R_PAREN {printf("declaration -> identifiers COLON ENUM L_PAREN identifiers R_PAREN\n");}
        ;

identifiers: ident {printf("identifiers -> ident\n");}
        | ident COMMA identifiers {printf("identifiers -> ident COMMA identifiers\n");}
        ;

ident: IDENT {printf("ident -> IDENT %s\n",yytext);}
        ;
        
statement: var ASSIGN expression {printf("statement -> var ASSIGN expression\n");}
        | IF bool_exp THEN statements ENDIF {printf("statement -> IF bool_exp THEN statements ENDIF\n");}
        | IF bool_exp THEN statements ELSE statements ENDIF {printf("statement -> IF bool_exp THEN statements ELSE statements ENDIF\n");}
        | WHILE bool_exp BEGINLOOP statements ENDLOOP {printf("statement -> WHILE bool_exp BEGINLOOP statements ENDLOOP\n");}
        | DO BEGINLOOP statements ENDLOOP WHILE bool_exp {printf("statement -> DO BEGINLOOP statements ENDLOOP WHILE bool_exp\n");}
        | READ vars {printf("statement -> READ vars\n");}
        | WRITE vars {printf("statement -> WRITE vars\n");}
        | CONTINUE {printf("statement -> CONTINUE\n");}
        | RETURN expression {printf("statement -> RETURN expression\n");}
        ;

statements: /*empty*/ {printf("statements -> epsilon\n");}
        | statement SEMICOLON statements{printf("statements -> statement SEMICOLON statements\n");}
        ;

vars: var {printf("vars -> var\n");}
        | var COMMA vars {printf("vars -> var COMMA vars\n");}
        ;
bool_exp: relation_and_exp {printf("bool_exp -> relation_and_exp\n");}
        | bool_exp OR relation_and_exp {printf("bool_exp -> bool_exp OR relation_and_exp\n");}
        ;

relation_and_exp: relation_exp {printf("relation_and_exp -> relation_exp\n");}
        | relation_and_exp AND relation_exp {printf("relation_and_exp -> relation_exp AND relation_exp\n");}
        ;

relation_exp: NOT exp1 {printf("relation_exp -> NOT relation_exp\n");}
        | exp1 {printf("relation_and_exp -> relation_exp\n");}
        ;

exp1: expression comp expression {printf("relation_exp -> expression comp expression\n");}
        | TRUE {printf("relation_exp -> TRUE\n");}
        | FALSE {printf("relation_exp -> FALSE\n");}
        | L_PAREN bool_exp R_PAREN {printf("relation_exp -> L_PAREN bool_exp R_PAREN\n");}
        ;

comp: EQ {printf("comp -> EQ\n");}
        | NEQ {printf("comp -> NEQ\n");}
        | LT {printf("comp -> LT\n");}
        | GT {printf("comp -> GT\n");}
        | LTE {printf("comp -> LTE\n");}
        | GTE {printf("comp -> GTE\n");}
        ;

expression: multiplicative_exp {printf("expression -> multiplicative_expression\n");}
        |  expression SUB multiplicative_exp {printf("expression -> multiplicative_expression SUB multiplicative_exp\n");}
        |  expression ADD multiplicative_exp {printf("expression -> multiplicative_expression ADD multiplicative_exp\n");}
        ;

multiplicative_exp: term {printf("multiplicative_expression -> term\n");}
        | multiplicative_exp MOD term {printf("multiplicative_expression -> multiplicative_expression MOD term\n");}
        | multiplicative_exp DIV term {printf("multiplicative_expression -> multiplicative_expression DIV term\n");}
        | multiplicative_exp MULT term {printf("multiplicative_expression -> multiplicative_expression MULT term\n");}
        ;

term: var {printf("term -> var\n");}
        | SUB var {printf("term -> SUB var\n");}
        | NUMBER {printf("term -> NUMBER\n");}
        | SUB NUMBER {printf("term -> SUB NUMBER\n");}
        | L_PAREN expression R_PAREN {printf("term -> L_PAREN expression R_PAREN\n");}
        | SUB L_PAREN expression R_PAREN {printf("term -> SUB L_PAREN expression R_PAREN\n");}
        | ident L_PAREN preloop R_PAREN {printf("term -> ident L_PAREN preloop R_PAREN\n");}
        ;

preloop: /*empty*/ {printf("preloop -> epsilon\n");}
        | expression COMMA preloop {printf("preloop -> expression COMMA preloop\n");}
        | expression {printf("preloop -> expression\n");}
        ;

var: ident {printf("var -> ident \n");}
        | ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
        ;

%%
int main(int argc, char ** argv){
        if(argc > 1){
                yyin = fopen(argv[1], "r");
                if (yyin == NULL){
                        printf("syntax: %s filename", argv[0]);
                }
        }
        yyparse();
        return 0;
}
void yyerror(const char *msg){
        printf("Error: Line %d, position %d: %s \n", currLine, currPos, msg);
}
