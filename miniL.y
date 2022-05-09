    /* cs152-miniL phase2 */
%{
#include <stdio.h>
#include <stdlib.h>
void yyerror(const char *msg);
extern int currLine;
extern int currPos;
extern const char* yytext;
FILE * yyin;
%}

%union{
  /* put your types here */
int num_val;
char* ident_val;
}

%error-verbose
%locations

%define parse.error verbose
/* %start program */
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
/* write your rules here */
program: /*empty*/ {printf("program -> epsilon\n");}
        | function program {printf("program -> function program\n");}
        ;

function: FUNCTION ident SEMICOLON BEGIN_PARAMS dec END_PARAMS BEGIN_LOCALS dec END_LOCALS BEGIN_BODY statements END_BODY
        {printf("function -> FUNCTION ident SEMICOLON BEGIN_PARAMS dec END_PARAMS BEGIN_LOCALS dec END_LOCALS BEGIN_BODY statements END_BODY\n");}
        ;

dec: /*empty*/ {printf("dec -> epsilon\n");}
        |declaration SEMICOLON dec {printf("dec -> decalaration SEMICOLON dec\n");}
        ;

declaration: identifiers COLON INTEGER {printf("declaration -> identifiers COLON INTEGER\n");}
        | identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
        | identifiers COLON ENUM L_PAREN identifiers R_PAREN {printf("declaration -> identifiers COLON ENUM L_PAREN identifiers R_PAREN\n");}
        ;

identifiers: ident {printf("identifiers -> ident\n");}
        | ident COMMA identifiers {printf("identifiers -> ident COMMA identifiers\n");}
        ;

statement: var ASSIGN expression {printf("statement -> var ASSIGN expression\n");}
        | IF bool_expr THEN statements ENDIF {printf("statement -> IF bool_expr THEN statements ENDIF\n");}
        | IF bool_expr THEN statements ELSE statements ENDIF {printf("statement -> IF bool_expr THEN statements ELSE statements ENDIF\n");}
        | WHILE bool_expr BEGINLOOP statements ENDLOOP {printf("statement -> WHILE bool_expr BEGINLOOP statements ENDLOOP\n");}
        | DO BEGINLOOP statements ENDLOOP WHILE bool_expr {printf("statement -> DO BEGINLOOP statements ENDLOOP WHILE bool_expr\n");}
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
bool_expr: relation_and_expr {printf("bool_expr -> relation_and_expr\n");}
        | bool_expr OR relation_and_expr {printf("bool_expr -> bool_expr OR relation_and_expr\n");}
        ;

relation_and_expr: relation_expr {printf("relation_and_expr -> relation_expr\n");}
        | relation_and_expr AND relation_expr {printf("relation_and_expr -> AND relation_expr\n");}
        ;

relation_expr: NOT exp1 {printf("relation_expr -> NOT exp1\n");}
        | exp1 {printf("relation_expr -> exp1\n");}
        ;

exp1: expression comp expression {printf("exp1 -> expression comp expression\n");}
        | TRUE {printf("exp1 -> TRUE\n");}
        | FALSE {printf("exp1 -> FALSE\n");}
        | L_PAREN bool_expr R_PAREN {printf("exp1 -> L_PAREN bool_expr R_PAREN\n");}
        ;

comp: EQ {printf("comp -> EQ\n");}
        | NEQ {printf("comp -> NEQ\n");}
        | LT {printf("comp -> LT\n");}
        | GT {printf("comp -> GT\n");}
        | LTE {printf("comp -> LTE\n");}
        | GTE {printf("comp -> GTE\n");}
        ;

expression: multiplicative_expr {printf("expression -> multiplicative_expr\n");}
        |  expression SUB multiplicative_expr {printf("expression -> expression SUB multiplicative_expr\n");}
        |  expression ADD multiplicative_expr {printf("expression -> expression ADD multiplicative_expr\n");}
        ;

multiplicative_expr: term {printf("multiplicative_expr -> term\n");}
        | multiplicative_expr MOD term {printf("multiplicative_expr -> multiplicative_expr MOD term\n");}
        | multiplicative_expr DIV term {printf("multiplicative_expr -> multiplicative_expr DIV term\n");}
        | multiplicative_expr MULT term {printf("multiplicative_expr -> multiplicative_expr MULT term\n");}
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
ident: IDENT {printf("ident -> IDENT %s\n",yytext);}
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
void yyerror(const char *msg) {
    /* implement your error handling */
        printf("Error: Line %d, position %d: %s \n", currLine, currPos, msg);
}
