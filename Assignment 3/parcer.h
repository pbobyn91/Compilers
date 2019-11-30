#ifndef PARCER_H_
#define PARCER_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#define NO_ATTR -1

#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

Token lookahead;
int synerrno;

extern Token malar_next_token();
extern int line;
extern Buffer* str_LTBL;
extern char* kw_table[];

void parser();
void match(int, int);
void syn_eh(int);
void syn_printe();
void gen_incode(char*);
void program();
void opt_statements();
void statements();
void statements_p();
void statement();
void assignment();
void assignment_expression();
void selection();
void iteration();
void pre_condition();
void input();
void variable_list();
void variable_list_p();
void variable_identifier();
void output();
void output_list();
void arithmetic();
void unary_arithmetic();
void additive_arithmetic();
void additive_arithmetic_p();
void multiplicative_arithmetic();
void multiplicative_arithmetic_p();
void primary_arithmetic();
void string();
void string_p();
void primary_string();
void conditional();
void logical_OR();
void logical_OR_p();
void logical_AND();
void logical_AND_p();
void relational();
void primary_a_relational_p();
void primary_s_relational_p();
void primary_a_relational();
void primary_s_relational();

#endif