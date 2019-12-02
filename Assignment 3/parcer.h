/****************************************
 * Filename: Parcer.h
 * Compiler: MS Visual Studio 2019
 * Author: Patrick Bobyn
 * Course: CST-8152 - Compilers 012
 * Assignment: 3
 * Date: December 6th 2019
 * Professor: Svillan Ranev
 * Purpose: All of the header files and variables required for parcer.c
 ***************************************/

#ifndef PARCER_H_
#define PARCER_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

/* Define a No Attribute value to be used when matching */
#define NO_ATTR -1

/* all the keywords with the location in array as a value */
#define ELSE 0			/* ELSE keyword */
#define FALSE 1			/* FALSE keyword */
#define IF 2			/* IF keyword*/
#define PLATYPUS 3		/* PLATYPUS keyword*/
#define READ 4			/* READ keyword */
#define REPEAT 5		/* REPEAT keyword */
#define THEN 6			/* THEN keyword */
#define TRUE 7			/* TRUE keyword */
#define WHILE 8			/* WHILE keyword */
#define WRITE 9			/* WRITE keyword */

Token lookahead;		/* the lookahead Token*/
int synerrno;			/* number of errors */

extern Token malar_next_token();	/* external function malar_next_token() from scanner */
extern int line;		/* external int line to count the lines */
extern Buffer* str_LTBL;	/* the str_LTBL buffer */
extern char* kw_table[];	/* the external kw_table */

/* List of all functions in parcer.c, these are all named according to the Syntactic Specifications */
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