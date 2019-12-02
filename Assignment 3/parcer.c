/* Filename: parcer.c
* Compiler: MS Visual Studio 2019
* Author: Patrick Bobyn
* Course: CST 8152 - Compilers 012
* Assignment: 3
* Date: December 6th 2019
* Professor: Sv. Ranev
* Purpose: Parses the tokens collected by the scanner.
* Function List: parser(), match(), syn_eh(), syn_printe(), gen_incode(), program(), opt_statements(), statements(), statements_p(), 
*				 statement(), assignment(), assignment_expression(), selection(), iteration(), pre_condition(), input(), variable_list(), 
*				 variable_list_p(), variable_identifier(), output(), output_list(), arithmetic(), unary_arithmetic(), additive_arithmetic(),
*				 additive_arithmetic_p(), multiplicative_arithmetic(), multiplicative_arithmetic_p(), primary_arithmetic(), string(), 
*				 string_p(), primary_string(), conditional(), logical_OR(), logical_OR_p(), logical_AND(), logical_AND_p(), relational(),
*				 primary_a_relational_p(), primary_s_relational_p(), primary_a_relational(), primary_s_relational()

******************************************************/

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

#include "parcer.h"

/*****************************************************
* Function Name: parser()
* Author: Patrick Bobyn
* History/Version: November 6 1.0
* Called Functions: malar_next_token(), program(), match(), gen_incode()
* Parameters: void
* Return Value: void
* Algorithm:
*****************************************************/
void parser() {
	lookahead = malar_next_token();
	program(); 
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed\n");
}

/*****************************************************
* Function Name: match(int, int)
* Author: Patrick Bobyn
* History/Version: November 6 1.0
* Called Functions: syn_eh(), malar_next_token(), syn_printe()
* Parameters: int pr_token_code, int pr_token_attribute
* Return Value: void
* Algorithm:
*****************************************************/
void match(int pr_token_code, int pr_token_attribute) {

	/* make sure the lookahead.code at this location is the proper code */
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code);
		return;
	}

	/* if the lookahead.code is SEOF_T then return */
	if (lookahead.code == SEOF_T) {
		return;
	}

	switch (pr_token_code) {
	case KW_T:	/* if the lookahead code is a keyword make sure its the correct attribute */
		if (pr_token_attribute != lookahead.attribute.kwt_idx) {
			syn_eh(pr_token_code);
			return;
		}
		break;
	case LOG_OP_T:	/* if the lookahead code is a log_op make sure its the correct attribute */
		if (pr_token_attribute != lookahead.attribute.log_op) {
			syn_eh(pr_token_code);
			return;
		}
		break;
	case ART_OP_T: /* if the lookahead code is an art_op make sure it is the correct attribute */
		if (pr_token_attribute != lookahead.attribute.arr_op) {
			syn_eh(pr_token_code);
			return;
		}
		break;
	case REL_OP_T: /* if lookahead code is rel_op make sure its the correct attribute */
		if (pr_token_attribute != lookahead.attribute.rel_op) {
			syn_eh(pr_token_code);
			return;
		}
		break;
	default:
		break;
	}

	/* get the next token */
	lookahead = malar_next_token();
	
	/* if the next token is an error print out the error and then get the next token */
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}
}

/*****************************************************
* Function Name: syn_eh()
* Author: Patrick Bobyn
* History/Version: November 6 1.0
* Called Functions: syn_printe(), malar_next_token(), exit()
* Parameters: int
* Return Value: void
* Algorithm:
*****************************************************/
void syn_eh(int sync_token_code) {

	/* print the error out and increment the error number */
	syn_printe();
	synerrno++;

	do {
		/* get next token */
		lookahead = malar_next_token();

		/* check the lookahead code against the input parameter*/
		if (sync_token_code == lookahead.code) {
			if (sync_token_code != SEOF_T) /* if lookahead is not SEOF_T get the next token then return */
				lookahead = malar_next_token();
			return;
		}

		/* if lookahead code is SEOF_T exit the program */
		if (lookahead.code == SEOF_T) {
			exit(synerrno);
			return;
		}

		/* loop while the lookahead code is not the input parameter */
	} while (sync_token_code != lookahead.code);
}

/*****************************************************
* Function Name: syn_printe()
* Author: Patrick Bobyn
* History/Version: November 6 1.0
* Called Functions: 
* Parameters: void 
* Return Value: void 
* Algorithm:
*****************************************************/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

/*****************************************************
* Function Name: gen_incode()
* Author: Patrick Bobyn
* History/Version: November 6 1.0
* Called Functions: 
* Parameters: char * message
* Return Value: void 
* Algorithm:
*****************************************************/
void gen_incode(char* message) {
	printf("%s", message);
}

/*****************************************************
* Function Name: program()
* Author: Patrick Bobyn
* Algorithm:
* <program> -> PLATYPUS {<opt_statements>}
* FIRST(<program>) -> {KW_T(PLATYPUS)
*****************************************************/
void program() {
	/* <program> -> PLATYPUS (<opt_statements>) */
	match(KW_T, PLATYPUS); /* PLATYPUS */
	match(LBR_T, NO_ATTR); /* ( */
	opt_statements();	   /* opt_statements */
	match(RBR_T, NO_ATTR); /* ) */
	gen_incode("PLATY: Program parsed\n");
}

/*****************************************************
* Function Name: opt_statements()
* Author: Patrick Bobyn
* Algorithm:
* <opt_statements> -> <statements><statements’>
* FIRST(<opt_statements>) -> { E, AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
*****************************************************/
void opt_statements() {
	/* <opt_statements> -> <statements><statements'> */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:	/* handle AVID_T and SVID_T cases */
		statements();	/* statements, statements' */
		break;
	case KW_T:	/* handle KW_T case */
		/* check for IF, WHILE, READ, WRTIE and in statements_p() */
		if (lookahead.attribute.get_int == IF 
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statements();	/* statements, statements' */
			break;
		}
	default:	/* empty string - optional statements */
		gen_incode("PLATY: Opt_statements parsed\n");
	}
}

/*****************************************************
* Function Name : statements()
* Author : Patrick Bobyn
* Algorithm :
* <statements> -> <statement><statements’>
* FIRST(<statements>) -> {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
*****************************************************/
void statements() {
	
	/* <statements> -> <statement><statements'> */
	statement();	/* statements */
	statements_p();	/* statements' */
}

/*****************************************************
*Function Name : statements_p()
* Author : Patrick Bobyn
* Algorithm :
* <statements’> -> <statement> <statements’> | E 
* FIRST(<statements’>) -> { E, AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
*****************************************************/
void statements_p() {

	/* <statements'> -> <statement><statements'> | E */
	switch (lookahead.code) {
	case KW_T:	/* KW_T for PLAYTPUS, ELSE, THEN, REPEAT */
		switch (lookahead.attribute.kwt_idx) {
		case PLATYPUS:
		case ELSE:
		case THEN:
		case REPEAT:
			return;
		default:
			break;
		}
	case AVID_T:
	case SVID_T: /* AVID_T, AVID_T cases */
		statement();	/* statements */
		statements_p();	/* statements' */
		break;
	}
}

/*****************************************************
* Function Name : statement()
* Author : Patrick Bobyn
* Algorithm :
* <statement> -> <assignment statement> 
*			   | <selection statement> 
*              | <iteration statement> 
*              | <input statement> 
*              | <output statement>
* FIRST(<statement>) -> {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
*****************************************************/
void statement() {

	/* <statement> -> <assignment> | <selection> | <iteration> | <input> | <output> */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:	/* AVID_T, SVID_T handles only assignment */
		assignment();	/* assignment */
		break;
	case KW_T:	/* KW_T case */
		switch (lookahead.attribute.kwt_idx) {
		case IF:	/* IF case, selection */
			selection();	/* selection */
			break;
		case WHILE:	/* WHILE case, iteraton */
			iteration();	/* iteration */
			break;
		case READ:	/* READ case, input */
			input();	/* input */
			break;
		case WRITE:	/* WRITE case, output */
			output();	/* output */
			break;
		default:	/* default, error */
			syn_printe();
		}
		break;
	default:	/* default, error */
		syn_printe();
	}
}

/*****************************************************
* Function Name : assignment()
* Author : Patrick Bobyn
* Algorithm :
* <assignment statement> -> <assignment expression>;
* FIRST(<assignment statement>) -> {AVID_T, SVID_T}
*****************************************************/
void assignment() {
	
	/* <assignment> -> <assignment expression>; */
	assignment_expression();	/* assignment expression */
	match(EOS_T, NO_ATTR);	/* the ; in the statement */
	gen_incode("PLATY: Assignment statement parsed\n");
}

/*****************************************************
* Function Name : assignment_expression()
* Author : Patrick Bobyn
* Algorithm :
* <assignment expression> -> AVID = <arithmetic expression> 
*							| SVID = <string expression>
* FIRST(<assignment expression>) -> {AVID_T, SVID_T}
*****************************************************/
void assignment_expression() {

	/* <assignment expression> -> AVID_T | SVID_T */
	switch (lookahead.code) {
	case AVID_T:	/* matches AVID_T, match type and then call arithmetic */
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		arithmetic();	/* arithmetic, AVID_T */
		gen_incode("PLATY: Assignment expression (arithmetic) parsed\n");
		break;
	case SVID_T:	/* SVID_T, match type and then call string */
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string();	/* string, SVID_T */
		gen_incode("PLATY: Assignment expression (string) parsed\n");
		break;
	default:	/* default, error */
		syn_printe();
		break;
	}
}

/*****************************************************
* Function Name : selection()
* Author : Patrick Bobyn
* Algorithm :
* <selection statement> ->
*					IF <pre-condition> (<conditional expression>) THEN { <opt_statements> }
*					ELSE { <opt_statements> } ;
* FIRST(<selection statement>) -> {KW_T(IF)}
*****************************************************/
void selection() {

	/* <selection> -> IF <pre-condition> {<conditonal expression>}	 THEN {<opt_statments>}
					  ELSE {<opt_statements>}; */
	match(KW_T, IF);		/* IF */
	pre_condition();
	match(LPR_T, NO_ATTR);	/* ( */
	conditional();			/* conditional expression */
	match(RPR_T, NO_ATTR);	/* ) */
	match(KW_T, THEN);		/* THEN */
	match(LBR_T, NO_ATTR);	/* { */
	opt_statements();		/* opt statements */
	match(RBR_T, NO_ATTR);	/* } */
	match(KW_T, ELSE);		/* ELSE */
	match(LBR_T, NO_ATTR);	/* { */
	opt_statements();		/* opt statements */
	match(RBR_T, NO_ATTR);	/* } */
	match(EOS_T, NO_ATTR);	/* ; */
	gen_incode("PLATY: Selection statement parsed\n");
}

/*****************************************************
* Function Name : iteration()
* Author : Patrick Bobyn
* Algorithm :
* <iteration statement> -> 
*					WHILE <pre-condition> (<conditional expression>) 	
*					REPEAT { <statements>}; 
* FIRST(<iteration statement>) -> {KW_T(WHILE)}
*****************************************************/
void iteration() {

	/* <iteration> -> WHILE <pre-condition> (<conditional>)
					  REPEAT { <statements> }; */
	match(KW_T, WHILE);		/* WHILE */
	pre_condition();
	match(LPR_T, NO_ATTR);	/* ( */
	conditional();			/* conditional */
	match(RPR_T, NO_ATTR);	/* ) */
	match(KW_T, REPEAT);	/* REPEAT */
	match(LBR_T, NO_ATTR);	/* { */
	statements();			/* statements */
	match(RBR_T, NO_ATTR);	/* } */
	match(EOS_T, NO_ATTR);	/* ; */
	gen_incode("PLATY: Iteration statement parsed\n");
}

/*****************************************************
* Function Name : pre_condition()
* Author : Patrick Bobyn
* Algorithm :
* <pre-condition> -> 
*				TRUE | FALSE
* FIRST(<pre-condition>) -> {KW_T(TRUE), KW_T(FALSE)}
*****************************************************/
void pre_condition() {

	/* <pre condition> -> TRUE | FALSE */
	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.kwt_idx) {
		case TRUE:		/* TRUE */
			match(KW_T, TRUE);
			break;
		case FALSE:		/* FALSE */
			match(KW_T, FALSE);
			break;
		default:
			syn_printe();
		}
		break;
	default:
		syn_printe();
	}
}

/*****************************************************
* Function Name : input()
* Author : Patrick Bobyn
* Algorithm :
* <input statement> ->
*				READ (<variable list>);
* FIRST(<input statement>) -> {KW_T(READ)}
*****************************************************/
void input() {
	
	/* <input> -> READ(<variable list>); */
	match(KW_T, READ);		/* READ */
	match(LPR_T, NO_ATTR);	/* ( */
	variable_list();		/* variable list */
	match(RPR_T, NO_ATTR);	/* ) */
	match(EOS_T, NO_ATTR);	/* ; */
	gen_incode("PLATY: Input statement parsed\n");
}

/*****************************************************
* Function Name : variable_list()
* Author : Patrick Bobyn
* Algorithm :
* <variable list> -> <variable identifier><variable list’>
* FIRST(<variable list>) -> {AVID_T, SVID_T, COM_T, E }
*****************************************************/
void variable_list() {
	
	/* <variable list> -> <variable identifier><variable list'> */
	variable_identifier(); /* variable identifier */
	variable_list_p();	/* variable list' */
	gen_incode("PLATY: Variable list parsed\n");
}

/*****************************************************
* Function Name : variable_list_p()
* Author : Patrick Bobyn
* Algorithm :
* <variable list’> -> ,<variable identifier> <variable list’> | E
* FIRST(<variable list’> -> { E , SVID_T, AVID_T}
*****************************************************/
void variable_list_p() {
	
	/* <variable list'> -> ,<variable identifier><variable list'> | E*/
	switch (lookahead.code) {
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
	default:	/* E */
		break;
	}
}

/*****************************************************
* Function Name : variable_identifier()
* Author : Patrick Bobyn
* Algorithm :
* <variable identifier> -> SVID_T | AVID_T
* FIRST(<variable identifier>) -> {SVID_T, AVID_T}
*****************************************************/
void variable_identifier() {

	/* <variable identifier> -> SVID_T | AVID_T */
	switch (lookahead.code) {
	case AVID_T:	/* AVID_T */
		match(AVID_T, NO_ATTR);
		break;
	case SVID_T:	/* SVID_T */
		match(SVID_T, NO_ATTR);
		break;
	default:		/* ERROR */
		syn_printe();
		break;
	}
}

/*****************************************************
* Function Name : output()
* Author : Patrick Bobyn
* Algorithm :
* <output statement> -> WRITE(<output list>);
* FIRST(<output statement) -> {KW_T(WRITE)}
*****************************************************/
void output() {

	/* <output> -> WRITE(<output list>); */
	match(KW_T, WRITE);		/* WRITE */
	match(LPR_T, NO_ATTR);	/* ( */
	output_list();			/* output list */
	match(RPR_T, NO_ATTR);	/* ) */
	match(EOS_T, NO_ATTR);	/* ; */
	gen_incode("PLATY: Output statement parsed\n");
}

/*****************************************************
* Function Name : output_list()
* Author : Patrick Bobyn
* Algorithm :
* <output list> -> <opt_variable_list> | STR_T | E
* FIRST(<output list>) -> { E, AVID_T, SVID_T, STR_T}
*****************************************************/
void output_list() {

	/* <output list> -> AVID_T, SVID_T, STR_T, E */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:	/* AVID_T, SVID_T */
		variable_list();	/* variable list */
		break;
	case STR_T:		/* STR_T */
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed\n");
		break;
	default:		/* E */
		gen_incode("PLATY: Output list (empty) parsed\n");
		break;
	}
}

/*****************************************************
* Function Name : arithmetic()
* Author : Patrick Bobyn
* Algorithm :
* <arithmetic expression> - > 
*						<unary arithmetic expression> 
*					  | <additive arithmetic expression> 
* FIRST(<arithmetic expression>) -> {ART_OP_T(PLUS), ART_OP_T(MINUS), AVID_T, FPL_T, INL_T}
*****************************************************/
void arithmetic() {

	/* <arithmetic> -> <unary arithmetic> 
					 | <additive arithmetic>*/
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.code) {
		case ART_OP_T:	/* ART_OP_T */
			switch (lookahead.attribute.arr_op) {
			case MULT:	/* MULT */
			case DIV:	/* DIV */
				multiplicative_arithmetic_p();
				additive_arithmetic_p();
				return;
			default:
				break;
			}
			unary_arithmetic();	/* unary arithmetic */
			break;
		}
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic();	/* additive arithmetic */
		break;
	default:	/* ERROR */
		syn_printe();	/* print error */
		return;
	}
	gen_incode("PLATY: Arithmetic expression parsed\n");
}

/*****************************************************
* Function Name : unary_arithmetic()
* Author : Patrick Bobyn
* Algorithm :
* <unary arithmetic expression> -> 
* 							- <primary arithmetic expression> 
*						  | + <primary arithmetic expression> 
* FIRST(<unary arithmetic expression>) -> {ART_OP_T(PLUS), ART_OP_T(MINUS)}
*****************************************************/
void unary_arithmetic() {
	
	/* <unary arithmetic> -> -<primary arithemtic> 
						   | +<primary arithmetic> */
	switch (lookahead.code) {
	case ART_OP_T:	/* ART_OP_T */
		match(lookahead.code, lookahead.attribute.arr_op);
		primary_arithmetic();	/* primary arithmetic */
		gen_incode("PLATY: Unary arithmetic expression parsed\n");
		break;
	default:	/* ERROR */
		syn_printe();
		return;
	}
}

/*****************************************************
* Function Name : additive_arithmetic()
* Author : Patrick Bobyn
* Algorithm :
* <additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expression’> 
* FIRST(<additive arithmetic expression>) -> {AVID_T, FPL_T, INL_T, LPR_T}
*****************************************************/
void additive_arithmetic() {
	
	/* <additive arithmetic> -> <multiplicative arithmetic><additive arithmetic'> */
	multiplicative_arithmetic();	/* multiplicative arithmetic */
	additive_arithmetic_p();		/* additive arithmetic' */
}

/*****************************************************
* Function Name : additive_arithmetic_p()
* Author : Patrick Bobyn
* Algorithm :
* <additive arithmetic expression’> -> 
*					  + <multiplicative arithmetic expression><additive arithmetic expressions’>
*					| - <multiplicative arithmetic expression><additive arithmetic expressions’>
*					| E
* FIRST(<additive arithmetic expression’> -> { E, ART_OP_T(PLUS), ART_OP_T(MINUS)}
*****************************************************/
void additive_arithmetic_p() {
	
	/* <additive arithmetic'> -> + <multiplicative arithmetic><additive arithmetic'> 
							   | - <multiplicative arithmetic><additive arithmetic'>
							   | E */
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
			match(ART_OP_T, PLUS);			/* + */
			multiplicative_arithmetic();	/* multiplicative */
			additive_arithmetic_p();		/* additive arithmetic' */
			break;
		case MINUS:
			match(ART_OP_T, MINUS);			/* - */
			multiplicative_arithmetic();	/* multiplicative */
			additive_arithmetic_p();		/* additive arithmetic' */
			break;
		default:
			return;
		}
		break;
	default:								/* E */
		return;
	}
	gen_incode("PLATY: Additive arithmetic expression parsed\n");
}

/*****************************************************
* Function Name : multiplicative_arithmetic()
* Author : Patrick Bobyn
* Algorithm : 
* <multiplicative arithmetic expression> -> 
*				<primary arithmetic expression><multiplicative arithmetic expression’>
* FIRST(<multiplicative arithmetic expression>) -> {AVID_T, FPL_T, INL_T, LPR_T}
*****************************************************/
void multiplicative_arithmetic() {

	/* <multiplicative arithemtic> -> <priamary arithmetic><multiplicative arithmetic'> */
	primary_arithmetic();	/* primary arithmetic */
	multiplicative_arithmetic_p();	/* muliplicative arithmetic' */
}

/*****************************************************
* Function Name : multiplicative_arithmetic_p()
* Author : Patrick Bobyn
* Algorithm :
* <multiplicative arithmetic expression’> ->
*		 	   * <primary arithmetic expression><multiplicative arithmetic expression’>
*		   	 | / <primary arithmetic expression><multiplicative arithmetic expression’>
*			 | E
* FIRST(<multiplicative arithmetic expression’>) -> { E, ART_OP_T(DIV), ART_OP_T(MULT)}
*****************************************************/
void multiplicative_arithmetic_p() {
	
	/* <multiplicative arithmetic'> -> * <priamry arithmetic><multiplicative arithmetic'> 
								     | / <priamry arithmetic><multiplicative arithmetic'>
									 | E */
	switch (lookahead.code) {
	case ART_OP_T:			/* ART_OP_T */
		switch (lookahead.attribute.arr_op) {
		case DIV:
			match(ART_OP_T, DIV);	/* / */
			primary_arithmetic();	/* primary arithmetic */
			multiplicative_arithmetic_p();	/* multiplicative arithmetic */
			break;
		case MULT:
			match(ART_OP_T, MULT);	/* * */
			primary_arithmetic();	/* primary arithmetic */
			multiplicative_arithmetic_p();	/* multiplicative arithmetic */
			break;
		default:
			return;
		}
		break;
	default:		/* E */
		return;
	}
	gen_incode("PLATY: Multiplicative arithmetic expression parsed\n");
}

/*****************************************************
* Function Name : primary_arithmetic()
* Author : Patrick Bobyn
* Algorithm : 
* <primary arithmetic expression> -> 
*			AVID_T 
*		  | FPL_T 
*		  | INL_T 
*		  | (<arithmetic expression>)
* FIRST(<primary arithmetic expression> -> {AVID_T, FPL_T, INL_T, LPR_T}
*****************************************************/
void primary_arithmetic() {

	/* <primary arithmetic> -> AVID_T | FPL_T | INL_T | LPR_T */
	switch (lookahead.code) {
	case AVID_T:		/* AVID_T */
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:			/* FPL_T */
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:			/* INL_T */
		match(INL_T, NO_ATTR);
		break;
	case LPR_T:			/* LPR_T */
		match(LPR_T, NO_ATTR);
		arithmetic();	/* arithmetic */
		match(RPR_T, NO_ATTR);
		break;
	default:	/* ERROR  */
		syn_printe();
		return;
	}

	gen_incode("PLATY: Primary arithmetic expression parsed\n");
}

/*****************************************************
*Function Name : string()
* Author : Patrick Bobyn
* Algorithm :
* <string expression> -> <primary string expression> <string expression’>
* FIRST(<string expression>) -> {STR_T, SVID_T}
*****************************************************/
void string() {
	
	/* <string> -> <primary string><string'> */
	primary_string();	/* primary string */
	string_p();			/* string' */
}

/*****************************************************
*Function Name : string_p()
* Author : Patrick Bobyn
* Algorithm :
* <string expression’> -> << <primary string expression><string expression’> | E
* FIRST(<string expression’> -> { E, SCC_OP_T}
*****************************************************/
void string_p() {

	/* <string'> -> << <primary string><string'> | E */
	switch (lookahead.code) {
	case SCC_OP_T:				/* << */
		match(SCC_OP_T, NO_ATTR);
		primary_string();		/* primary string */
		string_p();				/* string' */
		break;
	default:					/* E */
		gen_incode("PLATY: String expression parsed\n");
	}
}

/*****************************************************
* Function Name : primary_string()
* Author : Patrick Bobyn
* Algorithm :
*  <primary string expression> -> 
*					SVID_T 
*				  | STR_T
* FIRST(<primary string expression>) -> {SVID_T, STR_T}
*****************************************************/
void primary_string() {

	/* <primary string> -> SVID_T | STR_T */
	switch (lookahead.code) {
	case SVID_T:		/* SVID_T */
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:			/* STR_T */
		match(STR_T, NO_ATTR);
		break;
	default:			/* ERROR */
		syn_printe();
		return;
	}
	gen_incode("PLATY: Primary string expression parsed\n");
}

/*****************************************************
*Function Name : conditional()
* Author : Patrick Bobyn
* Algorithm : 
* <conditional expression> -> 
			<logical OR expression> 
* FIRST(<conditional expression>) -> {STR_T, SVID_T, AVID_T, FPL_T, INL_T}
*****************************************************/
void conditional() {

	/* <conditional> -> <logical OR> */
	logical_OR();	/* logical OR */
	gen_incode("PLATY: Conditional expression parsed\n");
}

/*****************************************************
*Function Name : logical_OR()
* Author : Patrick Bobyn
* Algorithm :
* <logical OR expression> -> <logical AND expression><logical OR expression’>
* FIRST(<logical OR expression>) -> {STR_T, SVID_T, AVID_T, FPL_T, INL_T}
*****************************************************/
void logical_OR() {
	
	/* <logical OR> -> <logical AND><logical OR'> */
	logical_AND();	/* logical AND */
	logical_OR_p();	/* logical OR' */
}

/*****************************************************
*Function Name : logical_OR_p()
* Author : Patrick Bobyn
* Algorithm :
* <logical OR expression’> -> .OR. <logical AND expression><logical OR expression> | E
* FIRST(<logical OR expression’>) -> { E, LOG_OP_T(OR)}
*****************************************************/
void logical_OR_p() {
	
	/* <logical OR'> -> .OR.<logical AND><logical OR'> | E */
	switch (lookahead.code) {
	case LOG_OP_T:	/* LOG_OP_T */
		switch (lookahead.attribute.log_op) {
		case AND:	/* AND returns */
			return;
		default:
			break;
		}
		match(LOG_OP_T, OR);
		logical_AND();	/* logical AND */
		logical_OR_p();	/* logical OR' */
		gen_incode("PLATY: Logical OR expression parsed\n");
	default:
		break;
	}
}

/*****************************************************
*Function Name : logical_AND()
* Author : Patrick Bobyn
* Algorithm :
* <logical AND expression> -> <relational expression><logical AND expression’>
* FIRST(<logical AND expression>) -> {STR_T, SVID_T, AVID_T, FPL_T, INL_T}
*****************************************************/
void logical_AND() {

	/* <logical AND> -> <relational><logical AND'> */
	relational();	/* relational */
	logical_AND_p();	/* logical AND' */
}

/*****************************************************
*Function Name : logical_AND_p()
* Author : Patrick Bobyn
* Algorithm :
* <logical AND expression’> -> .AND. <relational expression><logical AND expression’ | E
* FIRST(<logical AND expression’>) -> { E, LOG_OP_T(AND)}
*****************************************************/
void logical_AND_p() {
	
	/* <logical AND'> -> .AND.<relational><logical AND> | E */
	switch (lookahead.code) {
	case LOG_OP_T:	/* LOG_OP_T */
		switch (lookahead.attribute.log_op) {
		case OR:	/* OR returns */
			return;
		default:
			break;
		}
		match(LOG_OP_T, AND);
		relational();	/* relational */
		logical_AND_p();	/* logical AND' */
		gen_incode("PLATY: Logical AND expression parsed\n");
		break;
	default:		/* E */
		break;
	}
}

/*****************************************************
*Function Name : relational()
* Author : Patrick Bobyn
* Algorithm :
* <relational expression> -> 
*			  <primary a_relational expression> <primary a_relational expression’>
*		 	| <primary s_relational expression><primary s_relational expression’>
* FIRST(<relational expression>) -> {STR_T, SVID_T, AVID_T, FPL_T, INL_T}
*****************************************************/
void relational() {
	
	/* <relational> -> <primary a_relational><primary a_relational'> 
					 | <primary s_relational<primary s_relational'> */
 	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:	/* AVID_T, FPL_T, INL_T -> a_relational */
		primary_a_relational();	/* primary a relational */
		primary_a_relational_p();	/* primary a_relational' */
		break;
	case STR_T:
	case SVID_T:	/* STR_T, SVID_T -> s_relational */
		primary_s_relational();	/* primary s_relational */
		primary_s_relational_p();	/* primary s_relational' */
		break;
	default:		/* ERROR */
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed\n");
}

/*****************************************************
*Function Name : primary_a_relational_p()
* Author : Patrick Bobyn
* Algorithm :
* <primary a_relational expression’> ->
*				  == <primary a_relational expression>
*			   	| <> <primary a_relational expression>
*				| > <primary a_relational expression>
*				| < <primary a_relational expression>
* FIRST(<primary a_relational expression’>) -> {REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(LT), REL_OP_T(GT)}
*****************************************************/
void primary_a_relational_p() {

	/* <primary a_relational'> -> == <primary a_relational>
								| <> <primary a_relational>
								| > <primary a_relational>
								| < <primary a_relational> */
	switch (lookahead.code) {
	case REL_OP_T:	/* REL_OP_T */
		switch (lookahead.attribute.rel_op) {
		case EQ:	/* == */
			match(REL_OP_T, EQ);
			primary_a_relational();	/* primary a_relational */
			break;
		case NE:	/* <> */
			match(REL_OP_T, NE);
			primary_a_relational();	/* primary a_relational */
			break;
		case LT:	/* < */
			match(REL_OP_T, LT);
			primary_a_relational(); /* primary a_relational */
			break;
		case GT:	/* > */
			match(REL_OP_T, GT);
			primary_a_relational(); /* primary a_relational */
			break;
		default:				/* ERROR */
			syn_printe();
		}
		break;
	default:	/* ERROR */
		syn_printe();
	}
}

/*****************************************************
*Function Name : primary_s_relational_p()
* Author : Patrick Bobyn
* Algorithm :
* <primary s_relational expression’> ->
*		   == <primary s_relational expression> 
*		 | <> <primary s_relational expression> 
*		 | > <primary s_relational expression> 
*		 | < <primary s_relational expression>
* FIRST(<primary s_relational expression’>) -> {REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(LT), REL_OP_T(GT)}
*****************************************************/
void primary_s_relational_p() {

	/* <primary s_relational'> -> == <primary s_relational>
								| <> <primary s_relational>
								| > <primary s_relational>
								| < <primary s_relational> */
	switch (lookahead.code) {
	case REL_OP_T:
		switch (lookahead.attribute.rel_op) {
		case EQ:	/* == */
			match(REL_OP_T, EQ);
			primary_s_relational();		/* <primary s_relational> */
			break;
		case NE:	/* <> */
			match(REL_OP_T, NE);
			primary_s_relational();		/* <primary s_relational> */
			break;
		case LT:	/* > */
			match(REL_OP_T, LT);	
			primary_s_relational();		/* <primary s_relational> */
			break;
		case GT:	/* < */
			match(REL_OP_T, GT);
			primary_s_relational();		/* <primary s_relational> */
			break;
		default:	/* ERROR */
			syn_printe();
		}
		break;
	default:	/* ERROR */
		syn_printe();
	}
}

/*****************************************************
*Function Name : primary_a_relational()
* Author : Patrick Bobyn
* Algorithm :
* <primary a_relational expression> -> 
*			AVID_T 	
*		  | FPL_T 
*		  | INL_T 
* FIRST(primary a_relational expression> -> {AVID_T, FPL_T, INL_T}
*****************************************************/
void primary_a_relational() {

	/* <primary a_relational> -> AVID_T | FPL_T | INL_T */
	switch (lookahead.code) {
	case AVID_T:	/* AVID_T */
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:		/* FPL_T */
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:		/* INL_T */
		match(INL_T, NO_ATTR);
		break;
	default:		/* ERROR */
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed\n");
}

/*****************************************************
*Function Name : primary_s_relational()
* Author : Patrick Bobyn
* Algorithm :
* <primary s_relational expression> -> 
*			<primary string expression>
* FIRST(primary s_relational expression>) -> {STR_T, SVID_T}
*****************************************************/
void primary_s_relational() {

	/* <primary s_relational> -> STR_T, SVID_T */
	switch (lookahead.code) {
	case SVID_T:	/* SVID_T */
	case STR_T:		/* STR_T */
		break;
	default:		/* ERROR */
		syn_printe();
	}
	primary_string();
	gen_incode("PLATY: Primary s_relational expression parsed\n");
}