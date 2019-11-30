#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

#include "parcer.h"

void parser() {
	lookahead = malar_next_token();
	program(); 
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed\n");
}

void match(int pr_token_code, int pr_token_attribute) {

	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code);
		return;
	}

	if (lookahead.code == SEOF_T) {
		return;
	}

	switch (pr_token_code) {
	case KW_T:
		if (pr_token_attribute != lookahead.attribute.kwt_idx) {
			syn_eh(pr_token_code);
			return;
		}
		break;
	case LOG_OP_T:
		if (pr_token_attribute != lookahead.attribute.log_op) {
			syn_eh(pr_token_code);
			return;
		}
		break;
	case ART_OP_T:
		if (pr_token_attribute != lookahead.attribute.arr_op) {
			syn_eh(pr_token_code);
			return;
		}
		break;
	case REL_OP_T:
		if (pr_token_attribute != lookahead.attribute.rel_op) {
			syn_eh(pr_token_code);
			return;
		}
		break;
	default:
		break;
	}

	lookahead = malar_next_token();
	
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}
}

void syn_eh(int sync_token_code) {

	syn_printe();
	synerrno++;

	do {
		lookahead = malar_next_token();

		if (sync_token_code == lookahead.code) {
			if (sync_token_code != SEOF_T)
				lookahead = malar_next_token();
			return;
		}

		if (lookahead.code == SEOF_T) {
			exit(synerrno);
			return;
		}

	} while (sync_token_code != lookahead.code);
}

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

void gen_incode(char* message) {
	printf("%s", message);
}

void program() {
	/* <program> -> PLATYPUS (<opt_statements>) */
	match(KW_T, PLATYPUS); /* PLATYPUS */
	match(LBR_T, NO_ATTR); /* ( */
	opt_statements();	   /* opt_statements */
	match(RBR_T, NO_ATTR); /* ) */
	gen_incode("PLATY: Program parsed\n");
}

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

void statements() {
	
	/* <statements> -> <statement><statements'> */
	statement();	/* statements */
	statements_p();	/* statements' */
}

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

void assignment() {
	
	/* <assignment> -> <assignment expression>; */
	assignment_expression();	/* assignment expression */
	match(EOS_T, NO_ATTR);	/* the ; in the statement */
	gen_incode("PLATY: Assignment statement parsed\n");
}

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

void pre_condition() {

	/* <pre condition> -> TRUE | FALSE */
	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.kwt_idx) {
		case TRUE:
			match(KW_T, TRUE);
			break;
		case FALSE:
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

void input() {
	
	/* <input> -> READ(<variable list>); */
	match(KW_T, READ);		/* READ */
	match(LPR_T, NO_ATTR);	/* ( */
	variable_list();		/* variable list */
	match(RPR_T, NO_ATTR);	/* ) */
	match(EOS_T, NO_ATTR);	/* ; */
	gen_incode("PLATY: Input statement parsed\n");
}

void variable_list() {
	
	/* <variable list> -> <variable identifier><variable list'> */
	variable_identifier(); /* variable identifier */
	variable_list_p();	/* variable list' */
	gen_incode("PLATY: Variable list parsed\n");
}

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

void output() {

	/* <output> -> WRITE(<output list>); */
	match(KW_T, WRITE);		/* WRITE */
	match(LPR_T, NO_ATTR);	/* ( */
	output_list();			/* output list */
	match(RPR_T, NO_ATTR);	/* ) */
	match(EOS_T, NO_ATTR);	/* ; */
	gen_incode("PLATY: Output statement parsed\n");
}

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

void additive_arithmetic() {
	
	/* <additive arithmetic> -> <multiplicative arithmetic><additive arithmetic'> */
	multiplicative_arithmetic();	/* multiplicative arithmetic */
	additive_arithmetic_p();		/* additive arithmetic' */
}

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

void multiplicative_arithmetic() {

	/* <multiplicative arithemtic> -> <priamary arithmetic><multiplicative arithmetic'> */
	primary_arithmetic();	/* primary arithmetic */
	multiplicative_arithmetic_p();	/* muliplicative arithmetic' */
}

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

void string() {
	
	/* <string> -> <primary string><string'> */
	primary_string();	/* primary string */
	string_p();			/* string' */
}

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

void conditional() {

	/* <conditional> -> <logical OR> */
	logical_OR();	/* logical OR */
	gen_incode("PLATY: Conditional expression parsed\n");
}

void logical_OR() {
	
	/* <logical OR> -> <logical AND><logical OR'> */
	logical_AND();	/* logical AND */
	logical_OR_p();	/* logical OR' */
}

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

void logical_AND() {

	/* <logical AND> -> <relational><logical AND'> */
	relational();	/* relational */
	logical_AND_p();	/* logical AND' */
}

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
