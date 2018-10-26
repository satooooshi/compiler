#include "prog1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Macros
#define MAX(a, b) (a > b ? a : b)

// Struct definitions
typedef struct table_ *table;
typedef struct intAndtable_ *intAndTable;
struct table_ {string id; int value; table tail;};
struct intAndtable_ {int i; table t;};

//prototypes
int interpOp(int a, int b, A_binop op);
table interpStm(A_stm s, table t);
intAndTable interpExp(A_exp e, table t);
int lookup(table t, string key);
void interp(A_stm stm);

// Constructors
table Table(string id, int value, table tail)
{
	table t = checked_malloc(sizeof(*t));
	t->id=id; t->value=value; t->tail=tail;
	return t;
}

intAndTable IntAndTable(int i, table t)
{
	intAndTable it = checked_malloc(sizeof(*it));
	it->i=i; it->t=t;
	return it;
}

// Function definitions
int maxargs(A_stm stm);
int maxargsExp(A_exp exp);

// Maxargs
int maxargsExp(A_exp exp)
{
	switch (exp->kind) {
		case A_idExp:
		case A_numExp:
			return 0;
		case A_opExp:
			return MAX(maxargsExp(exp->u.op.left),
						maxargsExp(exp->u.op.right));
		case A_eseqExp:
			return MAX(maxargs(exp->u.eseq.stm),
						maxargsExp(exp->u.eseq.exp));
		default:
			fprintf(stderr, "Unknown expression kind\n");
			exit(1);
	}
}

int maxargs(A_stm stm)
{
	//TODO: put your code here.

	// Assign Stm
	if (stm->kind == A_assignStm)
		return maxargsExp(stm->u.assign.exp);

	// Compound Stm
	if (stm->kind == A_compoundStm)
		return MAX(maxargs(stm->u.compound.stm1),
					maxargs(stm->u.compound.stm2));

	// Print Stm ({A_expList exps;} print)
	//when exps.kind == A_pairExpList then has two arguments
	//else if exps.kind == A_LastExpList then has one argument
	int list_argc 	= 0;
	int exp_argc 	= 0;
	A_expList list 	= stm->u.print.exps;

	// Iterate pair list
	///when exps.kind == A_pairExpList
	while (list->kind == A_pairExpList) {
		++list_argc;
		exp_argc 	= MAX(exp_argc, maxargsExp(list->u.pair.head));
		list 		= list->u.pair.tail;
	}

	// Handle last list
	//when exps.kind == A_LastExpList
	++list_argc;
	exp_argc = MAX(exp_argc, maxargsExp(list->u.last));

	return MAX(list_argc, exp_argc);
	


	//return 0;
}

table interpPrintExpList(A_expList l, table t)
{
	if (l->kind == A_lastExpList) {
		intAndTable it = interpExp(l->u.last, t);
		printf("%d\n", it->i);
		return it->t;
	}

	// If PairExpList
	intAndTable it = interpExp(l->u.pair.head, t);
	printf("%d ", it->i);

	// Print recursively
	return interpPrintExpList(l->u.pair.tail, t);
}


// Interp
int interpOp(int a, int b, A_binop op)
{
	switch (op) {
		case A_plus:	return a + b;
		case A_minus:	return a - b;
		case A_times:	return a * b;
		case A_div:
			// Check zero division
			if (b == 0) {
				fprintf(stderr, "Cannot divide by zero\n");
				exit(1);
			}
			return a / b;
		default:
			fprintf(stderr, "Unknown operator\n");
			exit(1);
	}
}


table interpStm(A_stm s, table t)
{
	switch (s->kind) {
		case A_compoundStm:
			return interpStm(s->u.compound.stm2,
						interpStm(s->u.compound.stm1, t));
		case A_assignStm:
		{
			intAndTable it = interpExp(s->u.assign.exp, t);
			return Table(s->u.assign.id, it->i, it->t);
		}
		case A_printStm:
			return interpPrintExpList(s->u.print.exps, t);
		default:
			fprintf(stderr, "Unknown statement kind\n");
			exit(1);
	}
}

intAndTable interpExp(A_exp e, table t)
{
	switch (e->kind) {
		case A_idExp:
			return IntAndTable(lookup(t, e->u.id), t);
		case A_numExp:
			return IntAndTable(e->u.num, t);
		case A_opExp:
		{
			intAndTable it1 = interpExp(e->u.op.left, t);
			intAndTable it2 = interpExp(e->u.op.right, it1->t);
			return IntAndTable(interpOp(it1->i, it2->i, e->u.op.oper), it2->t);
		}
		case A_eseqExp:
			return interpExp(e->u.eseq.exp,
						interpStm(e->u.eseq.stm, t));
		default:
			fprintf(stderr, "Unknown expression kind\n");
			exit(1);
	}
}



void interp(A_stm stm)
{
	//TODO: put your code here.

	// Begin with an empty table
	interpStm(stm, NULL);

	
}

int lookup(table t, string key)
{
	// If found
	if (strcmp(key, t->id) == 0) return t->value;

	// If no tail exists
	if (!t->tail) {
		fprintf(stderr, "Key not found: %s\n", key);
		exit(1);
	}

	// Find element recursively
	return lookup(t->tail, key);
}