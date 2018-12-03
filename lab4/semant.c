#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"
#include "types.h"
#include "helper.h"
#include "env.h"
#include "semant.h"

/*Lab4: Your implementation of lab4*/

#define DEBUG 0
#define debug_print(entry,s) if (DEBUG) printf("entry %s:Entered %s\n", entry, s)


typedef void* Tr_exp;
struct expty 
{
	Tr_exp exp; 
	Ty_ty ty;
};

//In Lab4, the first argument exp should always be **NULL**.
struct expty expTy(Tr_exp exp, Ty_ty ty)
{
	struct expty e;

	e.exp = exp;
	e.ty = ty;

	return e;
}

void SEM_transProg(A_exp exp){
/*
	S_table venv=S_empty();
	S_table tenv=S_empty();
	
	if(exp->kind==A_letExp){
		printf("\nImLetExp\n");
		transExp(venv,tenv,exp);
		return ;
	}else{
		printf("\nImNotLetExp\n");
		return ;
	}
	return ;
	*/
  S_table venv = E_base_venv();
  S_table tenv = E_base_tenv();
  transExp(venv, tenv, exp);
}



/* Utilities */
static Ty_ty actual_ty(Ty_ty ty) {
  Ty_ty actual = ty;
  while (actual && actual->kind == Ty_name) {
    actual = actual->u.name.ty;
    /* Return if a cycle is detected */
    if (actual == ty)
      break;
  }
  return actual;
}

/* Compare types strictly */
static int compare_ty(Ty_ty ty1, Ty_ty ty2) {
  ty1 = actual_ty(ty1);
  ty2 = actual_ty(ty2);
  if (ty1->kind == Ty_int || ty1->kind == Ty_string)
    return ty1->kind == ty2->kind;
  
  if (ty1->kind == Ty_nil && ty2->kind == Ty_nil)
    return FALSE;
  
  if (ty1->kind == Ty_nil)
    return ty2->kind == Ty_record || ty2->kind == Ty_array;
  
  if (ty2->kind == Ty_nil)
    return ty1->kind == Ty_record || ty1->kind == Ty_array;
  
  return ty1 == ty2;
}


/* Protected symbol for loop variable checking */
S_symbol protect_sym(S_symbol s) {
  char protected_sym[256];
  sprintf(protected_sym, "<%s>", S_name(s));
  return S_Symbol(String(protected_sym));
}


void showExpKind(A_exp a){
	switch(a->kind){
		case A_varExp:{return ;}
		default:return;
	}
}




struct expty transExp(S_table venv, S_table tenv, A_exp a){

	showExpKind(a);

	/*
	struct A_exp_
	{
	union 
	{
		A_var var;
		int intt;
		string stringg;
		struct {S_symbol func; A_expList args;} call;
		struct {A_oper oper; A_exp left; A_exp right;} op;
		struct {S_symbol typ; A_efieldList fields;} record;
		A_expList seq;
		struct {A_var var; A_exp exp;} assign;
		struct {A_exp test, then, elsee;} iff;
		struct {A_exp test, body;} whilee;
		struct {S_symbol var; A_exp lo,hi,body; bool escape;} forr;
		struct {A_decList decs; A_exp body;} let;
		struct {S_symbol typ; A_exp size, init;} array;
	} u;
	};
	*/
	switch(a->kind) {
	
	/*
		struct A_var_
		{
			enum {A_simpleVar, A_fieldVar, A_subscriptVar} kind;
    		A_pos pos;
			union 
			{
				S_symbol simple;
				struct {A_var var; S_symbol sym;} field;
	    		struct {A_var var; A_exp exp;} subscript;
			} u;
		};
		var rec1:rectype := rectype {name="Nobody", age=1000}

	*/
	case A_varExp: {
		//
		//A_var var;
		A_var var = a->u.var;
		struct expty expty_var = transVar(venv, tenv, var);

		return expTy(NULL, expty_var.ty);
	}
	/*
		do_nothing2(a+1)
	*/
	case A_callExp: {
		//
		//struct {S_symbol func; A_expList args;} call;
		S_symbol func = a->u.call.func;
		A_expList args = a->u.call.args;

		//check function already defined using E_enventry
		/*
		struct E_enventry_ {
			enum {E_varEntry, E_funEntry} kind;
			union 
			{
				struct {Ty_ty ty;} var;
				struct {Ty_tyList formals; Ty_ty result;} fun;
			} u;
		};
		*/
      	E_enventry entry_func = S_look(venv, func);
      	if (entry_func == NULL) {
        	EM_error(a->pos, "undefined function %s", S_name(func));
        	//return expTy(NULL, Ty_Void());
      	}

      	if (entry_func->kind != E_funEntry) {
        	EM_error(a->pos, "%s is not a function", S_name(func));
        	//return expTy(NULL, Ty_Void());
    	}

		// Compare param type
      	Ty_tyList tl;
      	A_expList el;//struct A_expList_ {A_exp head; A_expList tail;};
      	struct expty expty_arg;
      	for (tl = entry_func->u.fun.formals, el = args;
        	tl && el; tl = tl->tail, el = el->tail) {	
        	expty_arg = transExp(venv, tenv, el->head);
        	if (!compare_ty(tl->head, expty_arg.ty)) {
          		EM_error(el->head->pos, "para type mismatch");
          		break;
        	}
      	}

		if (el){
			EM_error(a->pos, "too many params in function %s", S_name(func));
	  	}
       
      	if (tl){
			EM_error(a->pos, "too few params in function %s", S_name(func));
	  	}
      
      return expTy(NULL, entry_func->u.fun.result);//returning TYPE of the expression
	}
	/*
		10 > 20
	*/
	case A_opExp: {
		//
		//struct {A_oper oper; A_exp left; A_exp right;} op;
		A_oper oper = a->u.op.oper;
		A_exp left = a->u.op.left;
		A_exp right = a->u.op.right;

		

		struct expty expty_left = transExp(venv, tenv, left);
		struct expty expty_right = transExp(venv, tenv, right);

		switch (oper) {
        	case A_plusOp:
        	case A_minusOp:
        	case A_timesOp:
        	case A_divideOp: {
         		if (expty_left.ty->kind != Ty_int)
           			 EM_error(a->u.op.left->pos, "integer required");
          		if (expty_right.ty->kind != Ty_int)
            		EM_error(a->u.op.right->pos, "integer required");
          			return expTy(NULL, Ty_Int());
    		}
			case A_eqOp:
        	case A_neqOp: {
          		if (!compare_ty(expty_left.ty, expty_right.ty)) {
            		EM_error(right->pos, "same type required");
            		return expTy(NULL, Ty_Int());
          		}
          		return expTy(NULL, Ty_Int());
        	}
        	case A_ltOp:
        	case A_leOp:
        	case A_gtOp:
        	case A_geOp: {
          		Ty_ty ty_ty_left = actual_ty(expty_left.ty);//actual_ty converts expty to Ty_ty
          		Ty_ty ty_ty_right = actual_ty(expty_right.ty);
          		if (ty_ty_left->kind != Ty_int && ty_ty_left->kind != Ty_string)
            		EM_error(a->u.op.left->pos, "string or integer required");
          		if (ty_ty_left->kind == Ty_int) {
            		if (ty_ty_right->kind != Ty_int)
              			EM_error(a->u.op.right->pos, "same type required");
            			return expTy(NULL, Ty_Int());
          		}
         		 if (ty_ty_left->kind == Ty_string) {
            		if (ty_ty_right->kind != Ty_string)
              			EM_error(a->u.op.right->pos, "same type required");
            			return expTy(NULL, Ty_Int());
          		}
          	/* Error recovery */
         	 return expTy(NULL, Ty_Int());
        	}
      }
		
		return expTy(NULL, Ty_Int());
	}
	/*
		Student { name="satoshi", age=25 }
		 "typ"   			"fields"
	*/
	case A_recordExp: {
		//
		//struct {S_symbol typ; A_efieldList fields;} record;
		S_symbol typ = a->u.record.typ;
		A_efieldList fields= a->u.record.fields;

		// Get the record type
      	Ty_ty ty_ty_typ = actual_ty(transTy(tenv, A_NameTy(a->pos, typ)));
      	if (ty_ty_typ->kind != Ty_record) {
        	EM_error(a->pos, "type %s is not record", S_name(typ));
        	return expTy(NULL, Ty_Record(NULL));
      	}

		//Compare fields
      	A_efieldList el;
      	Ty_fieldList fl;
      	struct expty exp;
      	for (el = a->u.record.fields, fl = ty_ty_typ->u.record;
        	el && fl; el = el->tail, fl = fl->tail) {
        	if (strcmp(S_name(el->head->name), S_name(fl->head->name)) != 0) {
          		EM_error(a->pos, "field name should be %s but not %s",
           			 S_name(fl->head->name), S_name(el->head->name));
          	//continue;
        	}
			exp = transExp(venv, tenv, el->head->exp);
        	if (!compare_ty(fl->head->ty, exp.ty))
          		EM_error(el->head->exp->pos, "field type of %s mismatch",
            		S_name(fl->head->name));
      	}

		if (el || fl) {
        	EM_error(a->pos, "fields of type %s mismatch", S_name(a->u.record.typ));
      	}

      return expTy(NULL, ty_ty_typ);
	}
	/*
		( 10 > 20　) //かっこが必要
	*/
	case A_seqExp: {
		//
		//A_expList seq;
		A_expList seq = a->u.seq;
      	struct expty expty_exp = expTy(NULL, Ty_Void());
     	 for (; seq != NULL; seq = seq->tail){
       		 expty_exp = transExp(venv, tenv, seq->head);
		  }
      return expty_exp;
    }
	/*
		for i:=0 to 100 do ("a:=a+1";())
	*/
	case A_assignExp: {
		//
		//struct {A_var var; A_exp exp;} assign;
		A_var var = a->u.assign.var;
		A_exp exp = a->u.assign.exp;

		struct expty expty_var = transVar(venv, tenv, var);
      	struct expty expty_exp = transExp(venv, tenv, exp);
      	if (!compare_ty(expty_var.ty, expty_exp.ty)){
        	EM_error(a->pos, "unmatched assign exp");
	  	}

      //Loop variable detection
      if (var->kind == A_simpleVar &&
        S_look(venv, protect_sym(var->u.simple)) != NULL) {
        EM_error(var->pos, "loop variable can't be assigned");//*********************************************************
      }
	  
      return expTy(NULL, Ty_Void());//assignExp must have void type
	}
	/*
		while(10 > 5) do 5+6
	*/
	case A_whileExp: {
		//
		//struct {A_exp test, body;} whilee;
		A_exp test = a->u.whilee.test;
		A_exp body = a->u.whilee.body;

		struct expty expty_test = transExp(venv, tenv, test);
		

		if( expty_test.ty->kind != Ty_int ){
        	EM_error(test->pos, "expression must be integer");
		}

		S_beginScope(venv);
      	//Symbol for break checking 
      	S_enter(venv, S_Symbol("<loop>"), E_VarEntry(Ty_Int()));
		struct expty expty_body = transExp(venv, tenv, body);
      	S_endScope(venv);

      	if (expty_body.ty->kind != Ty_void){
        	EM_error(body->pos, "while body must produce no value");
		}

      	return expTy(NULL, Ty_Void());//A_whileExp must have void type!!
	}
	/*
		for i:=0 to 100 do (a:=a+1;())
	*/
	case A_forExp: {
		//
		//struct {S_symbol var; A_exp lo,hi,body; bool escape;} forr;
		S_symbol var = a->u.forr.var;
		A_exp lo = a->u.forr.lo;
		A_exp hi = a->u.forr.hi;
		A_exp body = a->u.forr.body;

		if(body->kind==A_assignExp)debug_print("A_forExp","fa");

		S_beginScope(venv);

		struct expty expty_lo=transExp(venv, tenv, lo);
		struct expty expty_hi=transExp(venv, tenv, hi);

		if(expty_lo.ty->kind!=Ty_int || expty_hi.ty->kind!=Ty_int ){
			 EM_error(a->u.forr.lo->pos, "for exp's range type is not integer");
		}

		S_enter(venv, var, E_VarEntry(Ty_Int()));
      	//Symbol for loop variable checking
      	S_enter(venv, protect_sym(var), E_VarEntry(Ty_Int()));//*********************************************************
      	//Symbol for break checking
      	S_enter(venv, S_Symbol("<loop>"), E_VarEntry(Ty_Int()));


		struct expty expty_body=transExp(venv, tenv, body);
		if (expty_body.ty->kind != Ty_void){
        	EM_error(body->pos, "for body must produce no value");
		}


		 S_endScope(venv);
		return expTy(NULL, Ty_Void());//A_forExp must have void type!!
	}
	/*
		if (10 > 20) then 30 else 40
		if (A_opExp) then A_intExp else A_intExp

		//types of then - else must correspond with each other
	*/
case A_ifExp: {
      struct expty test, then, elsee;
      test = transExp(venv, tenv, a->u.iff.test);
      
      if (test.ty->kind != Ty_int)
        EM_error(a->u.iff.test->pos, "expression must be integer");

      then = transExp(venv, tenv, a->u.iff.then);
      if (a->u.iff.elsee != NULL) {           /* if then else */
        elsee = transExp(venv, tenv, a->u.iff.elsee);
        if (!compare_ty(then.ty, elsee.ty)) {
			if (then.ty->kind != Ty_void) {  /* if then */
        		EM_error(a->u.iff.then->pos, "if-then exp's body must produce no value");
			}
          EM_error(a->u.iff.elsee->pos,
            "then exp and else exp type mismatch");
        }
      } else if (then.ty->kind != Ty_void) {  /* if then */
        	EM_error(a->u.iff.then->pos, "if-then exp's body must produce no value");
      }

      return expTy(NULL, then.ty);
    }
	/*
	case A_ifExp: {
		//
		//struct {A_exp test, then, elsee;} iff;
		A_exp test = a->u.iff.test;
		A_exp then = a->u.iff.then;
		A_exp elsee = a->u.iff.elsee;
		
		
		//if(a->u.iff.test->kind==A_seqExp)debug_print("expTy","ifExp");
		struct expty expty_test = transExp(venv, tenv, test);
		struct expty expty_then = transExp(venv, tenv, then);
		struct expty expty_elsee = transExp(venv, tenv, elsee);

		if (expty_test.ty->kind != Ty_int){
        	EM_error(test->pos, "expression must be integer");
		}

		

		if (elsee != NULL) {//if then else
			if ( !compare_ty( expty_then.ty, expty_elsee.ty ) ) {
          		EM_error(elsee->pos, "then exp and else exp type mismatch");
			}
		}else if(expty_then.ty->kind != Ty_void) {//if then
        		EM_error(then->pos, "if-then exp's body must produce no value");
      	}

		
		return expTy(NULL, expty_then.ty);
	}
	*/
case A_breakExp: {
      if (!S_look(venv, S_Symbol("<loop>"))) {
        EM_error(a->pos, "break must be in a loop");
      }
      return expTy(NULL, Ty_Void());
    }
	/*
	let
		type  arrtype = array of int
		var arr1:arrtype := arrtype [10] of 0
	in
		arr1
	end
	*/
    case A_letExp: {
	  //
	  //struct {A_decList decs; A_exp body;} let;
      A_decList decs = a->u.let.decs;
	  A_exp body = a->u.let.body;

      S_beginScope(venv);
      S_beginScope(tenv);

      for (; decs; decs = decs->tail){
		//A_dec decs->head
        transDec(venv, tenv, decs->head);
		//printf("# of decs\n");
	  }
      struct expty exp = transExp(venv, tenv, body);

      S_endScope(tenv);
      S_endScope(venv);
		//assert(0);
      return exp;
    }
	/*
		var arr1:arrtype := arrtype [10] of 0
	*/
	case A_arrayExp: {
	  //
	  //struct {S_symbol typ; A_exp size, init;} array;
	  S_symbol typ = a->u.array.typ;
      A_exp size = a->u.array.size;
	  A_exp init = a->u.array.init;

	  Ty_ty ty_ty_typ = S_look(tenv, typ);
	  struct expty expty_size = transExp(venv, tenv, size);
	  struct expty expty_init = transExp(venv, tenv, init);

	  //type intArray = "array" of int
	  //var row := "intArray" [ N ] of 0
	  ty_ty_typ = actual_ty(ty_ty_typ);
      if (ty_ty_typ->kind != Ty_array) {
        EM_error(a->pos, "type %s must be array",  S_name(typ));
        return expTy(NULL, ty_ty_typ);
      }

	  //var row := "intArray" [ N ] of "0"
      if (!compare_ty(ty_ty_typ->u.array, expty_init.ty))
        EM_error(a->pos, "type mismatch");

      if (ty_ty_typ)
        return expTy(NULL, ty_ty_typ);
      else {
        EM_error(a->pos, "undefined type %s", S_name(typ));
        return expTy(NULL, Ty_Int());//?????????????????????shold return what??
      }
	  
	}
	case A_nilExp:
      		return expTy(NULL, Ty_Nil());
	/*
		var arr1:arrtype := arrtype [10] of 0
	*/
	case A_intExp: {
	  //
	  //int intt;
	  int intt = a->u.intt;
	  //
	  //exptyは実際の構造体(Tr_exp)と型(Ty_ty)の情報を持つ. Tr_expはLab4では実装しない
	  //struct expty expTy(Tr_exp exp, Ty_ty ty)
	  /*
	  struct Ty_ty_ 
		{
		  enum {Ty_record, Ty_nil, Ty_int, Ty_string, Ty_array,
		  Ty_name, Ty_void} kind;
		union {
			Ty_fieldList record;
			Ty_ty array;
			struct {S_symbol sym; Ty_ty ty;} name;
		} u;
	};
	  */
	  //printf("A_intExp\n");
	  return expTy(NULL, Ty_Int());
	}
	case A_stringExp: {
	  //
	  //string stringg;
	  string stringg = a->u.stringg;
	  //printf("A_stringExp\n");
	  return expTy(NULL, Ty_String());
	}
    
  }
  assert(0);
}

void transDec(S_table venv, S_table tenv, A_dec d) {
	/*
	struct A_dec_ 
	{
		union 
		{
			A_fundecList function;
			// escape may change after the initial declaration
			struct {S_symbol var; S_symbol typ; A_exp init; bool escape;} var;
			A_nametyList type;
		} u;
	};
	*/
	switch(d->kind){
		
		/*
			var arr1:arrtype := arrtype [10] of 0
		*/
		case A_varDec: {
			//
			//struct {S_symbol var; S_symbol typ; A_exp init; bool escape;} var;
			S_symbol var = d->u.var.var;
			S_symbol typ = d->u.var.typ;
			A_exp init = d->u.var.init;

			Ty_ty ty_ty_typ = NULL;
      		struct expty expty_init = transExp(venv, tenv, init);

      		if (typ)//var John : "Employeeここがある" :=
        		ty_ty_typ = actual_ty(transTy(tenv, A_NameTy(d->pos, typ)));//すでに定義されている必要がある．
      
      		// Nil assigning checking 
      		if (!ty_ty_typ && expty_init.ty->kind == Ty_nil)
       			 EM_error(d->pos, "init should not be nil without type specified");
	
	  		//implicit typing except nil on right side
      		if (!ty_ty_typ)
        		ty_ty_typ = expty_init.ty;

	  		//compare left side & right side type
      		if (!compare_ty(ty_ty_typ, expty_init.ty))
        		EM_error(d->pos, "type mismatch");
        
			//void S_enter(S_table t, S_symbol sym, void *value);
      		S_enter(venv, var, E_VarEntry(ty_ty_typ));//************************************************************************S_enter
			debug_print("var" ,S_name(var) );
      		return ;
		}
		/*
			type  "arrtype" = array of int //Ty_name()
			type "row" =...
			type...
				のリスト
		*/
		case A_typeDec: {
			//
			//A_nametyList type;
			A_nametyList type = d->u.type;

	  //first cycle
      //add new type FIRST!! case for recursive types
      for (; type; type = type->tail) {
		  //
		  //struct A_namety_ {S_symbol name; A_ty ty;};
		  S_symbol name = type->head->name;
		  A_ty ty = type->head->ty;
         if ( S_look(tenv, name) ){//type already defined
           EM_error(d->pos, "two types have the same name");
		   assert(0);
		 }
		 
		 //Ty_ty ty_ty_typ = transTy(tenv, ty);//convert A_ty to Ty_ty, Ty_ty Ty_Name(S_symbol sym, Ty_ty ty);
         S_enter(tenv, name, Ty_Name(name, NULL));//Ty_Name(name, NULL)second argment wil be filled in second cycle//************************************************S_enter
		 debug_print("type", S_name(name) );
      }

	
      //second cycle
      for (type = d->u.type; type; type = type->tail) {
		//
		//struct A_namety_ {S_symbol name; A_ty ty;};
		S_symbol name = type->head->name;
        Ty_ty ty_ty_typ = S_look(tenv, name);
        if (!ty_ty_typ || ty_ty_typ->kind != Ty_name){
          EM_error(d->pos, "type %s not found", S_name(name));//first cycleで入れたから見つかるはず
		}
        ty_ty_typ->u.name.ty = transTy(tenv, type->head->ty);//fill the second argment of Ty_Name(name, NULL) 
      }
	

      // Cycle Check 
      for (type = d->u.type; type; type = type->tail) {
		//
		//struct A_namety_ {S_symbol name; A_ty ty;};
		S_symbol name = type->head->name;
        Ty_ty ty_ty_typ = S_look(tenv, name);
        if (!ty_ty_typ)
          EM_error(d->pos, "type %s not found", S_name(name));
        
        if (ty_ty_typ->kind == Ty_name && actual_ty(ty_ty_typ) == ty_ty_typ) {
          EM_error(d->pos, "illegal type cycle");
          break;
        }
      }
      break;
	  
		}
		/*
			function nfactor(n: int): int =
				if  n = 0 
				then 1
				else n * nfactor(n-1)

			function do_nothing1(a: int, b: string)=
				do_nothing2(a+1)

			function do_nothing2(d: int) =
				do_nothing1(d, "str")

			//functionのリスト
			//the order of definition doesnt matter
		*/

		case A_functionDec: {
			//
			//A_fundecList function;
			//struct A_fundecList_ {A_fundec head; A_fundecList tail;};
			A_fundecList function = d->u.function;

			//struct Ty_tyList_ {Ty_ty head; Ty_tyList tail;};
			Ty_tyList ty_ty_params = NULL;
			Ty_tyList last_ty_ty_params = NULL;

			//FIRST!! define function FIRST!! case for mutually recursive procedures
			//iterate over every A_funcdec in A_fundecList
			for( ; function; function=function->tail ){//A_fundec
				//
				//struct A_fundec_ {
				//				A_pos pos;
                //				S_symbol name; A_fieldList params; 
				//				S_symbol result; 
				//				A_exp body;
				//};
				S_symbol name = function->head->name;
				A_fieldList params = function->head->params;
				S_symbol result = function->head->result;
				//A_exp body = function->head->body;

				//function returns value or void 
				Ty_ty ty_ty_result = (result) ? S_look(tenv, result) : Ty_Void();

				//already defined
				if( S_look(venv, name) ){
					EM_error(d->pos, "two functions have the same name");
				}

				//PARAMS TYPE CHECK
				//iterate over every params 
				for (params = function->head->params; params; params = params->tail){//A_field
					S_symbol name = params->head->name;
					S_symbol typ = params->head->typ;

					//Insert at tail to avoid order reversed 
					if (ty_ty_params == NULL) {
						//
						//struct Ty_ty_ 
						//{
						//	enum {Ty_record, Ty_nil, Ty_int, Ty_string, Ty_array,Ty_name, Ty_void} kind;
						//	union {
						//		Ty_fieldList record;
						//		Ty_ty array;
						//		struct {S_symbol sym; Ty_ty ty;} name;
						//		} u;
						//};
						//
						//struct Ty_tyList_ {Ty_ty head; Ty_tyList tail;};
      					ty_ty_params = Ty_TyList(Ty_Name(name, actual_ty(transTy(tenv, A_NameTy(params->head->pos, typ)))), NULL);//this always positions HEAD!!
      					last_ty_ty_params = ty_ty_params;
    				} else {
      					last_ty_ty_params->tail = Ty_TyList(Ty_Name(name, actual_ty(transTy(tenv, A_NameTy(params->head->pos, typ)))), NULL);
      					last_ty_ty_params = last_ty_ty_params->tail;
    				}
  				}

				//define function
				S_enter(venv, name, E_FunEntry(ty_ty_params, ty_ty_result));
				debug_print ("func", S_name(name) );

			}

			//SECOND!! process body expression of function
			//iterate over every A_funcdec in A_fundecList
			for(function = d->u.function ; function; function=function->tail ){//A_fundec
				//
				//struct A_fundec_ {
				//				A_pos pos;
                //				S_symbol name; A_fieldList params; 
				//				S_symbol result; 
				//				A_exp body;
				//};
				S_symbol name = function->head->name;
				A_fieldList params = function->head->params;
				S_symbol result = function->head->result;
				A_exp body = function->head->body;
			
			
				//process formals
				S_beginScope(venv);
        		{
					//
					//struct A_fieldList_ {A_field head; A_fieldList tail;};
          			A_fieldList params = function->head->params;
					
					Ty_tyList ty_ty_tyList = ty_ty_params;
          			for (; params; params = params->tail, ty_ty_tyList = ty_ty_tyList->tail){
						//
						//struct A_field_ {S_symbol name, typ; A_pos pos; bool escape;};
            			S_enter( venv, params->head->name, E_VarEntry(ty_ty_tyList->head) );//--------------------------------------------------------S_enter
					}
        		}

				//process function body
				struct expty expty_body = transExp(venv, tenv, body);
				E_enventry e = S_look(venv, name);//should be found because already S_enter above
        		if (!compare_ty(expty_body.ty, e->u.fun.result)){
          			if (e->u.fun.result->kind == Ty_void)
            			EM_error(function->head->pos, "procedure returns value");
          			else 
            			EM_error(function->head->pos, "function body return type mismatch: %s", S_name(name));
				}

				S_endScope(venv);
			
			
			}
			break ;
		}
		
		default:
      		assert(0);
	}
}



Ty_ty transTy(S_table tenv, A_ty a){

	//Takes
	/*
	struct A_ty_ 
	{
		enum {A_nameTy, A_recordTy, A_arrayTy} kind;
		A_pos pos;
		union 
		{
			S_symbol name;
			A_fieldList record;
			S_symbol array;
		} u;
	};
	*/
	//
	//Ty_ty S_look()であるか調べる.
	//
	//Returns
	/*
	struct Ty_ty_ 
	{
		enum {Ty_record, Ty_nil, Ty_int, Ty_string, Ty_array,
			  Ty_name, Ty_void} kind;
		union {
			Ty_fieldList record;
			Ty_ty array;
			struct {S_symbol sym; Ty_ty ty;} name;
		} u;
	};
*/
	switch(a->kind){

		/*
			var arr1:"arrtype" := arrtype [10] of 0
			type myint := "int"
		*/
		case A_nameTy:{
			//
			//S_symbol name;
			S_symbol name = a->u.name;
			
			Ty_ty ty_ty_name = S_look(tenv, name);

			if(ty_ty_name){
				debug_print("transTy", S_name(name));
				return ty_ty_name;
			}else{
				EM_error(a->pos, "undefined type %s", S_name(a->u.name));
        		return ty_ty_name;//return empty Ty_ty;	
			}
			
		}
		/*
			type  rectype = {name:"string", age:"int"}
		*/
		case A_recordTy: {
			//
			//A_fieldList record;
			A_fieldList record = a->u.record;


			//validate type
			Ty_fieldList tl = NULL;
			Ty_fieldList last_tl = NULL;
			for(; record; record=record->tail){
				A_field head = record -> head;
				S_symbol name = head -> name;
				S_symbol typ = head -> typ;

				Ty_ty ty_ty_typ = S_look(tenv, typ);
				//printf("--%s\n", S_name(typ));

				//one field type not found
				if(!ty_ty_typ){
					EM_error(a->pos, "undefined type %s", S_name(typ));
					return ty_ty_typ;//return empty Ty_ty;
				}

			
				//construct fieldList from valid types
				//BE CAREFUL OF ORDER add new elem at tail, and set tail be NULL
				if (tl == NULL) {//first elem in linked list
					//struct A_fieldList_ {A_field head; A_fieldList tail;};
					//struct A_field_ {S_symbol name, typ; A_pos pos; bool escape;};
    	     		tl = Ty_FieldList(Ty_Field(name, ty_ty_typ), NULL);//first elem in list, tail is set NULL
        	  		last_tl = tl;
        		} else {
          			last_tl->tail = Ty_FieldList(Ty_Field(name, ty_ty_typ), NULL);
          			last_tl = last_tl->tail;
       			}
				
			}
			debug_print("transTy", "A_recordTy");
			return Ty_Record(tl);
		}
		/*
			type  arrtype = array of "int"
		*/
		case A_arrayTy: {
			//
			//S_symbol array;
			S_symbol array = a->u.array;

			Ty_ty ty_ty_typ = S_look(tenv, array);

			if(!ty_ty_typ){
				EM_error(a->pos, "undefined type %s", S_name(a->u.name));
					return ty_ty_typ;//return empty Ty_ty;
			}
			debug_print("transTy", "A_arrayTy");
			return Ty_Array(ty_ty_typ);
		}
		default:{
			assert(0);
		}
	}

	return NULL;//equals to returning empty Ty_ty;
}


struct expty transVar(S_table venv, S_table tenv, A_var v){
/*
struct A_var_
{
	union 
	{
		S_symbol simple;
		struct {A_var var; S_symbol sym;} field;
	    struct {A_var var; A_exp exp;} subscript;
	} u;
};
*/
  switch (v->kind) {
	  /*
	  	"a" := a + 1
	  */
	case A_simpleVar: {
		//
		//S_symbol simple;
		S_symbol simple = v->u.simple;
		
      	E_enventry entry_simple = S_look(venv, simple);//S_look means Symbol_lookUp
      	if (entry_simple && entry_simple->kind == E_varEntry) {
        	return expTy(NULL, actual_ty(entry_simple->u.var.ty));
      	}else {
        	EM_error(v->pos, "undefined variable %s", S_name(simple));
        return expTy(NULL, Ty_Int());
      	}
    }
	/*
		rec1.name := 3;
	*/
	case A_fieldVar: {
		//
		//struct {A_var var; S_symbol sym;} field;
		A_var var = v->u.field.var;
		S_symbol sym = v->u.field.sym;

		struct expty expty_var = transVar(venv, tenv, var);
		if (expty_var.ty->kind != Ty_record) {
        	EM_error(v->pos, "not a record type");
        	return expTy(NULL, Ty_Int());
      	}

		Ty_fieldList list;
      	for (list =  expty_var.ty->u.record; list != NULL; list = list->tail){
        if (!strcmp(S_name(list->head->name), S_name(v->u.field.sym))){
			//record found
          return expTy(NULL, actual_ty(list->head->ty));
		}
	  }

	  //no corresponding record found
      EM_error(v->pos, "field %s doesn't exist", S_name(v->u.field.sym));
      return expTy(NULL, Ty_Int());
	}
	/*
		col[c] : = r;
	*/
	case A_subscriptVar: {
		//
		//struct {A_var var; A_exp exp;} subscript;
		A_var var = v->u.subscript.var;
		A_exp exp = v->u.subscript.exp;

		struct expty expty_var=transVar(venv, tenv, var);
		if(expty_var.ty->kind!=Ty_array){
			EM_error(v->pos, "array type required");
			return expTy(NULL, Ty_Int());
		}

		struct expty expty_exp = transExp(venv, tenv, exp);
		if(expty_exp.ty->kind!=Ty_int){
			EM_error(v->u.subscript.exp->pos, "subscript must be int");
			return expTy(NULL, Ty_Int());//???????????????????????????????
		}
		return expTy(NULL, actual_ty(expty_var.ty->u.array));
	}
	default:
		assert(0);
  }

}
