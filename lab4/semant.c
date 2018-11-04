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

/* Type list generation of a function */
static Ty_tyList makeFormalTyList(S_table tenv, A_fieldList params) {
  A_fieldList l;
  Ty_tyList tys = NULL, last_tys = NULL;
  for (l = params; l; l = l->tail) {
    /* Insert at tail to avoid order reversed */
    if (tys == NULL) {
      tys = Ty_TyList(Ty_Name(l->head->name,
        actual_ty(transTy(tenv, A_NameTy(l->head->pos, l->head->typ)))), NULL);
      last_tys = tys;
    } else {
      last_tys->tail = Ty_TyList(Ty_Name(l->head->name,
        actual_ty(transTy(tenv, A_NameTy(l->head->pos, l->head->typ)))), NULL);
      last_tys = last_tys->tail;
    }
  }
  return tys;
}

/*
struct A_var_
{
	union 
	{
		//var a=
		S_symbol simple;//kind==A_simpleVar
		struct {A_var var; S_symbol sym;} field;//kind==A_fieldVar
	    struct {A_var var; A_exp exp;} subscript;//kind==A_subscriptVar
	} u;
};
*/
struct expty transVar(S_table venv, S_table tenv, A_var v){
  switch (v->kind) {
	/*
		var greeting := hello(simpleVar)
		var elem := arr1[3](SubscriptVar)
		var node := tree.children(fieldVar)
	*/
	//0. expはtransExp, varはtransVarで部品ごとのexptyの変換.
	//1. S_lookでvenv内（現在の環境スコープ内)にexptyの該当するS_symbolがあるか調べる．
	//(S_look, strcmp(S_name), !=Ty_array())
	//2. あったらexptyに変換(constructor expTyを使う)してreturn
    case A_simpleVar: {
      E_enventry x = S_look(venv, v->u.simple);
      if (x && x->kind == E_varEntry) {
        return expTy(NULL, actual_ty(x->u.var.ty));
      }
      else {
        EM_error(v->pos, "undefined variable %s", S_name(v->u.simple));
        return expTy(NULL, Ty_Int());
      }
    }
	case A_subscriptVar: {
      struct expty var, sub;
	  //var elem := arr1["5"] 
      sub = transExp(venv, tenv, v->u.subscript.exp);
      if (sub.ty->kind != Ty_int)
        EM_error(v->u.subscript.exp->pos, "subscript must be int");
	  //var elem := "arr1"[5] 
      var = transVar(venv, tenv, v->u.subscript.var);
      if (var.ty->kind != Ty_array) {
        EM_error(v->pos, "array type required");
        return expTy(NULL, Ty_Int());
      }

      return expTy(NULL, actual_ty(var.ty->u.array));
    }
	case A_fieldVar: {
	  //var node := "tree".children
      struct expty var = transVar(venv, tenv, v->u.field.var);
      if (var.ty->kind != Ty_record) {
        EM_error(v->pos, "not a record type");
        return expTy(NULL, Ty_Int());
      }

      Ty_fieldList list;
      for (list = var.ty->u.record; list != NULL; list = list->tail){
	  	//var node := tree."children", !strcmp(S_name(list->head->name), S_name("children")
        if (!strcmp(S_name(list->head->name), S_name(v->u.field.sym))){
			//record found
          return expTy(NULL, actual_ty(list->head->ty));
		}
	  }

	  //no corresponding record found
      EM_error(v->pos, "field %s doesn't exist", S_name(v->u.field.sym));
      return expTy(NULL, Ty_Int());
    }
  }
}
//venv: var environment
//tenv: type environment
//transform A_exp to expty
struct expty transExp(S_table venv, S_table tenv, A_exp a){
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
		case A_letExp: { 
			struct expty exp;
			A_decList d;
			S_beginScope(venv); 
			S_beginScope(tenv);

			//show all decs
			//start from d, unless d!=NULL, next itr=d->tail
			for (d = a->u.let.decs; d; d=d->tail){
				if(d->head->kind==A_functionDec){
			
					
					A_fundecList f;
					for (f = d->head->u.function; f; f=f->tail){
						printf("dec:A_functionDec, ");
						printf("funcName:%s\n",S_look(venv,d->head->u.function->head->name));
					}
				}
				else if(d->head->kind==A_varDec){
					printf("dec:A_varDec\n");
				}
				else{
					printf("dec:A_typeDec\n");
				}
        		//transDec(venv,tenv,d->head);
			}
		
			//show all exps
			if(a->u.let.body->kind==A_seqExp){
				printf("in body:%d\n",a->u.let.body->kind);
				A_expList e;
				for(e=a->u.let.body->u.seq; e; e=e->tail){
					printf("exp:%d\n",e->head->kind);
				}
			}
			
    		//exp = transExp(venv,tenv,a->u.let.body);
    		S_endScope(tenv);
   			S_endScope(venv);
    		return exp;
		}
		*/
		case A_varExp: {
      		struct expty exp = transVar(venv, tenv, a->u.var);
      		return expTy(NULL, exp.ty);
    	}
		case A_nilExp:
      		return expTy(NULL, Ty_Nil());
    	case A_intExp:
      		return expTy(NULL, Ty_Int());
		case A_stringExp:
      		if (a->u.stringg == NULL)
        	EM_error(a->pos, "string required");
      		return expTy(NULL, Ty_String());
			  //1. search from current environment table
			  //2. if found, check whether it is a E_funcEntry rather than E_varEntry
			  //3. check params type(args type)
			  //4. return expTy(formatted type struct)
		case A_callExp: {
      		E_enventry func = S_look(venv, a->u.call.func);
      		if (func == NULL) {
        		EM_error(a->pos, "undefined function %s", S_name(a->u.call.func));
        		return expTy(NULL, Ty_Void());
      		}

      		if (func->kind != E_funEntry) {
        		EM_error(a->pos, "%s is not a function", S_name(a->u.call.func));
        		return expTy(NULL, Ty_Void());
      		}

      /* Compare param type */
      Ty_tyList tl;
      A_expList el;
      struct expty exp;
      for (tl = func->u.fun.formals, el = a->u.call.args;
        tl && el; tl = tl->tail, el = el->tail) {	
        exp = transExp(venv, tenv, el->head);
        if (!compare_ty(tl->head, exp.ty)) {
          EM_error(el->head->pos, "para type mismatch");
          break;
        }
      }

		if (el){
			EM_error(a->pos, "too many params in function %s", S_name(a->u.call.func));
	  	}
       
      	if (tl){
			EM_error(a->pos, "too few params in function %s", S_name(a->u.call.func));
	  	}
      
        

      return expTy(NULL, func->u.fun.result);
    }
	case A_opExp: {
      A_oper oper = a->u.op.oper;
      struct expty left = transExp(venv, tenv, a->u.op.left);
      struct expty right = transExp(venv, tenv, a->u.op.right);
      switch (oper) {
        case A_plusOp:
        case A_minusOp:
        case A_timesOp:
        case A_divideOp: {
          if (left.ty->kind != Ty_int)
            EM_error(a->u.op.left->pos, "integer required");
          if (right.ty->kind != Ty_int)
            EM_error(a->u.op.right->pos, "integer required");
          return expTy(NULL, Ty_Int());
    	}
		case A_eqOp:
        case A_neqOp: {
          if (!compare_ty(left.ty, right.ty)) {
            EM_error(a->u.op.right->pos, "same type required");
            return expTy(NULL, Ty_Int());
          }
          return expTy(NULL, Ty_Int());
        }
        case A_ltOp:
        case A_leOp:
        case A_gtOp:
        case A_geOp: {
          Ty_ty leftTy = actual_ty(left.ty);
          Ty_ty rightTy = actual_ty(right.ty);
          if (leftTy->kind != Ty_int && leftTy->kind != Ty_string)
            EM_error(a->u.op.left->pos, "string or integer required");
          if (leftTy->kind == Ty_int) {
            if (rightTy->kind != Ty_int)
              EM_error(a->u.op.right->pos, "same type required");
            return expTy(NULL, Ty_Int());
          }
          if (leftTy->kind == Ty_string) {
            if (rightTy->kind != Ty_string)
              EM_error(a->u.op.right->pos, "same type required");
            return expTy(NULL, Ty_Int());
          }
          /* Error recovery */
          return expTy(NULL, Ty_Int());
        }
      }
    }
	case A_recordExp: {
      /* Get the record type */
      Ty_ty t = actual_ty(transTy(tenv, A_NameTy(a->pos, a->u.record.typ)));
      if (t->kind != Ty_record) {
        EM_error(a->pos, "type %s is not record", S_name(a->u.record.typ));
        return expTy(NULL, Ty_Record(NULL));
      }

      /* Compare fields */
      A_efieldList el;
      Ty_fieldList fl;
      struct expty exp;
      for (el = a->u.record.fields, fl = t->u.record;
        el && fl; el = el->tail, fl = fl->tail) {
        if (strcmp(S_name(el->head->name), S_name(fl->head->name)) != 0) {
          EM_error(a->pos, "field name should be %s but not %s",
            S_name(fl->head->name), S_name(el->head->name));
          continue;
        }

        exp = transExp(venv, tenv, el->head->exp);
        if (!compare_ty(fl->head->ty, exp.ty))
          EM_error(el->head->exp->pos, "field type of %s mismatch",
            S_name(fl->head->name));
      }
      if (el || fl) {
        EM_error(a->pos, "fields of type %s mismatch", S_name(a->u.record.typ));
      }

      return expTy(NULL, t);
    }
    case A_seqExp: {
      struct expty exp = expTy(NULL, Ty_Void());
      A_expList list;
      for (list = a->u.seq; list != NULL; list = list->tail)
        exp = transExp(venv, tenv, list->head);
      return exp;
    }

	//var name := exp
    case A_assignExp: {
      struct expty var = transVar(venv, tenv, a->u.assign.var);
      struct expty exp = transExp(venv, tenv, a->u.assign.exp);
      if (!compare_ty(var.ty, exp.ty)){
        EM_error(a->pos, "unmatched assign exp");
	  }

      /* Loop variable detection */
      if (a->u.assign.var->kind == A_simpleVar &&
        S_look(venv, protect_sym(a->u.assign.var->u.simple)) != NULL) {
        EM_error(a->u.assign.var->pos, "loop variable can't be assigned");
      }
      return expTy(NULL, Ty_Void());
    }
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
    case A_whileExp: {
      struct expty exp, body;
      exp = transExp(venv, tenv, a->u.whilee.test);
      if (exp.ty->kind != Ty_int)
        EM_error(a->u.whilee.test->pos, "expression must be integer");

      S_beginScope(venv);
      /* Symbol for break checking */
      S_enter(venv, S_Symbol("<loop>"), E_VarEntry(Ty_Int()));
      body = transExp(venv, tenv, a->u.whilee.body);
      S_endScope(venv);

      if (body.ty->kind != Ty_void)
        EM_error(a->u.whilee.test->pos, "while body must produce no value");
      return expTy(NULL, Ty_Void());
    }
    case A_forExp: {
      struct expty explo, exphi, body;
      S_beginScope(venv);

      explo = transExp(venv, tenv, a->u.forr.lo);
      if (explo.ty->kind != Ty_int)
        EM_error(a->u.forr.lo->pos, "for exp's range type is not integer");
      exphi = transExp(venv, tenv, a->u.forr.hi);
      if (exphi.ty->kind != Ty_int)
        EM_error(a->u.forr.hi->pos, "for exp's range type is not integer");

      S_enter(venv, a->u.forr.var, E_VarEntry(Ty_Int()));
      /* Symbol for loop variable checking */
      S_enter(venv, protect_sym(a->u.forr.var), E_VarEntry(Ty_Int()));
      /* Symbol for break checking */
      S_enter(venv, S_Symbol("<loop>"), E_VarEntry(Ty_Int()));

      body = transExp(venv, tenv, a->u.forr.body);
      if (body.ty->kind != Ty_void)
        EM_error(a->u.forr.body->pos, "for body must produce no value");

      S_endScope(venv);
      return expTy(NULL, Ty_Void());
    }
    case A_breakExp: {
      if (!S_look(venv, S_Symbol("<loop>"))) {
        EM_error(a->pos, "break must be in a loop");
      }
      return expTy(NULL, Ty_Void());
    }
    case A_letExp: {
      struct expty exp;
      A_decList d;
      S_beginScope(venv);
      S_beginScope(tenv);

      for (d = a->u.let.decs; d; d = d->tail)
        transDec(venv, tenv, d->head);
      exp = transExp(venv, tenv, a->u.let.body);

      S_endScope(tenv);
      S_endScope(venv);
      return exp;
    }
    case A_arrayExp: {
      Ty_ty t = S_look(tenv, a->u.array.typ);
      struct expty size, init;
      size = transExp(venv, tenv, a->u.array.size);
      init = transExp(venv, tenv, a->u.array.init);

      t = actual_ty(t);
      if (t->kind != Ty_array) {
        EM_error(a->pos, "type %s must be array",  S_name(a->u.array.typ));
        return expTy(NULL, t);
      }

      if (!compare_ty(t->u.array, init.ty))
        EM_error(a->pos, "type mismatch");

      if (t)
        return expTy(NULL, t);
      else {
        EM_error(a->pos, "undefined type %s", S_name(a->u.array.typ));
        return expTy(NULL, Ty_Int());
      }
    }
  }
  assert(0);
}

void transDec(S_table venv, S_table tenv, A_dec d) {

	switch(d->kind){
		/*
		//struct {S_symbol var; S_symbol typ; A_exp init; bool escape;} var;
		vardec:   VAR ID ASSIGN exp             {$$ = A_VarDec(EM_tokPos, S_Symbol($2), NULL, $4);}
    		|     VAR ID COLON ID ASSIGN exp    {$$ = A_VarDec(EM_tokPos, S_Symbol($2), S_Symbol($4), $6);}
		*/
		case A_varDec: {
			Ty_ty t = NULL;
      struct expty e = transExp(venv, tenv, d->u.var.init);

      if (d->u.var.typ)
        t = actual_ty(transTy(tenv, A_NameTy(d->pos, d->u.var.typ)));
      
      /* Nil assigning checking */
      if (!t && e.ty->kind == Ty_nil)
        EM_error(d->pos, "init should not be nil without type specified");

      if (!t)
        t = e.ty;

      if (!compare_ty(t, e.ty))
        EM_error(d->pos, "type mismatch");
        
      S_enter(venv, d->u.var.var, E_VarEntry(t));
      break;
		}
		/*
		//A_nametyList type;
		tydec:  TYPE ID EQ ty               {$$ = A_Namety(S_Symbol($2), $4);}
		*/
		case A_typeDec: {
			A_nametyList l;
      /* First Handle */
      for (l = d->u.type; l; l = l->tail) {
         if (S_look(tenv, l->head->name)){
           EM_error(d->pos, "two types have the same name");
		 }

        S_enter(tenv, l->head->name, Ty_Name(l->head->name, NULL));
      }

      /* Second Handle */
      for (l = d->u.type; l; l = l->tail) {
        Ty_ty t = S_look(tenv, l->head->name);
        if (!t || t->kind != Ty_name)
          EM_error(d->pos, "type %s not found", S_name(l->head->name));
        
        t->u.name.ty = transTy(tenv, l->head->ty);
      }

      /* Cycle Check */
      for (l = d->u.type; l; l = l->tail) {
        Ty_ty t = S_look(tenv, l->head->name);
        if (!t)
          EM_error(d->pos, "type %s not found", S_name(l->head->name));
        
        if (t->kind == Ty_name && actual_ty(t) == t) {
          EM_error(d->pos, "illegal type cycle");
          break;
        }
      }
      break;
		}
		/*
		## A_fundecList function;
		fundec:   FUNCTION ID LPAREN tyfields RPAREN EQ exp             {$$ = A_Fundec(EM_tokPos, S_Symbol($2), $4, NULL, $7);}
    		|     FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp    {$$ = A_Fundec(EM_tokPos, S_Symbol($2), $4, S_Symbol($7), $9);}
		*/
		case A_functionDec: {
			A_fundecList funList;

      /* First Handle */
      for (funList = d->u.function; funList; funList = funList->tail) {
        A_fundec f = funList->head;
        Ty_ty resultTy = (f->result) ? S_look(tenv, f->result) : Ty_Void();
        Ty_tyList formalTys = makeFormalTyList(tenv, f->params);

         if (S_look(venv, f->name)){
           EM_error(f->pos, "two functions have the same name");
		 }

        S_enter(venv, f->name, E_FunEntry(formalTys, resultTy));
      }

      /* Second Handle */
      for (funList = d->u.function; funList; funList = funList->tail) {
        A_fundec f = funList->head;
        E_enventry e = S_look(venv, f->name);
        if (e == NULL || e->kind != E_funEntry) {
          EM_error(f->pos, "cannot find function %s", S_name(f->name));
          continue;
        }

        S_beginScope(venv);
        {
          A_fieldList l; Ty_tyList t;
          for (l = f->params, t = e->u.fun.formals; l; l = l->tail, t = t->tail)
            S_enter(venv, l->head->name, E_VarEntry(t->head));
        }

        struct expty body = transExp(venv, tenv, f->body);
        if (!compare_ty(body.ty, e->u.fun.result)){
          if (e->u.fun.result->kind == Ty_void)
            EM_error(f->pos, "procedure returns value");
          else 
            EM_error(f->pos, "function body return type mismatch: %s", S_name(f->name));
		}
        S_endScope(venv);
      }
      break;
		}
		default:
      		assert(0);
	}
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

struct Ty_ty_ 
{
	enum {Ty_record, Ty_nil, Ty_int, Ty_string, Ty_array,
		  Ty_name, Ty_void} kind;
	
	union {
		Ty_fieldList record;
		Ty_ty array;
		struct {S_symbol sym; Ty_ty ty;} name;
	} u;
};s
*/
/*
ty:     ID                          {$$ = A_NameTy(EM_tokPos, S_Symbol($1));}
    |   LBRACE tyfields RBRACE      {$$ = A_RecordTy(EM_tokPos, $2);}
    |   ARRAY OF ID                 {$$ = A_ArrayTy(EM_tokPos, S_Symbol($3));}
*/
Ty_ty transTy(S_table tenv, A_ty a){

	
	switch(a->kind){

		//type "tree"(name) ={key: int, children: treelist}
		//type "myint"(name) = int
		case A_nameTy:{
			Ty_ty ty;
			ty=S_look(tenv,a->u.name);
			if(ty){
				return ty;
			}else{
				EM_error(a->pos, "undefined type %s", S_name(a->u.name));
        		return ty;//return empty Ty_ty;	
			}
			
		}
		//type tree ={key(name): "int"(typ), children: "treelist"}
		case A_recordTy:{
			Ty_fieldList tl = NULL, last_tl = NULL;
			A_fieldList field_list;
			for(field_list=a->u.record; field_list; field_list=field_list->tail){
				A_field f=field_list->head;
				Ty_ty ty;
				ty=S_look(tenv,f->typ);
				if(!ty){
					EM_error(a->pos, "undefined type %s", S_name(f->typ));
        			return ty;//return empty Ty_ty;	
				}

				if (tl == NULL) {
          tl = Ty_FieldList(Ty_Field(field_list->head->name, ty), NULL);
          last_tl = tl;
        } else {
          last_tl->tail = Ty_FieldList(Ty_Field(field_list->head->name, ty), NULL);
          last_tl = last_tl->tail;
        }
				
				
			}
			return Ty_Record(tl);
		}

		//type  arrtype = array of "myint"(array)
		case A_arrayTy:{
			Ty_ty ty;
			ty=S_look(tenv,a->u.array);
			if(ty){
				return Ty_Array(ty);
			}else{
				EM_error(a->pos, "undefined type %s", S_name(a->u.array));
        		return ty;//return empty Ty_ty;	
			}
		}
		default:{
			
		}
	}

	return NULL;//equals to returning empty Ty_ty;
}
