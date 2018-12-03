/*
 * parse.c - Parse source file.
 */

#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "types.h"
#include "errormsg.h"
#include "parse.h"
#include "semant.h"

extern int yyparse(void);
extern A_exp absyn_root;

/* parse source file fname; 
   return abstract syntax data structure */
A_exp parse(string fname) 
{EM_reset(fname);
 if (yyparse() == 0) /* parsing worked */
   return absyn_root;
 else return NULL;
}

/*
used for component testing
*/
void testScope(){



  S_table t=S_empty();
  S_enter(t, S_Symbol("greeting"), "goodbye");
  S_beginScope(t);
  S_enter(t, S_Symbol("greeting"), "hello");
  printf("sym:%s\n", S_look(t,S_Symbol("greeting")) );
  S_endScope(t);
  printf("sym:%s\n", S_look(t,S_Symbol("greeting")) );


  return ;
}

int main(int argc, char **argv){

  //testScope();

  if(argc != 2){
    fprintf(stderr,"usage: a.out filename\n");
    exit(1);
  }
  //pr_exp(stderr, parse(argv[1]), 0);
  parse(argv[1]);
  //pr_exp(stderr,absyn_root,0);
  if (absyn_root){
  	SEM_transProg(absyn_root);
  } 
  fprintf(stderr,"\n");
  return 0;
}
