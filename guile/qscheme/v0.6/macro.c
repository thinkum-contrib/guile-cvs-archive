/* -*- tab-width:4; -*- */
/*
 *
 */
#include "s.h"

#define MACRO_DEBUG

/*E* (make-macro CODE) => MACRO */
SOBJ scm_mkmacro(SOBJ obj)
{
  SOBJ new = NULL;

  new = scm_newcell(SOBJ_T_MACRO);
  SCM_MACRO_CODE(new) = obj;
  SCM_MACRO_FUNC(new) = NULL;

  return(new);
}

void scm_macro_mark(SOBJ obj)
{
  if (SCM_MACRO_CODE(obj)) 	scm_gc_mark(SCM_MACRO_CODE(obj));
  if (SCM_MACRO_FUNC(obj))	scm_gc_mark(SCM_MACRO_FUNC(obj));
}

/*E* (macro->lambda MACRO) => PROC */
SOBJ scm_macro_to_lambda(SOBJ obj)
{
  SOBJ arg, env, formal, expr;
  SCM_Code *code;

  if (!SCM_MACROP(obj)) 	SCM_ERR("bad macro", obj);

#ifdef MACRO_DEBUG
  scm_puts("; macro->lambda "); scm_cprint(obj);
#endif

  arg = env = formal = expr = NULL;

  code = SCM_PROC_CODE(SCM_CLOSURE_CODE(SCM_MACRO_CODE(obj)));
 
  env = SCM_CAR(code->envlist);
  if (code->optargs) {
	formal = SCM_LSYM_NAME(SCM_CAR(env));
	env = SCM_CDR(env);
  }
  while(env && SCM_CDR(env)) {
	formal = scm_cons(SCM_LSYM_NAME(SCM_CAR(env)), formal);
	env = SCM_CDR(env);
  }

  env = SCM_CAR(code->envlist);
  while(env && SCM_CDR(env)) {
	arg = scm_cons(SCM_LSYM_NAME(SCM_CAR(env)), arg);
	env = SCM_CDR(env);
  }

  expr = scm_cons(scm_sym_lambda,
				  scm_cons(formal, scm_cons(scm_cons(obj, arg), NULL)));
  
#ifdef MACRO_DEBUG
  scm_puts("; expr= ");  scm_cprint(expr);
#endif
  return(scm_eval(expr, NULL));   
}

/*E* (macro-func MACRO) => PROC */
SOBJ scm_macro_func(SOBJ macro)
{
  if (!SCM_MACROP(macro)) 	SCM_ERR("bad macro", macro);
  return(SCM_MACRO_FUNC(macro));
}

/*E* (macro-set-func! MACRO FUNC) => MACRO */
SOBJ scm_macro_set_func(SOBJ macro, SOBJ value)
{
  if (!SCM_MACROP(macro)) 	SCM_ERR("bad macro", macro);
  SCM_MACRO_FUNC(macro) = value;
  return(macro);
}

/*E* (macro-code MACRO) => PROC */
SOBJ scm_macro_code(SOBJ macro)
{
  if (!SCM_MACROP(macro)) SCM_ERR("bad macro", macro);
  return(SCM_MACRO_CODE(macro));
}

/*E* (macro-set-code! MACRO CODE) -> MACRO */
SOBJ scm_macro_set_code(SOBJ macro, SOBJ value)
{
  if (!SCM_MACROP(macro)) 	SCM_ERR("bad macro", macro);
  SCM_MACRO_CODE(macro) = value;
  return(macro);
}

void scm_init_macro()
{
  scm_add_cprim("make-macro",		scm_mkmacro,		 1);
  scm_add_cprim("macro->lambda",	scm_macro_to_lambda, 1);

  scm_add_cprim("macro-func",		scm_macro_func,		1);
  scm_add_cprim("macro-set-func!",	scm_macro_set_func,	2);
  scm_add_cprim("macro-code",		scm_macro_code,		1);
  scm_add_cprim("macro-set-code!",	scm_macro_set_code,	2);
}
