/* -*- tab-width:4; -*- */
/*
 * Procedure related functions
 *
 * $Id$
 */
#include "s.h"
#include "proc.h"
#include "heap.h"

void scm_env_mark(SOBJ env)
{
  SOBJ val;
  int i;

  if (env == NULL) 	return;

  for (i = 0; i < SCM_INUM(SCM_ENV_FRAME(env)->nslots); i++) {
	val = SCM_ENV_FRAME(env)->binding[i];
	if (scm_is_pointer_to_heap(val)) {
	  scm_gc_mark(val);
	}
  }
  if (SCM_ENV_NEXT(env) != NULL) {
	scm_gc_mark(SCM_ENV_NEXT(env));
  }
}

void scm_proc_mark(SOBJ proc)
{
  SCM_Code *c; 
  int i;
  
  if (SCM_PROC_ENV(proc)) 	scm_gc_mark(SCM_PROC_ENV(proc));

  if ((c = SCM_PROC_CODE(proc)) != NULL) {
	scm_gc_mark(c->envlist);
	
	for (i = 0; i < c->size; i++) {
	  if (scm_is_pointer_to_heap(c->code[i]))
		scm_gc_mark(c->code[i]);
	}
  }
}

void scm_clos_mark(SOBJ obj)
{
  if (SCM_CLOSURE_ENV(obj)) 	scm_gc_mark(SCM_CLOSURE_ENV(obj));
  if (SCM_CLOSURE_CODE(obj))	scm_gc_mark(SCM_CLOSURE_CODE(obj));
}

void scm_env_sweep(SOBJ env)
{
  if (SCM_ENV_FRAME(env)) {
	scm_free(SCM_ENV_FRAME(env));
	SCM_ENV_FRAME(env) = NULL;
  }
}

void scm_proc_sweep(SOBJ proc)
{
  if (SCM_PROC_CODE(proc)) {
	scm_free(SCM_PROC_CODE(proc));
	SCM_PROC_CODE(proc) = NULL;
  }
}

void scm_env_print(SOBJ obj, PORT *p)
{
  int i;
  if (obj == NULL) return;
	
  if (SCM_ENV_FRAME(obj) == NULL) {
	port_puts(p, "#<env null>");
  } else {
	port_puts(p, "#<env next=");
	port_putx(p, SCM_ENV_NEXT(obj));
	port_puts(p, " nslots=");
	port_putn(p, SCM_INUM(SCM_ENV_FRAME(obj)->nslots));
	port_puts(p, " binding[]=");
	for (i = 0; i < SCM_INUM(SCM_ENV_FRAME(obj)->nslots); i++) {
	  if (!SCM_INUMP(SCM_ENV_FRAME(obj)->binding[i]) &&
		  SCM_CLOSUREP(SCM_ENV_FRAME(obj)->binding[i])) {
		port_puts(p, "#<closure>");
	  } else {
		scm_write_obj(SCM_ENV_FRAME(obj)->binding[i], p, 1);
	  }
	  if (i != SCM_INUM(SCM_ENV_FRAME(obj)->nslots)-1) port_putc(p, ' ');
	}
	port_putc(p, '>');
  }
}

SCM_STRBUF *scm_env2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  SOBJ *bind;
  int i;
  char *spacer;

  if (obj == NULL) 	return(sb);
  if (SCM_ENV_FRAME(obj) == NULL)
	return(scm_strbuf_concat_str(sb, "#<env NULL>"));

  sb = scm_strbuf_concat_sprintf(sb, "#<env %p: next=%p nslots=%d binding=[",
								 obj,
								 SCM_ENV_NEXT(obj),
								 SCM_INUM(SCM_ENV_FRAME(obj)->nslots));

  spacer = "[";
  bind = SCM_ENV_FRAME(obj)->binding;
  for (i = 0; i < SCM_INUM(SCM_ENV_FRAME(obj)->nslots); i++) {
	sb = scm_strbuf_concat_str(sb, spacer);
	spacer = " ";
	if (!SCM_INUMP(bind[i]) && SCM_CLOSUREP(bind[i])) {
	  sb = scm_strbuf_concat_str(sb, "#<closure>");
	} else {
	  sb = scm_iobj2str(sb, bind[i], raw);
	}
  }
  sb = scm_strbuf_concat_str(sb, "]");
  return(sb);
}

void scm_proc_print(SOBJ obj, PORT *p)
{
  SOBJ env, e;
  int i;

  if (SCM_PROC_CODE(obj) == NULL) {
	port_puts(p, "#<proc null>");
  } else {
	port_puts(p, "#<proc (");
	env = SCM_PROC_CODE(obj)->envlist;
	if (env && SCM_PAIRP(env))
	  e = scm_reverse(SCM_CAR(env));
	else
	  e = NULL;

	if (SCM_PROC_CODE(obj)->nlocals) {
	  port_puts(p, "local: ");
	  for (i = 0; i < SCM_PROC_CODE(obj)->nlocals && e; i++) {
		scm_write_obj(SCM_CAAR(e), p, 1);
		e = SCM_CDR(e);  if (e) port_putc(p, ' ');
	  }
	}
	if (SCM_PROC_CODE(obj)->nlocals) {
	  port_puts(p, " args: ");
	}
	while(e) {
	  if (SCM_CAR(e)) {
		scm_write_obj(SCM_CAAR(e), p, 1);
	  } else {
		port_puts(p, "*BAD_ENV*");
		break;
	  }
	  e = SCM_CDR(e);  if (e) port_putc(p, ' ');
	}
	port_puts(p, ") ");
	scm_env_print(SCM_PROC_ENV(obj), p);
	port_puts(p, " code=");
	port_putx(p, SCM_PROC_CODE(obj));
	port_puts(p, ">");
  }
}

SCM_STRBUF *scm_proc2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  SOBJ env, e;
  int i;
  char *spacer;
  
  if (SCM_PROC_CODE(obj) == NULL)
	return(scm_strbuf_concat_str(sb, "#<proc NULL>"));
  
  env = SCM_PROC_CODE(obj)->envlist;
  e = (env && SCM_PAIRP(env)) ? scm_reverse(SCM_CAR(env)) : NULL;
  sb = scm_strbuf_concat_sprintf(sb, "#<code %p", obj);
  if (SCM_PROC_CODE(obj)->nlocals) {
	spacer = " local:";
	for (i = 0; i < SCM_PROC_CODE(obj)->nlocals && e; i++) {
	  sb = scm_strbuf_concat_str(sb, spacer);  spacer = " ";
	  sb = scm_iobj2str(sb, SCM_CAAR(e), 1);
	}
  }
  spacer = " args:";
  while(e) {
	sb = scm_strbuf_concat_str(sb, spacer);  spacer = " ";
	if (SCM_CAR(e)) {
	  sb = scm_iobj2str(sb, SCM_CAAR(e), 1);
	} else {
	  sb = scm_strbuf_concat_str(sb, "*BADENV*");
	}
	e = SCM_CDR(e);
  }
  sb = scm_env2str(sb, SCM_PROC_ENV(obj), 0);
  sb = scm_strbuf_concat_sprintf(sb, " code=%p>", SCM_PROC_CODE(obj));
  return(sb);
}

void scm_clos_print(SOBJ obj, PORT *p)
{
  port_puts(p, "#<closure ");
  if (SCM_CLOSURE_ENV(obj)) {
	scm_env_print(SCM_CLOSURE_ENV(obj), p);
  } else {
	port_puts(p, "global");
  }
  port_puts(p, " ");
  scm_proc_print(SCM_CLOSURE_CODE(obj), p);
  port_puts(p, ">");
}

SCM_STRBUF *scm_clos2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  sb = scm_strbuf_concat_str(sb, "#<closure ");
  if (SCM_CLOSURE_ENV(obj)) {
	sb = scm_env2str(sb, SCM_CLOSURE_ENV(obj), raw);
  } else {
	sb = scm_strbuf_concat_str(sb, "global");
  }
  sb = scm_strbuf_concat_str(sb, " ");
  sb = scm_proc2str(sb, SCM_CLOSURE_CODE(obj), raw);
  sb = scm_strbuf_concat_str(sb, ">");
  return(sb);
}

/*-- public functions */

/* return #t if x is something we can call */

/*S* (procedure? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a procedure, #f otherwise */

SOBJ scm_procedurep(SOBJ x)
{
  return(SCM_MKBOOL(SCM_PROCP(x) || SCM_CLOSUREP(x) ||
					SCM_PRIMP(x) || SCM_CPRIMP(x)));
}

/*E* (environment? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is an environment, #f otherwise */

/*E* (closure? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a closure, #f otherwise */

/*E* (primitive? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a primitive, #f otherwise */

/*E* (cprimitive? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a C primitive, #f otherwise */

/*E* (syntax? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a syntax, #f otherwise */

SOBJ scm_environmentp(SOBJ x){  return(SCM_MKBOOL(SCM_ENVP(x))); 		}
SOBJ scm_closurep(SOBJ x) 	 {	return(SCM_MKBOOL(SCM_CLOSUREP(x))); 	}
SOBJ scm_primitivep(SOBJ x)  {  return(SCM_MKBOOL(SCM_PRIMP(x))); 		}
SOBJ scm_cprimitivep(SOBJ x) {  return(SCM_MKBOOL(SCM_CPRIMP(x))); 		}
SOBJ scm_syntaxp(SOBJ x) 	 {  return(SCM_MKBOOL(SCM_SYNTAXP(x))); 	}

/*E* (cprimitive-arity PRIM) => NUMBER */
SOBJ scm_cprimitive_arity(SOBJ x)
{
  if (!SCM_CPRIMP(x))	SCM_ERR("bad cprimitive", x);
  return(SCM_MKINUM(SCM_CPRIM_NARGS(x)));
}

/*E* (primitive-address PRIM) => POINTER */
SOBJ scm_primitive_address(SOBJ x)
{
  if (!SCM_PRIMP(x))	SCM_ERR("bad cprimitive", x);
  return(scm_mkpointer(SCM_PRIM(x)->address));
}

/*E* (primitive-arity PRIM) => NUMBER */
SOBJ scm_primitive_arity(SOBJ x)
{
  if (!SCM_PRIMP(x))	SCM_ERR("bad cprimitive", x);
  return(SCM_MKINUM(SCM_PRIM(x)->nargs));
}

void scm_init_proc()
{
  scm_add_cprim("procedure?", 	scm_procedurep, 	1);
  scm_add_cprim("environment?", scm_environmentp, 	1);
  scm_add_cprim("closure?",     scm_closurep, 		1);
  scm_add_cprim("primitive?", 	scm_primitivep, 	1);
  scm_add_cprim("cprimitive?", 	scm_cprimitivep, 	1);
  scm_add_cprim("syntax?",		scm_syntaxp,		1);

  scm_add_cprim("cprimitive-arity",		scm_cprimitive_arity,	1);

  scm_add_cprim("primitive-address",	scm_primitive_address,	1);
  scm_add_cprim("primitive-arity",		scm_primitive_arity,	1);
}
