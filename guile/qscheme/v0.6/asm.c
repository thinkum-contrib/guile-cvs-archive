/* -*- tab-width:4; -*- */

#include "s.h"
#include "vm2.h"
#include "stack.h"
#include "asm.h"

/* #define DEBUG_ASM */


/* Max number of successive pushes:
 * Note: this measure is important for threads. With correct vm it
 * gives the maximum depth. VM must ensure to sync the stack before
 * calls. So that we have good checkpoints.  */

int scm_push_seq_max;	

/*-- local const */

static SOBJ scm_atom_local, scm_atom_rest, scm_atom_optionnal;

static int
  warn_unbound_func = FALSE,
  warn_unbound_symbol = FALSE;


/*-- forward decl */
static SOBJ scm_asm_internal(SOBJ pcode, SOBJ icode);
static SOBJ scm_optimize1(SOBJ icode);
static SOBJ scm_compile_funcall2(SOBJ icode, SOBJ func, SOBJ argl, SOBJ env);
static SOBJ scm_compile_letrec(SOBJ icode, SOBJ argl, SOBJ env);

/****************************************************************
 * helper functions
 ****************************************************************/

/* search atom in current environment and in the current module
 * chain.
 *
 * Returns:
 *   NULL if symbol is not found
 *   a LSYM if it's a local symbol. in this case the depth is set.
 *   a SYM if it's a global symbol.
 *
 * If create_flag is TRUE, the symbol is created and the functions
 * never returns NULL.  */


static SOBJ lookup_atom(SOBJ atom, SOBJ env, int *depth, int create_flag)
{
  SOBJ sym;

  if (env != NULL) {
	if ((sym = scm_env_search(env, atom, depth)) != NULL)
	  return(sym);
  }
  return(scm_module_find_symbol(scm_current_module, atom, create_flag));
}


/*-- convert icode to proc: Used when assembling (mkclosure) */
static SOBJ icode_to_proc(SOBJ icode, SOBJ env, int nargs, int optargs)
{
  SOBJ proc;
  SCM_Code *code;
  long size;

  size = SCM_ASIZE(icode);
  code = scm_must_alloc(offsetof(SCM_Code, code[size]));
  code->envlist = NULL;
  code->nargs = nargs;
  code->optargs = optargs;
  code->nlocals = 0;
  code->size = size;
  memcpy(code->code, SCM_ARRAY(icode), size * sizeof(SOBJ*));
  
  proc = scm_newcell(SOBJ_T_PROC);

  SCM_PROC_CODE(proc) = code;
  SCM_PROC_ENV(proc) = NULL;
  return(proc);
}

static SOBJ mkproc(SOBJ icode, SOBJ env)
{
  return(icode_to_proc(icode, env, 0, FALSE));
}

static SOBJ mkcode(SOBJ icode)
{
  return(scm_mkcode(SCM_ARRAY(icode), SCM_ASIZE(icode)));
}


/* search for local vars in the body of the expr */
static SOBJ search_local(SOBJ list, SOBJ body)
{
  SOBJ expr, var;
  while(body) {
	expr = SCM_CAR(body);
	if (SCM_PAIRP(expr)) {
	  if (SCM_CAR(expr) == SCM_SYM_NAME(scm_sym_define)) {
		var = SCM_CADR(expr);
		if (SCM_ATOMP(var)) {
		  list = scm_cons(var, list);
		} else if (SCM_PAIRP(var)) {
		  list = scm_cons(SCM_CAR(var), list);
		} else {
		  scm_puts("; strange construct: "); scm_cprint(var);
		}
	  }
	}
	body = SCM_CDR(body);
  }
  return(list);
}

/****************************************************************
 * function to generate and manipulate SCM_ASM_CODE structure
 ****************************************************************/
static int 	scm_asm_last_label = 1;

static SOBJ scm_asm_known_labels; /* array of resolved labels */
static SOBJ scm_asm_unknown_labels;	/* array to store unresolved labels */


static SOBJ next_label()
{
  ++scm_asm_last_label;
  return(SCM_MKINUM(scm_asm_last_label));
}

static SOBJ scm_asm_new_code()
{
  SOBJ code = scm_mkarray(0, NULL);
  return(code);
}

SOBJ scm_array_resize(SOBJ array, int newsize)
{
  SCM_Array *p;
  if (newsize > SCM_AMAX(array)) {
	p = scm_must_realloc(SCM_ADESCR(array),
						 sizeof(SCM_Array) + (newsize * sizeof(SOBJ)));
	SCM_ADESCR(array) = p;
	p->alloced = newsize + 1;
  }
  return(array);
}

static void scm_asm_put_opc(SOBJ c, int opcode)
{
  if (SCM_ASIZE(c) >= SCM_AMAX(c)) 
	scm_array_resize(c, 2 * SCM_ASIZE(c));

  SCM_AREF(c, SCM_ASIZE(c)++) = SCM_OPCODE(opcode);
}

static void scm_asm_put_lit(SOBJ c, SOBJ obj)
{
  if (SCM_ASIZE(c) >= SCM_AMAX(c)) 
	scm_array_resize(c, 2 * SCM_ASIZE(c));

  SCM_AREF(c, SCM_ASIZE(c)++) = obj;
}

static int scm_asm_defined_label(SOBJ lab)
{
  int i;

  for (i = 0; i < SCM_ASIZE(scm_asm_known_labels); i += 2) {
	if (SCM_AREF(scm_asm_known_labels, i) == lab) {
	  return(i);
	}
  }
  return(-1);
}

static void scm_asm_put_lab_ref(SOBJ c, SOBJ lab)
{
  int lab_index; 
  if ((lab_index = scm_asm_defined_label(lab)) >= 0) {
	scm_asm_put_lit(c, SCM_MKINUM(SCM_INUM(SCM_AREF(scm_asm_known_labels,
												lab_index+1)) -
							  SCM_ASIZE(c)));
  } else {
	scm_vector_append(scm_asm_unknown_labels, lab);
	scm_vector_append(scm_asm_unknown_labels, SCM_MKINUM(SCM_ASIZE(c)));
	scm_asm_put_lit(c, SCM_MKINUM(0));
  }
}

static void scm_asm_put2(SOBJ c, int opcode, SOBJ obj)
{
  scm_asm_put_opc(c, opcode);
  scm_asm_put_lit(c, obj);
}
 
/*-- opcode assembly */

static void scm_asm_nop(SOBJ c, SOBJ arglist)
{
}

static void scm_asm_end(SOBJ c, SOBJ arglist)
{
  scm_asm_put_opc(c, SCM_OP_END);
}

static void scm_asm_dolet(SOBJ c, SOBJ arglist)
{
  scm_asm_put_opc(c, SCM_OP_DOLET);
}

static void scm_asm_doletstar(SOBJ c, SOBJ arglist)
{
  scm_asm_put2(c, SCM_OP_DOLETSTAR, SCM_CAR(arglist));
}

static void scm_asm_drop(SOBJ c, SOBJ arglist)
{
  scm_asm_put_opc(c, SCM_OP_DROP);
}

static void scm_asm_pushq(SOBJ c, SOBJ arglist)
{
  scm_asm_put2(c, SCM_OP_PUSH, SCM_CAR(arglist));
}

static void scm_asm_pushv(SOBJ c, SOBJ arglist)
{
  scm_asm_put2(c, SCM_OP_PUSHV, SCM_CAR(arglist));
}

/*I* (pushl <varnum> <depth>) */
static void scm_asm_pushl(SOBJ c, SOBJ arglist)
{
  SOBJ n, d;
  int depth, varnum;
  
  n = SCM_CAR(arglist);
  d = SCM_CADR(arglist);
  depth = SCM_INUM(d);
  if (depth < 4) {
	scm_asm_put2(c, SCM_OP_PUSHL0 + depth, n);
  } else {
	varnum = SCM_INUM(n);
	scm_asm_put2(c, SCM_OP_PUSHL, SCM_MKINUM(depth << 16 | varnum));
  }
}

/*I* (store) */
static void scm_asm_store(SOBJ c, SOBJ arglist)
{
  scm_asm_put_opc(c, SCM_OP_STORE);
}

/* syntax: (setl var-number depth) */
static void scm_asm_setl(SOBJ c, SOBJ arglist)
{
  SOBJ varnum;
  int n, d;
  varnum = SCM_CAR(arglist);
  d = SCM_INUM(SCM_CADR(arglist));

  if (d == 0) {
	scm_asm_put2(c, SCM_OP_SETL0, varnum);
  } else {
	n = SCM_INUM(varnum);
	scm_asm_put2(c, SCM_OP_SETL,  SCM_MKINUM( ((d << 16) | n) ));
  }
}

/* syntax: (setl0drop var-number) */
static void scm_asm_setl0drop(SOBJ c, SOBJ arglist)
{
  SOBJ varnum;
  varnum = SCM_CAR(arglist);
  scm_asm_put2(c, SCM_OP_SETL0DROP, varnum);
}

/*I* (getvar) */
static void scm_asm_getvar(SOBJ c, SOBJ arglist)
{
  scm_asm_put_opc(c, SCM_OP_GETVAR);
}

/*I* (setvar) */
static void scm_asm_setvar(SOBJ c, SOBJ arglist)
{
  scm_asm_put_opc(c, SCM_OP_SETVAR);
}


static void scm_asm_mark(SOBJ c, SOBJ arglist)
{
  scm_asm_put_opc(c, SCM_OP_MARK);
}

/* (mkclosure nvars nopts code) */
static void scm_asm_mkclosure(SOBJ c, SOBJ arglist)
{
  scm_asm_put_opc(c, SCM_OP_MKCLOSURE);
}

/*I* (mkproc varlist nargs nlocals optargs asmcode) */
static void scm_asm_mkproc(SOBJ c, SOBJ argl)
{
  SOBJ varlist, optargs, asmcode, icode;
  SOBJ proc;
  SCM_Code *code;
  int size, nargs, nlocals;
  
  varlist = SCM_CAR(argl);  				argl = SCM_CDR(argl);
  nargs   = SCM_INUM(SCM_CAR(argl));		argl = SCM_CDR(argl);
  nlocals = SCM_INUM(SCM_CAR(argl));		argl = SCM_CDR(argl);
  optargs = SCM_CAR(argl);  				argl = SCM_CDR(argl);
  icode   = SCM_CAR(argl);					argl = SCM_CDR(argl);

  if (argl) SCM_ERR("mkproc: to much args, rest", argl);

  asmcode = scm_asm_internal(scm_asm_new_code(), icode);

  size  = SCM_ASIZE(asmcode);

  code = scm_must_alloc(offsetof(SCM_Code, code[size]));
  code->envlist = varlist;
  code->nargs   = nargs;
  code->nlocals = nlocals;
  code->optargs = (optargs != scm_false);
  code->size    = size;
  memcpy(code->code, SCM_ARRAY(asmcode), size * sizeof(SOBJ*));
  
  proc = scm_newcell(SOBJ_T_PROC);
  SCM_PROC_CODE(proc) = code;
  SCM_PROC_ENV(proc) = NULL;

  scm_asm_put2(c, SCM_OP_PUSH, proc);
}

static void scm_asm_mkcode(SOBJ c, SOBJ arglist)
{
  SCM_ERR("scm_asm_mkcode: not implemented", arglist);
}


static void scm_asm_endlet(SOBJ c, SOBJ arglist)
{
  scm_asm_put_opc(c, SCM_OP_ENDLET);
}

static void scm_asm_return(SOBJ c, SOBJ arglist)
{
  scm_asm_put_opc(c, SCM_OP_RETURN);
}

static void scm_asm_call_jump(SOBJ c, SOBJ arglist, int op_to_compile)
{
  SOBJ f, sym, val;
  /*int depth;*/

  if (arglist == NULL) {		/* no arguments */
	scm_asm_put_opc(c, op_to_compile);
	return;
  }
  f = SCM_CAR(arglist);
  if (SCM_ATOMP(f)) SCM_ERR("scm_asm_call_jump: got atom", f);

  sym = f;

  if (sym == NULL) SCM_ERR("scm_asm_call_jump: undefined symbol for function", f);

  switch(SCM_OBJTYPE(sym)) {
  case SOBJ_T_LSYMBOL:
	SCM_ERR("scm_asm_call_jump: shoud not have lsymbol here", sym);
	/*
	scm_asm_push_local(c, sym, depth);
	scm_asm_put_opc(c, op_to_compile);
	*/
	break;

  case SOBJ_T_SYMBOL:
	val = SCM_SYM_VALUE(sym);
	if (val == NULL) val = scm_unbound;
	switch(SCM_OBJTYPE(val)) {
	case SOBJ_T_CPRIM:
	  {
		int nargs = SCM_CPRIM_NARGS(val);
		if (nargs > 5)
		  SCM_ERR("scm_asm_call_jump: cprim with more than 5 arguments", sym);
	  
		scm_asm_put2(c, SCM_OP_PUSH, val);
		scm_asm_put_opc(c, (nargs < 0) ? SCM_OP_CALLS : SCM_OP_CALLC0 + nargs);
	  }
	  break;
	case SOBJ_T_PRIM:
	  scm_asm_put_lit(c, SCM_PRIM(val)->address);
	  if (op_to_compile == SCM_OP_ENDLET_JUMP) {
		scm_asm_put_opc(c, SCM_OP_ENDLET);
	  }
	  break;

	default:
	  scm_asm_put2(c, SCM_OP_PUSHV, sym);
	  scm_asm_put_opc(c, op_to_compile);
	}
	break;

  default:
	SCM_ERR("call: don't know how to compile", f);
  }
}

static void scm_asm_callp(SOBJ c, SOBJ argl)
{
  scm_asm_put_lit(c, SCM_PRIM(SCM_CAR(argl))->address);
}

static void scm_asm_callc(SOBJ c, SOBJ argl)
{
  SOBJ cfunc = SCM_CAR(argl);
  int nargs = SCM_CPRIM_NARGS(cfunc);
  scm_asm_put2(c, SCM_OP_PUSH, cfunc);
  if (nargs >= 0) {
	scm_asm_put_opc(c, SCM_OP_CALLC0 + nargs);
  } else {
	scm_asm_put_opc(c, SCM_OP_CALLS);
  }
}

static void scm_asm_call(SOBJ c, SOBJ argl)
{
  scm_asm_call_jump(c, argl, SCM_OP_CALL);
}

static void scm_asm_jump(SOBJ c, SOBJ argl)
{
  scm_asm_call_jump(c, argl, SCM_OP_JUMP);
}

static void scm_asm_endlet_jump(SOBJ c, SOBJ argl)
{
  scm_asm_call_jump(c, argl, SCM_OP_ENDLET_JUMP);
}

static void scm_asm_endlet_return(SOBJ c, SOBJ argl)
{
  scm_asm_put_opc(c, SCM_OP_ENDLET);
  scm_asm_put_opc(c, SCM_OP_RETURN);
}


static void scm_asm_label(SOBJ c, SOBJ argl)
{
  int i, loc;
  SOBJ lab;

  lab = SCM_CAR(argl);
  scm_vector_append(scm_asm_known_labels, lab);
  scm_vector_append(scm_asm_known_labels, SCM_MKINUM(SCM_ASIZE(c)));

  /*-- resolv labels now */
  for (i = 0; i < SCM_ASIZE(scm_asm_unknown_labels); i += 2) {
	if (SCM_AREF(scm_asm_unknown_labels, i) == lab) {
	  loc = SCM_INUM(SCM_AREF(scm_asm_unknown_labels, i+1));
	  SCM_AREF(c, loc) = SCM_MKINUM(SCM_ASIZE(c) - loc);
	  SCM_AREF(scm_asm_unknown_labels, i) = SCM_MKINUM(0);
	}
  }
}

static void scm_asm_br_and(SOBJ c, SOBJ argl)
{
  scm_asm_put_opc(c, SCM_OP_BR_AND);
  scm_asm_put_lab_ref(c, SCM_CAR(argl));
}

static void scm_asm_br_or(SOBJ c, SOBJ argl)
{
  scm_asm_put_opc(c, SCM_OP_BR_OR);
  scm_asm_put_lab_ref(c, SCM_CAR(argl));
}

static void scm_asm_br_cond(SOBJ c, SOBJ argl)
{
  scm_asm_put_opc(c, SCM_OP_BR_COND);
  scm_asm_put_lab_ref(c, SCM_CAR(argl));
}

static void scm_asm_br_while(SOBJ c, SOBJ argl)
{
  scm_asm_put_opc(c, SCM_OP_BR_WHILE);
  scm_asm_put_lab_ref(c, SCM_CAR(argl));
}

static void scm_asm_brf(SOBJ c, SOBJ argl)
{
  SOBJ lab = SCM_CAR(argl);

  scm_asm_put_opc(c, SCM_OP_BRF);
  scm_asm_put_lab_ref(c, lab);
}

static void scm_asm_brt(SOBJ c, SOBJ argl)
{
  SOBJ lab = SCM_CAR(argl);

  scm_asm_put_opc(c, SCM_OP_BRT);
  scm_asm_put_lab_ref(c, lab);
}

/*I* (bra <label>) */
static void scm_asm_bra(SOBJ c, SOBJ argl)
{
  SOBJ lab = SCM_CAR(argl);

  scm_asm_put_opc(c, SCM_OP_BRA);
  scm_asm_put_lab_ref(c, lab);
}

/*I* (catch <label>) */
static void scm_asm_catch(SOBJ c, SOBJ argl)
{
  SOBJ lab = SCM_CAR(argl);
  scm_asm_put_opc(c, SCM_OP_CATCH);
  scm_asm_put_lab_ref(c, lab);
}

/*I* (uncatch) */
static void scm_asm_uncatch(SOBJ c, SOBJ argl)
{
  scm_asm_put_opc(c, SCM_OP_UNCATCH);
}

/*I* (load-r0) */
static void scm_asm_load_r0(SOBJ c, SOBJ argl)
{
  scm_asm_put_opc(c, SCM_OP_LOAD_R0);
}

/*I* (save-r0) */
static void scm_asm_save_r0(SOBJ c, SOBJ argl)
{
  scm_asm_put_opc(c, SCM_OP_SAVE_R0);
}

/*-- table to dispatch assembly */

static SOBJ
  op_nop, op_end, op_dolet, op_doletstar, op_endlet_jump, op_drop,
  op_pushq, op_pushv, op_pushl,
  op_store,
  op_setl,  op_setl0drop,
  op_getvar, op_setvar,
  op_mark, op_mkclosure, op_mkproc, op_mkcode,
  op_endlet, op_return, op_endlet_return,
  op_callp, op_callc, op_call, op_jump,
  op_br_and, op_br_or, op_br_cond, op_br_while, op_bra, op_brf, op_brt,
  op_catch, op_uncatch,
  op_label,
  op_save_r0,
  op_load_r0;


static SCM_ASM_INSTR scm_asm_table[] = {
  { "nop",			&op_nop,		scm_asm_nop 			},	
  { "end",			&op_end,		scm_asm_end 			},
  { "dolet",		&op_dolet,		scm_asm_dolet 			},
  { "dolet*",		&op_doletstar,	scm_asm_doletstar 		},
  { "endlet-jump",	&op_endlet_jump,scm_asm_endlet_jump 	},
  { "endlet-return",&op_endlet_return,scm_asm_endlet_return },
  { "drop",			&op_drop,		scm_asm_drop 			},
  { "pushq",		&op_pushq,		scm_asm_pushq, 			}, 	/* push quoted */
  { "pushv",		&op_pushv,		scm_asm_pushv, 			}, 	/* push global */
  { "pushl",		&op_pushl,		scm_asm_pushl			},	/* push local val*/
  { "store",		&op_store,		scm_asm_store 			},
  { "setl",			&op_setl,		scm_asm_setl 			},
  { "setl0drop",	&op_setl0drop,	scm_asm_setl0drop		},

  { "getvar",		&op_getvar,		scm_asm_getvar 			},
  { "setvar",		&op_setvar,		scm_asm_setvar 			},

  { "mark",			&op_mark,		scm_asm_mark 			},	/* push a mark */
  { "mkclosure",	&op_mkclosure,	scm_asm_mkclosure 		},
  { "mkproc",		&op_mkproc,		scm_asm_mkproc 			},	/* make a proc */
  { "mkcode",		&op_mkcode,		scm_asm_mkcode 			},  /* create a code */

  { "endlet",		&op_endlet,		scm_asm_endlet 			},	/* push a mark */
  { "return",		&op_return,		scm_asm_return 			},	/* push a mark */
  { "callp",		&op_callp,		scm_asm_callp 			},	/* call prim */
  { "callc",		&op_callc,		scm_asm_callc 			},	/* call c func */
  { "call",			&op_call,		scm_asm_call 			},	/* general call */
  { "jump",			&op_jump,		scm_asm_jump			},

  { "br-and",		&op_br_and,		scm_asm_br_and 			},
  { "br-or",		&op_br_or,		scm_asm_br_or 			},
  { "br-cond",		&op_br_cond,	scm_asm_br_cond			},
  { "br-while",		&op_br_while,	scm_asm_br_while		},
  { "bra",			&op_bra,		scm_asm_bra 			},
  { "brf",			&op_brf,		scm_asm_brf 			},
  { "brt",			&op_brt,		scm_asm_brt 			},

  { "catch",		&op_catch,		scm_asm_catch 			}, /* catch */
  { "uncatch",		&op_uncatch,	scm_asm_uncatch 		}, /* uncatch */

  { "label",		&op_label,		scm_asm_label 			},
  { "save-r0",		&op_save_r0,	scm_asm_save_r0			},
  { "load-r0",		&op_load_r0,	scm_asm_load_r0			},
  { NULL, NULL, NULL }
};


static void scm_asm_make_table()
{
  SCM_ASM_INSTR *p = scm_asm_table;

  while(p->name) {
	*p->atom = scm_mkatom(p->name);
	p++;
  }
}

static SCM_ASM_INSTR *scm_asm_search(SOBJ atom)
{
  SCM_ASM_INSTR *p;
  for (p = scm_asm_table; p->name != NULL; p++) {
	if (*p->atom == atom) return(p);
  }
  return(NULL);
}

static SOBJ scm_asm_dispatch(SOBJ c, SOBJ opcode, SOBJ arglist)
{
  SCM_ASM_INSTR *p;

  if ((p = scm_asm_search(opcode)) == NULL) 
	SCM_ERR("scm_asm_dispatch: bad instruction", opcode);

  if (arglist != NULL && !SCM_PAIRP(arglist))
	SCM_ERR("scm_asm_dispatch: bad argument list", scm_cons(opcode, arglist));

  (*p->func)(c, arglist);
  
  return(NULL);
}


/* assemble an instruction
 *
 * Instruction has form: (<opcode> [<arg>]*)
 */
SOBJ scm_asm_instr(SOBJ c, SOBJ expr)
{
  SOBJ op, arg;
  
  if (!SCM_PAIRP(expr)) SCM_ERR("asm: bad instruction format", expr);

  op = SCM_CAR(expr);
  arg = SCM_CDR(expr);

  scm_asm_dispatch(c, op, arg);

  return(NULL);
}

SOBJ scm_syntax_asm(SOBJ expr)
{
  SOBJ c = scm_asm_new_code();

  while(expr) {
	if (!SCM_PAIRP(expr))	SCM_ERR("asm: bad list", expr);
	scm_asm_instr(c, SCM_CAR(expr));
	expr = SCM_CDR(expr);
  }
#ifdef DEBUG_ASM
  scm_disassemble(SCM_ARRAY(c), SCM_ASIZE(c));
#endif
  return(NULL);
}

static SOBJ scm_asm_internal(SOBJ pcode, SOBJ icode)
{
  int i;

  for (i = 0; i < SCM_ASIZE(icode); i++) {
	scm_asm_instr(pcode, SCM_AREF(icode, i));
  }
  return(pcode);
}

static SOBJ scm_assemble(SOBJ icode)
{
  int i;
  SOBJ c;

  if (!SCM_ARRAYP(icode)) 	SCM_ERR("scm_assemble: bad icode", icode);

  c = scm_asm_new_code();
  scm_asm_known_labels = scm_mkarray(0, NULL);
  scm_asm_unknown_labels = scm_mkarray(0, NULL);

  scm_asm_internal(c, icode);

#ifdef DEBUG_ASM
  for (i = 0; i < SCM_ASIZE(scm_asm_known_labels); i += 2) {
	scm_puts("label ");
	scm_cdisplay(SCM_AREF(scm_asm_known_labels, i));
	scm_puts(" at offset ");
	scm_cdisplay(SCM_AREF(scm_asm_known_labels, i+1));
	scm_puts("\n");
  }
#endif

  for (i = 0; i < SCM_ASIZE(scm_asm_unknown_labels); i += 2) {
	if (SCM_AREF(scm_asm_unknown_labels, i) != SCM_MKINUM(0)) {
	  scm_puts("OUPS: label ");
	  scm_cdisplay(SCM_AREF(scm_asm_unknown_labels, i));
	  scm_puts(" used in code at offset ");
	  scm_cdisplay(SCM_AREF(scm_asm_unknown_labels, i+1));
	  scm_puts(" has not been resolved\n");
	}
  }
#ifdef DEBUG_ASM
  scm_disassemble(SCM_ARRAY(c), SCM_ASIZE(c));
#endif
  return(c);
}


/*
 * Compiler
 */

static void scm_asm_gen(SOBJ a, SOBJ list)
{
  SCM_Array *p = SCM_ADESCR(a);
  
  if (p->size >= p->alloced) {
	p = scm_must_realloc(p,(p->alloced *2*sizeof(SOBJ)) + sizeof(SCM_Array));
	p->alloced = p->alloced * 2;
	SCM_ADESCR(a) = p;
  }
  p->item[p->size++] = list;
}

static void scm_asm_gen1(SOBJ a, SOBJ item)
{
  scm_asm_gen(a, scm_cons(item, NULL));
}

static void scm_asm_gen2(SOBJ a, SOBJ n1, SOBJ n2)
{
  scm_asm_gen(a, SCM_LIST2(n1, n2));
}

static void scm_asm_gen3(SOBJ a, SOBJ n1, SOBJ n2, SOBJ n3)
{
  scm_asm_gen(a, SCM_LIST3(n1, n2, n3));
}

static void scm_asm_gen6(SOBJ a, SOBJ n1, SOBJ n2, SOBJ n3, SOBJ n4, SOBJ n5, SOBJ n6)
{
  scm_asm_gen(a, SCM_LIST6(n1, n2, n3, n4, n5, n6));
}

static SOBJ scm_compile_obj(SOBJ icode, SOBJ obj, SOBJ env);

static void scm_asm_gen_push_args(SOBJ icode, SOBJ argl, SOBJ env)
{
  int l;

  if (argl == NULL)	return;

  if (SCM_CDR(argl))	scm_asm_gen_push_args(icode, SCM_CDR(argl), env);

  l = scm_list_length(argl);
  if (l > scm_push_seq_max)		scm_push_seq_max = l;
  /* printf("; push seq length=%d max=%d\n", l, scm_push_seq_max); */
  scm_compile_obj(icode, SCM_CAR(argl), env);
}

static void scm_asm_gen_mark(SOBJ ic)
{
  scm_asm_gen1(ic, op_mark);
}

static void scm_asm_gen_call(SOBJ ic, int needmark, SOBJ sym, SOBJ argl, SOBJ env)
{
  if (needmark) scm_asm_gen_mark(ic);
  scm_asm_gen_push_args(ic, argl, env);
  scm_asm_gen2(ic, op_call, sym);
}

static void scm_asm_gen_callp(SOBJ ic, int needmark, SOBJ prim, SOBJ argl, SOBJ env)
{
  if (needmark) scm_asm_gen_mark(ic);
  scm_asm_gen_push_args(ic, argl, env);
  scm_asm_gen2(ic, op_callp, prim);
}
  
static void scm_asm_gen_callc(SOBJ ic, int needmark, SOBJ cprim, SOBJ argl, SOBJ env)
{
  if (needmark) scm_asm_gen_mark(ic);
  scm_asm_gen_push_args(ic, argl, env);
  scm_asm_gen2(ic, op_callc, cprim);
}
  

/************************************************************************
 * Compiler: common parts
 ************************************************************************/
/*F* (begin EXPR...) => ANY */
/*D* Evaluates EXPR sequencially from left to right. The value the last
  expression is returned */
/* NOTE: compile_begin is public, because it is needed by the module
 syntax */

SOBJ scm_compile_begin(SOBJ icode, SOBJ argl, SOBJ env)
{
  while(argl) {
	scm_compile_obj(icode, SCM_CAR(argl), env);
	argl = SCM_CDR(argl);
	if (argl != NULL) 	scm_asm_gen1(icode, op_drop);
  }
  return(icode);
}

/*-- lambda compiler -- itself */
static SOBJ scm_compile_lambda2(SOBJ icode, SOBJ formal, SOBJ body, SOBJ env)
{
  SOBJ newenv, code;
  int varnum, optargs;
  int nargs, nlocals;
  SOBJ l, sym, vlist;

  /* parse lambda arguments and create matching environement */
  varnum = 0;
  nargs = 0;  nlocals = 0;
  optargs = FALSE;
  newenv = scm_env_add_level(env);

  vlist = search_local(NULL, body);
#ifdef DEBUG
  if (vlist) {				/* local defines */
	scm_puts("; local define: ");	scm_cprint(vlist);
  }
#endif

  for (l = vlist; l; l = SCM_CDR(l)) {
#ifdef DEBUG
	scm_puts("; local def: ");  scm_cprint(SCM_CAR(l));
#endif
	newenv = scm_env_add(newenv, scm_mklsymbol(SCM_CAR(l), varnum++));
	nlocals++;
  }

  /* add local definitions to the env */
  l = formal;
  while(l && SCM_PAIRP(l)) {
	sym = SCM_CAR(l);  l = SCM_CDR(l);
	if (SCM_KEYWORDP(sym) && SCM_KEYW_NAME(sym) == scm_atom_local) {
		while(l && SCM_PAIRP(l) && !SCM_KEYWORDP(SCM_CAR(l))) {
		  scm_puts("local def: "); scm_cprint(SCM_CAR(l));
		  newenv = scm_env_add(newenv, scm_mklsymbol(SCM_CAR(l), varnum++));
		  nlocals++;
		  l = SCM_CDR(l);
		}
	}
  }
  
  /* add formal fixed arguments to the env */
  l = formal;
  while (l && SCM_PAIRP(l) && !SCM_KEYWORDP(SCM_CAR(l))) {
	newenv = scm_env_add(newenv, scm_mklsymbol(SCM_CAR(l), varnum++));
	nargs++;
	l = SCM_CDR(l);
  }

  /* handle optionnal parameters */
  if (l && SCM_ATOMP(l)) {		/* formal like (a b c . x) */
	newenv = scm_env_add(newenv, scm_mklsymbol(l, varnum++));
	optargs = TRUE;
	nargs++;
  } else {
	while(l && SCM_PAIRP(l) && SCM_KEYWORDP(SCM_CAR(l))) { /* (:keyw ...) */
	  sym = SCM_CAR(l);  l = SCM_CDR(l);

	  if (SCM_KEYW_NAME(sym) == scm_atom_rest) {
		/* register optionnal var */
		if (l && SCM_PAIRP(l)) {
		  newenv = scm_env_add(newenv, scm_mklsymbol(SCM_CAR(l), varnum++));
		  optargs = TRUE;
		  nargs++;
		  l = SCM_CDR(l);
		  continue;
		}
	  }
	  
	  if (SCM_KEYW_NAME(sym) == scm_atom_local) {
		/* ignore local var, because they are already defined */
		while(l && SCM_PAIRP(l) && !SCM_KEYWORDP(SCM_CAR(l))) {
		  l = SCM_CDR(l);
		}
		continue;
	  }
	}
	if (l != NULL) SCM_ERR("bad formal syntax", formal);
  }
  code = scm_compile_begin(scm_mkarray(0,NULL), body, newenv);
  scm_asm_gen1(code, op_return);
  scm_optimize1(code);
  scm_asm_gen6(icode, op_mkproc, newenv,
	   SCM_MKINUM(nargs), SCM_MKINUM(nlocals), SCM_MKBOOL(optargs),
	   code);
  scm_asm_gen1(icode, op_mkclosure);
  return(icode);
}

/*-- compile a set! -- scm_asm_generic */ 
static SOBJ scm_compile_set2(SOBJ icode, SOBJ var, SOBJ expr, SOBJ env)
{
  SOBJ sym;
  int depth;
 
  sym = lookup_atom(var, env, &depth, FALSE);
  if (sym == NULL) SCM_ERR("set! needs an existing symbol", var);
  switch(SCM_OBJTYPE(sym)) {
  case SOBJ_T_SYMBOL:
	if (SCM_VARP(SCM_SYM_VALUE(sym))) {
	  scm_compile_obj(icode, expr, env); /* compile the value */
	  scm_asm_gen2(icode, op_pushq, SCM_SYM_VALUE(sym));
	  scm_asm_gen1(icode, op_setvar);
	  break;
	}
	scm_compile_obj(icode, expr, env);
	scm_asm_gen2(icode, op_pushq, sym);
	scm_asm_gen1(icode, op_store);
	break;

  case SOBJ_T_LSYMBOL:
	scm_compile_obj(icode, expr, env);
	scm_asm_gen3(icode, op_setl, SCM_MKINUM(SCM_LSYM_OFS(sym)), SCM_MKINUM(depth));
	break;

  default:
	SCM_ERR("set!: bad symbol", var);
  }  
  return(icode);
}

/*F* (if TEST CONSEQUENT [ALTERNATE]) => OBJ */
/*D* Evaluates CONSEQUENT if the TEST expression evaluates to
  #t. Otherwise evaluates ALTERNATE. */
static SOBJ scm_compile_if(SOBJ icode, SOBJ argl, SOBJ env)
{
  int len = scm_list_length(argl);
  SOBJ l1, l2;					/* labels */

  if (len != 2 && len != 3)
	SCM_ERR("scm_compile_if: (if <expr> <true> [ <false> ])", argl);

  if (len == 2) {				/* (if <expr> <true>) */
	l1 = next_label();
	scm_compile_obj(icode, SCM_CAR(argl), env);
	scm_asm_gen2(icode, op_br_and, l1);
	scm_compile_obj(icode, SCM_CADR(argl), env);
	scm_asm_gen2(icode, op_label, l1);
	return(icode);
  }
  l1 = next_label();  l2 = next_label();
  scm_compile_obj(icode, SCM_CAR(argl), env);
  scm_asm_gen2(icode, op_brf, l1);
  scm_compile_obj(icode, SCM_CADR(argl), env);
  scm_asm_gen2(icode, op_bra,   l2);
  scm_asm_gen2(icode, op_label, l1);
  scm_compile_obj(icode, SCM_CADDR(argl), env);
  scm_asm_gen2(icode, op_label, l2);
  return(icode);
}

/*F* (define VAR [EXPR]) => EXPR */
/*D* Create a new variable VAR, bind it to a fresh location and store
  the value of EXPR. If EXPR is not given, #unbound is assigned */

/*F* (define (VAR FORMAL) BODY) => PROC */
/*D* Create a variable VAR and assign the procedure equivalent to
  (lambda (FORMAL) BODY). */

/*E* (define (VAR ARG [:local LOC] [:rest SYM]) BODY) => PROC */
/*D* Same as define but also creates local variables and a binding for
  the rest of arguments */

static SOBJ scm_compile_define(SOBJ icode, SOBJ argl, SOBJ env)
{
  SOBJ var, sym;
  int depth;

  var = SCM_CAR(argl);

  if (SCM_SYMBOLP(var))			/* obtain atom from symbol */
	var = SCM_SYM_NAME(var);

  if (SCM_ATOMP(var)) {

	/* force creation of symbol in case it does not exist */
	sym = lookup_atom(var, env, &depth, TRUE);


	/* compile the code scm_asm_generating the value of the symbol */
	if (!SCM_PAIRP(SCM_CDR(argl))) {
	  /* (define x) => (define x #unbound) */
	  scm_asm_gen2(icode, op_pushq, scm_unbound);
	} else {
	  scm_compile_obj(icode, SCM_CADR(argl), env);
	}

	if (SCM_LSYMBOLP(sym)) {	/* local symbol */
	  scm_asm_gen3(icode, op_setl, SCM_MKINUM(SCM_LSYM_OFS(sym)), SCM_MKINUM(depth));
	} else {					/* global symbol */
	  if (SCM_SYNTAXP(SCM_SYM_VALUE(sym))) SCM_ERR("syntax redefinition:", sym);
	  scm_asm_gen2(icode, op_pushq, sym);  scm_asm_gen1(icode, op_store);
	}
	return(icode);
  }

  if (SCM_PAIRP(var)) {			/* (define (var formals) body) */
	SOBJ body, formal;

	formal = SCM_CDR(var);		/* need check */
	var    = SCM_CAR(var);
	body   = SCM_CDR(argl);		/* need check */
#ifdef OLD
	vlist  = search_local(NULL, body);
	if (vlist) {				/* local definitions */
	  scm_puts("; localy defined variables: ");	scm_cprint(vlist);
	  scm_puts("; have to create environment for this variables\n");
	  formal = scm_append2(formal,
						   scm_cons(scm_mkkeyword2(scm_atom_local,
												   scm_unbound),
									vlist));
	  scm_puts("; new formal: "); scm_cprint(formal);
	}
#endif
	/* force creation of symbol in case it does not exist */
	sym = lookup_atom(var, env, &depth, TRUE);

	scm_compile_lambda2(icode, formal, body, env);

	if (SCM_LSYMBOLP(sym)) {	/* local symbol here */
	  scm_asm_gen3(icode, op_setl, SCM_MKINUM(SCM_LSYM_OFS(sym)), SCM_MKINUM(depth));
	} else {	
	  scm_asm_gen2(icode, op_pushq, sym);
	  scm_asm_gen1(icode, op_store);
	}
	return(icode);

  }
  SCM_ERR("scm_compile_define: cannot compile", argl);
  return(NULL);		/* keep compiler silent */
}

/*-- compile a set expr */
/*F* (set! VAR EXPR) => VALUE */
/*D* Evaluates EXPR and stores the resulting value in the location to
  which VAR is bound. */
static SOBJ scm_compile_set(SOBJ icode, SOBJ argl, SOBJ env)
{
  SOBJ var, expr;

  var = SCM_CAR(argl);
  expr = SCM_CADR(argl);
  return(scm_compile_set2(icode, var, expr, env));
}

static SOBJ scm_compile_named_let(SOBJ icode, SOBJ name, SOBJ argl, SOBJ env)
{
  SOBJ bind, body, newenv, obj, pair;
  SOBJ larg, vals, *pn;

  bind = SCM_CAR(argl);
  body = SCM_CDR(argl);

  /* make the binding for the lambda code */
  newenv = scm_env_add_level(env);
  newenv = scm_env_add(newenv, scm_mklsymbol(name, 0));
  scm_asm_gen2(icode, op_doletstar, SCM_MKINUM(1));

  /* build a list of arg for the lambda */
  larg = NULL;
  pn = &larg;
  for (obj = bind; obj; obj = SCM_CDR(obj)) {
	pair = SCM_CAR(obj);
	if (!SCM_PAIRP(obj))	SCM_ERR("let: incorrect binding", obj);
	*pn = scm_cons(SCM_CAR(pair), NULL);
	pn  = &SCM_CDR(*pn);
  }
  /*  scm_puts("named-let: lambda args=");  scm_cprint(larg); */
  scm_compile_lambda2(icode, larg, body, newenv);
  scm_asm_gen3(icode, op_setl0drop, SCM_MKINUM(0), SCM_MKINUM(0));

  /* push the argument for the lambda call */
  scm_asm_gen_mark(icode);
  vals = NULL;
  for (obj = bind; obj; obj = SCM_CDR(obj)) {
	pair = SCM_CAR(obj);
	if (!SCM_PAIRP(obj))	SCM_ERR("let: incorrect binding", obj);
	vals = scm_cons(SCM_CDR(pair) ? SCM_CADR(pair) : scm_unbound, vals);
  }

  for (obj = vals; obj; obj = SCM_CDR(obj)) {
	scm_compile_obj(icode, SCM_CAR(obj), newenv);
  }
  scm_asm_gen3(icode, op_pushl, SCM_MKINUM(0), SCM_MKINUM(0));

  /* call this lambda */
  scm_asm_gen1(icode, op_call);

  scm_asm_gen1(icode, op_endlet);
  return(icode);
}


/*-- 	(let BINDING EXPR...) 
 * or 	(let NAME BINDING EXPR)
 */
/*F* (let BINDING EXPR...) => OBJ */
/*D* Creates local variables as described in BINDING and evaluates in
  the new environemnent the expressions. Returns the value of last
  EXPR. The BINDING are evaluated using the enclosing environment */

/*F* (let NAME BINDING EXPR...) => OBJ */
/*D* Does the same as LET except that it binds NAME to the body of the
  let construct. Thus the execution of the body EXPRs may be repeated
  by invoking the procedure named NAME. */
static SOBJ scm_compile_let(SOBJ icode, SOBJ argl, SOBJ env)
{
  SOBJ bind, expr, newenv, obj, pair;
  int varnum;
  SOBJ atom;

  if (SCM_ATOMP(SCM_CAR(argl))) { /* named let?*/
	return(scm_compile_named_let(icode, SCM_CAR(argl), SCM_CDR(argl), env));
  }
  bind = SCM_CAR(argl);
  expr = SCM_CDR(argl);

  if (search_local(NULL, expr) != NULL) {
	return(scm_compile_letrec(icode, argl, env));
  }

  newenv = scm_env_add_level(env); /* add a new level of env */
  varnum = 0;
  atom = NULL;

  /* create the new environment */
  for (obj = bind; obj; obj = SCM_CDR(obj)) {
	pair = SCM_CAR(obj);
	if (!SCM_PAIRP(pair)) 			SCM_ERR("let: incorrect binding", obj);
	if (SCM_ATOMP(SCM_CAR(pair)))
	  atom = SCM_CAR(pair);
	else if (SCM_SYMBOLP(SCM_CAR(pair)))
	  atom = SCM_SYM_NAME(SCM_CAR(pair));
	else
	  SCM_ERR("let: bad local var", SCM_CAR(pair));
	newenv = scm_env_add(newenv, scm_mklsymbol(atom, varnum++));
  }

  /* compile initialisation code */
  scm_asm_gen1(icode, op_mark);
  for (obj = scm_reverse(bind); obj; obj = SCM_CDR(obj)) {
	pair = SCM_CAR(obj);
	if (SCM_CDR(pair)) {
	  scm_compile_obj(icode, SCM_CADR(pair), env);
	} else {
	  scm_asm_gen2(icode, op_pushq, scm_unbound);
	}
  }
  scm_asm_gen1(icode, op_dolet);
  scm_compile_begin(icode, expr, newenv);
  scm_asm_gen1(icode, op_endlet);
  /* scm_optimize1(icode); */
  return(icode);
}

/*-- (let* BINDING EXPR) */
/*F* (let* ((VAR INIT) ...) BODY) => OBJ */
/*D* Let* is similar to let, but the bindings are performed
  sequentially from left to right, and the region of a binding
  indicated by (VAR INIT) is that part of the let* expression to the
  right of the binding.  Thus the second binding is done in an
  environment in which the first binding is visible, and so on. */
static SOBJ scm_compile_letstar(SOBJ icode, SOBJ argl, SOBJ env)
{
  SOBJ bind, expr, newenv, obj, pair;
  SOBJ vlist;
  int varnum;

  bind = SCM_CAR(argl);
  expr = SCM_CDR(argl);

  newenv = scm_env_add_level(env); /* add a new level of env */
  varnum = 0;

  /* create the new environment */
  for (obj = bind; obj; obj = SCM_CDR(obj)) {
	pair = SCM_CAR(obj);
	if (!SCM_PAIRP(pair)) 			SCM_ERR("let: incorrect binding", obj);
	if (!SCM_ATOMP(SCM_CAR(pair)))	SCM_ERR("let: bad local var", SCM_CAR(pair));
	newenv = scm_env_add(newenv, scm_mklsymbol(SCM_CAR(pair), varnum++));
  }
  /* add local defs to env */
  vlist = search_local(NULL, expr);
  for (obj = vlist; obj; obj = SCM_CDR(obj)) {
	newenv = scm_env_add(newenv, scm_mklsymbol(SCM_CAR(obj), varnum++));
  }
  
  /* header of let* */
  scm_asm_gen2(icode, op_doletstar, SCM_MKINUM(varnum));
  
  /* compile initialisation code */
  varnum = 0;
  for (obj = bind; obj; obj = SCM_CDR(obj)) {
	pair = SCM_CAR(obj);
	if (SCM_CDR(pair)) {
	  scm_compile_obj(icode, SCM_CADR(pair), newenv);
	} else {
	  scm_asm_gen2(icode, op_pushq, scm_unbound);
	}
	scm_asm_gen2(icode, op_setl0drop, SCM_MKINUM(varnum++));
  }
  /* set unbound in local defs */
  for (obj = vlist; obj; obj = SCM_CDR(obj)) {
	scm_asm_gen2(icode, op_pushq, scm_unbound);
	scm_asm_gen2(icode, op_setl0drop, SCM_MKINUM(varnum++));
  }

  /* compile the body */
  scm_compile_begin(icode, expr, newenv);
  scm_asm_gen1(icode, op_endlet);
  /* scm_optimize1(icode); */
  return(icode);
}

/*F* (letrec ((VAR INIT) ...) BODY) => OBJ */
/*D* The VARs are bound to fresh locations holding undefined values,
  the INITs are evaluated in the resulting environment (in some
  unspecified order), each VAR is assigned to the result of the
  corresponding INIT, the BODY is evaluated in the resulting
  environment, and the value of the last expression in BODY is
  returned. Each binding of a VAR has the entire letrec expression as
  its region, making it possible to define mutually recursive
  procedures. */

static SOBJ scm_compile_letrec(SOBJ icode, SOBJ argl, SOBJ env)
{
  SOBJ bind, expr, newenv, obj, pair;
  int varnum;
  SOBJ vlist;

  bind = SCM_CAR(argl);
  expr = SCM_CDR(argl);

  newenv = scm_env_add_level(env); /* add a new level of env */
  varnum = 0;

  /*-- add variable to the new env */
  for (obj = bind; obj; obj = SCM_CDR(obj)) {
	pair = SCM_CAR(obj);
	if (!SCM_PAIRP(pair))			SCM_ERR("letrec: incorrect binding", obj);
	if (!SCM_ATOMP(SCM_CAR(pair)))	SCM_ERR("letrec: bad local var", SCM_CAR(pair));
	newenv = scm_env_add(newenv, scm_mklsymbol(SCM_CAR(pair), varnum++));
  }
  /* add local defs to env */
  vlist = search_local(NULL, expr);
  for (obj = vlist; obj; obj = SCM_CDR(obj)) {
	newenv = scm_env_add(newenv, scm_mklsymbol(SCM_CAR(obj), varnum++));
  }

  /*-- compile code to alloc space for the new variables */
  scm_asm_gen2(icode, op_doletstar, SCM_MKINUM(varnum));

  /*-- compile code to scm_asm_generate */
  varnum = 0;
  for (obj = bind; obj; obj = SCM_CDR(obj)) {
	pair = SCM_CAR(obj);
	if (SCM_CDR(pair)) {
	  scm_compile_obj(icode, SCM_CADR(pair), newenv);
	} else {
	  scm_asm_gen2(icode, op_pushq, scm_unbound);
	}
	scm_asm_gen2(icode, op_setl0drop, SCM_MKINUM(varnum++));
  }
  /* set unbound in local defs */
  for (obj = vlist; obj; obj = SCM_CDR(obj)) {
	scm_asm_gen2(icode, op_pushq, scm_unbound);
	scm_asm_gen2(icode, op_setl0drop, SCM_MKINUM(varnum++));
  }
  /* compile the body */
  scm_compile_begin(icode, expr, newenv);
  scm_asm_gen1(icode, op_endlet);
  /* scm_optimize1(icode); */
  return(icode);
}

/*-- lambda compiler -- front end */
/*F* (lambda FORMALS BODY) => PROCEDURE */
/*D* Returns a new procedure. The environment in effect when the
  lambda expression was evaluated is remembered as part of the
  procedure.  When the procedure is later called with some actual
  arguments, the environment in which the lambda expression was
  evaluated will be extended by binding the variables in the FORMAL
  argument list to fresh locations, the corresponding actual argument
  values will be stored in those locations, and the expressions in the
  BODY of the lambda expression will be evaluated sequentially in the
  extended environment.  The result of the last expression in the
  BODY will be returned as the result of the procedure call. */

static SOBJ scm_compile_lambda(SOBJ icode, SOBJ argl, SOBJ env)
{
  SOBJ formal, body;
  
  formal = SCM_CAR(argl);
  body = SCM_CDR(argl);
  return(scm_compile_lambda2(icode, formal, body, env));
}

/*-- compiler for quote */
/*F* (quote X) => X */
/*D* Return X, unevaluated */

static SOBJ scm_compile_quote(SOBJ icode, SOBJ argl, SOBJ env)
{
  scm_asm_gen2(icode, op_pushq, SCM_CAR(argl));
  return(icode);
}

/*-- KKK: should try to compile parial constant list */
SOBJ backquotify(SOBJ ic, SOBJ l, SOBJ env, int level)
{
  static SOBJ sym_cons, sym_append, sym_list2, sym_list2vec;

  if (sym_cons == NULL) {
	SOBJ prim(char *x) {
	  int dummy;
	  return(SCM_SYM_VALUE(lookup_atom(scm_mkatom(x),NULL,&dummy,FALSE)));
	}
	sym_cons   = prim("cons");
	sym_list2  = prim("list2");
	sym_append = prim("qq-append2");
	sym_list2vec = prim("list->vector");
  }

  if (SCM_ARRAYP(l)) {
	backquotify(ic, scm_vector_to_list(l), env, level);
	scm_asm_gen2(ic, op_callc, sym_list2vec);
	return(ic);
  }

  if (!SCM_PAIRP(l)) {
	scm_asm_gen2(ic, op_pushq, l);
	return(ic);
  }

  if (scm_eqv(SCM_CAR(l), scm_sym_qquote) != scm_false) {
	backquotify(ic, SCM_CADR(l), env, level+1);
	scm_asm_gen2(ic, op_pushq, scm_sym_qquote);
	scm_asm_gen2(ic, op_callp, sym_list2);
	return(ic);
  }

  if (scm_eqv(SCM_CAR(l), scm_sym_unquote) != scm_false) {
	if (level == 1) {
	  scm_compile_obj(ic, SCM_CADR(l), env);
	} else {
	  backquotify(ic, SCM_CADR(l), env, level-1);
	  scm_asm_gen2(ic, op_pushq, scm_sym_unquote);
	  scm_asm_gen2(ic, op_callp, sym_list2);
	}
	return(ic);
  }

  if (SCM_PAIRP(SCM_CAR(l)) &&
	  scm_eqv(SCM_CAAR(l), scm_sym_unquote_splicing) != scm_false) {
	
	if (SCM_CDR(l)) 	backquotify(ic, SCM_CDR(l), env, level);
	scm_compile_obj(ic, SCM_CAR(SCM_CDAR(l)), env);
	if (SCM_CDR(l)) 	scm_asm_gen2(ic, op_callp, sym_append);
	return(ic);
  }
  backquotify(ic, SCM_CDR(l), env, level);
  backquotify(ic, SCM_CAR(l), env, level);
  scm_asm_gen2(ic, op_callp, sym_cons);
  return(ic);
}


/*-- compiler for quasiquote */
/*F* (quasiquote TEMPLATE) => LIST */
/*D* If no comma appears in the TEMPLATE, just behaves like
  quote. Otherwise the values following the comma are evaluated and
  the result is inserted in place. */

static SOBJ scm_compile_qquote(SOBJ icode, SOBJ argl, SOBJ env)
{
  /* scm_asm_gen2(icode, op_pushq, SCM_CAR(argl)); */
  backquotify(icode, SCM_CAR(argl), env, 1);
  /*  scm_cprint(backquotify(icode, SCM_CAR(argl), env, 1)); */
  return(icode);
}

/*-- compiler for the-env */
/*F* (the-env) => ENV */
/*D* Returns the current compilation environment */
static SOBJ scm_compile_the_env(SOBJ icode, SOBJ argl, SOBJ env)
{
  scm_asm_gen2(icode, op_pushq, env);
  return(icode);
}


/*-- compiler for and */
/*F* (and TEST1 ...) => OBJ */
/*D* Expressions are evaluated from left to right, and the value of
  the first expression that evaluates to a false is returned. Any
  remaining expressions are not evaluated.  If all the expressions
  evaluate to true values, the value of the last expression is
  returned. If there are no expressions then #t is returned.*/

static SOBJ scm_compile_and(SOBJ icode, SOBJ argl, SOBJ env)
{
  SOBJ lab;

  if (argl == NULL) {
	scm_asm_gen2(icode, op_pushq, scm_true);
  } else {
	lab = next_label();
	while(1) {
	  scm_compile_obj(icode, SCM_CAR(argl), env);
	  if ((argl = SCM_CDR(argl)) == NULL) break;
	  scm_asm_gen2(icode, op_br_and, lab);
	}
	scm_asm_gen2(icode, op_label, lab);
  }
  return(icode);
}

/*F* (or TEST1 ...) => OBJ */
/*D* expressions are evaluated from left to right, and the value of
  the first expression that evaluates to a true is returned. Any
  remaining expressions are not evaluated.  If all expressions
  evaluate to false values, the value of the last expression is
  returned. If there are no expressions then #f is returned. */

/*-- compiler for or */
static SOBJ scm_compile_or(SOBJ icode, SOBJ argl, SOBJ env)
{
  SOBJ lab;

  if (argl == NULL) {
	scm_asm_gen2(icode, op_pushq, scm_false);
  } else {
	lab = next_label();
	while(1) {
	  scm_compile_obj(icode, SCM_CAR(argl), env);
	  if ((argl = SCM_CDR(argl)) == NULL) break;
	  scm_asm_gen2(icode, op_br_or, lab);
	}
	scm_asm_gen2(icode, op_label, lab);
  }
  return(icode);
}

/*-- compiler for cond */
/*F* (cond (TEST EXPR...) ... [(else EXPR...)] ) => OBJ */
/*D* Each TEST is evaluated until one evaluates to true or the final
  else is reached. When TEST is true, the rest of EXPR is evaluated
  and the result of last is returned. */

static SOBJ scm_compile_cond(SOBJ icode, SOBJ argl, SOBJ env)
{
  static SOBJ sym_else, sym_impl;
  SOBJ clause, exitlab, lab;

  if (sym_else == NULL) { sym_else = scm_mkatom("else"); }
  if (sym_impl == NULL) { sym_impl = scm_mkatom("=>"); }

  if (argl == NULL) {
	scm_asm_gen2(icode, op_pushq, scm_undefined);
  } else {
	exitlab = next_label();
	lab = NULL;
	while(argl) {
	  clause = SCM_CAR(argl);  argl   = SCM_CDR(argl);

	  if (SCM_CAR(clause) == sym_else) { /* else clause */
		if (argl)  SCM_ERR("cond: else clause must be the last:", argl);
		if (lab) scm_asm_gen2(icode, op_label, lab);
		lab = NULL;				/* disable generation of default case */
		scm_compile_begin(icode, SCM_CDR(clause), env);

	  } else {					/* regular clause */
		if (lab) scm_asm_gen2(icode, op_label, lab);
		scm_compile_obj(icode, SCM_CAR(clause), env); /* compile test */
		lab = next_label();
		if (SCM_CDR(clause) != NULL) {
		  if (SCM_CADR(clause) == sym_impl) {
			SOBJ sym = SCM_CADDR(clause);

			/* this is a little tricky:
			 * - compile save-r0
			 * - compile funcall: (sym '())
			 * - replace code for argument pushing with load-r0
			 * Note: optimizer will strip sequences like (save-r0) (load-r0)
			 */
			scm_asm_gen2(icode, op_br_cond, lab);
			scm_asm_gen1(icode, op_save_r0);
			scm_compile_funcall2(icode, sym, scm_cons(NULL, NULL), env);
			SCM_AREF(icode, SCM_ASIZE(icode) - 2) = scm_cons(op_load_r0, NULL);
		  } else {
			/* compile exprlist */
			scm_asm_gen2(icode, op_brf, lab);
			scm_compile_begin(icode, SCM_CDR(clause), env);
		  }
		} else {
		  scm_asm_gen2(icode, op_br_cond, lab);
		}
		scm_asm_gen2(icode, op_bra,  exitlab);
	  }
	}
	if (lab) {					/* generate default */
	  scm_asm_gen2(icode, op_label, lab);
	  scm_asm_gen2(icode, op_pushq, scm_false);
	}
	scm_asm_gen2(icode, op_label, exitlab);
  }
  return(icode);
}

/*-- compile helper for while and until */
static SOBJ scm_compile_loop(SOBJ icode, SOBJ argl, SOBJ env, SOBJ branch_op)
{
  SOBJ test_lab = next_label();
  SOBJ loop_lab = next_label();

  scm_asm_gen2(icode, op_pushq, scm_undefined);
  scm_asm_gen2(icode, op_bra,   test_lab);
  scm_asm_gen2(icode, op_label, loop_lab);
  scm_asm_gen1(icode, op_drop);
  scm_compile_begin(icode, SCM_CDR(argl), env);
  scm_asm_gen2(icode, op_label,	test_lab);
  scm_compile_obj(icode, SCM_CAR(argl), env);
  scm_asm_gen2(icode, branch_op, 	loop_lab);

  return(icode);
}

/*E* (while TEST EXPR ...) => OBJ */
/*D* Evaluates the EXPR while TEST evaluates to TRUE. */

static SOBJ scm_compile_while(SOBJ icode, SOBJ argl, SOBJ env)
{
  if (scm_list_length(argl) < 2)
	SCM_ERR("while: syntax <test> [<expr>]+", argl);

  return(scm_compile_loop(icode, argl, env, op_brt));
}

/*E* (until TEST EXPR ...) => OBJ */
/*D* Evaluates the EXPR until TEST evaluates to TRUE. */

static SOBJ scm_compile_until(SOBJ icode, SOBJ argl, SOBJ env)
{
  if (scm_list_length(argl) < 2)
	SCM_ERR("until: syntax <test> [<expr>]+", argl);

  return(scm_compile_loop(icode, argl, env, op_brf));
}

/*-- compiler for do expressions */
/*F* (do ((VAR INIT STEP)...)(TST EXPR...) CMD...) => OBJ */
/*D* Binds VAR to INIT and start iterate. First evaluate TEST. If TEST
  is true, then EXPR are evaluated and the result of last EXPR is
  returned. If TST is false then CMD are evaluated and the iteration
  restarts by evaluating the STEP expr and binding the result to VAR
  again. */

static SOBJ scm_compile_do(SOBJ icode, SOBJ argl, SOBJ env)
{
  SOBJ i_list, t_list, newenv, obj, decl; SOBJ loop_label, exit_label;
  SOBJ v_list; SOBJ enclosing_env; int varnum;

  if (scm_list_length(argl) < 2)
    SCM_ERR("do: syntax (do (<iteration spec>*) (<test> <do result>) <command>*)",
		argl);

  i_list = SCM_CAR(argl);
  t_list = SCM_CADR(argl);

#ifdef DEBUG
  scm_puts("iteration list=");  scm_cdisplay(i_list);
  scm_puts(", test list=");     scm_cprint(t_list);
#endif

  /* build the new environement for the body */
  newenv = scm_env_add_level(env);
  varnum = 0;
  for (obj = i_list; obj; obj = SCM_CDR(obj)) {
    decl = SCM_CAR(obj);
    if (!SCM_PAIRP(decl))  SCM_ERR("do: incorrect iterator list", decl);
    newenv = scm_env_add(newenv, scm_mklsymbol(SCM_CAR(decl), varnum++));
  }
#ifdef DEBUG
  scm_puts("newenv=");  scm_cprint(newenv);
#endif

  /* alloc space for loop variables */
  scm_asm_gen2(icode, op_doletstar, SCM_MKINUM(varnum));

  /* generate initialisation code for each variable */
  enclosing_env = scm_env_add_level(env);
  varnum = 0;
  for (obj = i_list; obj; obj = SCM_CDR(obj)) {
    decl = SCM_CAR(obj);
    if (SCM_CDR(decl) == NULL) SCM_ERR("do: bad iterator init", decl);
	/* init code is runned in the enclosing env */
    scm_compile_obj(icode, SCM_CADR(decl), enclosing_env);
	scm_asm_gen2(icode, op_setl0drop, SCM_MKINUM(varnum++));
  }
  /* get 2 labels: one for the loop, one for the exit */
  loop_label = next_label();
  exit_label = next_label();

  /* the test part */
  scm_asm_gen2(icode, op_label, loop_label);
  scm_compile_obj(icode, SCM_CAR(t_list), newenv);
  scm_asm_gen2(icode, op_brt, exit_label);

  /* the loop body */
  if (SCM_CDDR(argl) != NULL) {
    scm_compile_begin(icode, SCM_CDDR(argl), newenv);
    scm_asm_gen1(icode, op_drop);
  }

#ifdef OLD_DO
  varnum = 0;
  for (obj = i_list; obj; obj = SCM_CDR(obj)) {
    decl = SCM_CAR(obj);
    if (SCM_CDDR(decl)) {
      scm_compile_obj(icode, SCM_CADDR(decl), newenv);
	  scm_asm_gen2(icode, op_setl0drop, SCM_MKINUM(varnum));
    }
    varnum++;
  }
#else
  v_list = NULL;
  varnum = 0;
  for (obj = i_list; obj; obj = SCM_CDR(obj)) {
    decl = SCM_CAR(obj);
    if (SCM_CDDR(decl)) {
      scm_compile_obj(icode, SCM_CADDR(decl), newenv);
	  v_list = scm_cons(SCM_MKINUM(varnum), v_list);
    }
    varnum++;
  }
  for (obj = v_list; obj; obj = SCM_CDR(obj)) {
	scm_asm_gen2(icode, op_setl0drop, SCM_CAR(obj));
  }
#endif
  scm_asm_gen2(icode, op_bra, loop_label);

  /* generate the return code here */
  scm_asm_gen2(icode, op_label, exit_label);
  if (SCM_CDR(t_list)) {
    scm_compile_obj(icode, SCM_CADR(t_list), newenv);
  } else {
    scm_asm_gen2(icode, op_pushq, scm_true);
  }
  scm_asm_gen1(icode, op_endlet);
  return (icode);
}


/*-- compiler for catch */
/*F* (catch TAG HANDLER EXPR...) => OBJ */
/*D* Catch exceptions occuring during evaluation of the EXPR. TAG is
  either a list of symbol or #t or #f. HANDLER is a function to handle
  the exception. When an execption occurs, the TAG list is search for
  a matching symbol and the HANDLER procedure is called. */
static SOBJ scm_compile_catch(SOBJ icode, SOBJ argl, SOBJ env)
{
  SOBJ tag, thunk, expr, lab;

  tag = SCM_CAR(argl);
  thunk = SCM_CADR(argl);
  expr = SCM_CDDR(argl);
  scm_compile_obj(icode, thunk, env);
  scm_asm_gen2(icode, op_pushq, tag);
  lab = next_label();
  scm_asm_gen2(icode, op_catch, lab);
  scm_compile_begin(icode, expr, env);
  scm_asm_gen1(icode, op_uncatch);
  scm_asm_gen2(icode, op_label, lab);
  return(icode);
}

#ifdef COMMENT
/*-- compiler for xxx */
static SOBJ scm_compile_xxx(SOBJ icode, SOBJ argl, SOBJ env)
{
  return(icode);
}
#endif
/*KKK: end of the scm_compile_xxx section */

/*-- icode */
static void err_bad_arg_count(SOBJ sym, int wanted, int got)
{
  char buf[128];
  sprintf(buf, "bad argument count (expected %d, got %d) for", wanted, got);
  SCM_ERR(buf, sym);
}

static void check_prim_arg_count(SOBJ sym, SOBJ argl)
{
  SOBJ prim = SCM_SYM_VALUE(sym);
  int nargs = SCM_PRIM(prim)->nargs;
  int got_args;

  if (nargs < 0)	return;

  got_args = scm_list_length(argl);

  if (nargs != got_args)
	err_bad_arg_count(sym, SCM_PRIM(prim)->nargs, got_args);
}

static void check_cprim_arg_count(SOBJ sym, SOBJ argl)
{
  SOBJ cprim = SCM_SYM_VALUE(sym);
  int  nargs = SCM_CPRIM_NARGS(cprim);
  int  got_args;

  if (nargs < 0)	return;
  if (nargs > SCM_OP_CALLC_MAX)
	SCM_ERR("callc does not support all this args", sym);
  
  got_args = scm_list_length(argl);
  if (nargs != got_args) 
	err_bad_arg_count(sym, nargs, got_args);
}

/*E* (execute-macro MACRO FORM ENV) => OBJ */
static SOBJ execute_macro(SOBJ macro, SOBJ form, SOBJ env)
{
  SOBJ code = SCM_MACRO_CODE(macro);
  SOBJ vmcode[] = {
	SCM_OPCODE(SCM_OP_MARK),
	SCM_OPCODE(SCM_OP_PUSH), form,
	SCM_OPCODE(SCM_OP_PUSH), code,
	SCM_OPCODE(SCM_OP_CALL),
	SCM_OPCODE(SCM_OP_END)
  };
  return(scm_run_engine(vmcode));
}

/*E* (macro-expand EXPR ENV) => OBJ */
static SOBJ macroexpand(SOBJ expr, SOBJ env)
{
  if (SCM_PAIRP(expr) && SCM_MACROP(SCM_CAR(expr))) {
	return(execute_macro(SCM_CAR(expr), expr, env));
  }
  return(expr);
}

static SOBJ scm_compile_funcall2(SOBJ icode, SOBJ func, SOBJ argl, SOBJ env)
{
  int depth;
  SOBJ v, sym;

  sym = SCM_SYMBOLP(func) ? func : lookup_atom(func, env, &depth, TRUE);

  if (!SCM_SYMBOLP(sym) && !SCM_LSYMBOLP(sym))
	SCM_ERR("scm_compile_funcall: not a symbol, don't know what to compile", sym);

  if (SCM_LSYMBOLP(sym)) {
	/* fetch value and call */
	scm_asm_gen_mark(icode);
	scm_asm_gen_push_args(icode, argl, env);
	scm_asm_gen3(icode, op_pushl, SCM_MKINUM(SCM_LSYM_OFS(sym)),SCM_MKINUM(depth));
	scm_asm_gen1(icode, op_call);
	return(icode);
  }

  /* ok, it's a global symbol.
	 try to scm_asm_generate better code based upon the fact that some symbol
     are bound to primitive, cprim, macros, syntax */

  v = SCM_SYM_VALUE(sym);		/* load binding */

  switch(SCM_OBJTYPE(v)) {		/* test it's binding */
  case SOBJ_T_PRIM:				/* vm primitive */
	check_prim_arg_count(sym, argl);
	scm_asm_gen_callp(icode, (SCM_PRIM(v)->nargs < 0), v, argl, env);
	return(icode);
		
  case SOBJ_T_CPRIM:
	check_cprim_arg_count(sym, argl);
	scm_asm_gen_callc(icode, (SCM_CPRIM_NARGS(v) < 0), v, argl, env);
	return(icode);
	  
  case SOBJ_T_MACRO:
	/*
	  scm_puts("; before execute_macro: "); scm_cprint(form);	
	  l = execute_macro(v, form, env);
	  scm_puts("; execute_macro returns: "); scm_cprint(l);
	  return(scm_compile_obj(icode, l, env));
	*/
	return(scm_compile_obj(icode,
						   execute_macro(v, scm_cons(sym, argl), env), env));

  case SOBJ_T_SYNTAX:
	return( (*SCM_SYNTAX_FUNC(v))(icode, argl, env) );
  }

  /* Nothing special. Sorry. Compiling default code */
  if (warn_unbound_func && (v == NULL || v == scm_unbound)) {
	scm_puts("Warning: function "); scm_cdisplay(sym);
	scm_puts(" was not bound to "); scm_cdisplay(v);
	scm_puts(" at compile-time\n");
  }
  scm_asm_gen_call(icode, TRUE, sym, argl, env);
  return(icode);
}

static SOBJ scm_compile_funcall(SOBJ icode, SOBJ form, SOBJ env)
{
  SOBJ func, argl;

  func = SCM_CAR(form);
  argl = SCM_CDR(form);

  return(scm_compile_funcall2(icode, func, argl, env));
}

static SOBJ scm_compile_pair(SOBJ icode, SOBJ form, SOBJ env)
{
  SOBJ f;

  f = SCM_CAR(form);

  switch(SCM_OBJTYPE(f)) {
  case SOBJ_T_PAIR:
	scm_asm_gen_mark(icode);
	scm_asm_gen_push_args(icode, SCM_CDR(form), env);
	scm_compile_pair(icode, f, env);
	scm_asm_gen1(icode, op_call);
	break;

	SCM_ERR("scm_compile_pair: don't know how to compile pair function", f);

  case SOBJ_T_INUM:
  case SOBJ_T_BNUM:
  case SOBJ_T_FNUM:
	SCM_ERR("scm_compile_pair: illegal expr (number ..)", form);

  case SOBJ_T_SYMBOL:
  case SOBJ_T_ATOM:
	return(scm_compile_funcall(icode, form, env));

  case SOBJ_T_MACRO:
	{
	  SOBJ l = execute_macro(f, form, env);
	  /* scm_puts("; execute_macro returns: "); scm_cprint(l); */
	  return(scm_compile_obj(icode, l, env));
	}

  default:
	if (f != NULL) {
	  scm_puts("func type is ");
	  scm_puts(scm_type_hook[SCM_OBJTYPE(f)].name);
	  scm_puts("\n");
	}
	SCM_ERR("scm_compile_pair: unsupported func", f);
  }
  return(icode);
}

static SOBJ scm_compile_obj(SOBJ icode, SOBJ obj, SOBJ env)
{
  SOBJ sym;
  int depth;

  switch(SCM_OBJTYPE(obj)) {
  case SOBJ_T_PAIR:				/* ( PROC | SYNTAX ARG... ) */
	scm_compile_pair(icode, obj, env);
	break;

  case SOBJ_T_SYMBOL:

	/* Coming here either for generated symbol or for modules symbols.
	   Generated symbols should be looked up in the env: trying to
	   find sym in env */

#ifdef DEBUG
	scm_puts("; compile_obj: got symbol ");  scm_cprint(obj);
	scm_puts(";   could be a generated symbol or a module symbol\n");
#endif
	sym = lookup_atom(SCM_SYM_NAME(obj), env, &depth, FALSE);
	if (sym == NULL) {
	  /* not found in current env: should be a module symbol */
#ifdef DEBUG
	  scm_puts(";   seems to be a module symbol\n");
#endif
	  sym = obj; depth = 0;
#ifdef DEBUG
	} else {
	  scm_puts(";   seems to be a generated symbol\n");
#endif
	}
	goto sym_found;

  case SOBJ_T_ATOM:
	sym = lookup_atom(obj, env, &depth, FALSE);
	if (sym == NULL) {			/* oops. symbol not found */
	  /* Assuming forward decl, create an unbound symbol. */
	  scm_hash_set(scm_symbol_hash, obj, scm_unbound);
	  sym = scm_hash_search(scm_symbol_hash, obj);
	}
	/* Here we have either a new symbol or an existing symbol. */
	
  sym_found:
	switch(SCM_OBJTYPE(sym)) {
	case SOBJ_T_SYMBOL:
	  if (SCM_VARP(SCM_SYM_VALUE(sym))) {
		scm_asm_gen2(icode, op_pushq, SCM_SYM_VALUE(sym));
		scm_asm_gen1(icode, op_getvar);
	  } else if (SCM_MACROP(SCM_SYM_VALUE(sym)) &&
				 SCM_MACRO_FUNC(SCM_SYM_VALUE(sym)) != NULL) {
		scm_asm_gen2(icode, op_pushq, SCM_MACRO_FUNC(SCM_SYM_VALUE(sym)));
	  } else {
		if (warn_unbound_symbol) {
		  scm_puts("Warning: symbol "); scm_cdisplay(sym);
		  scm_puts(" is unbound during compilation\n");
		}
		scm_asm_gen2(icode, op_pushv, sym);
	  }
	  break;
		
	case SOBJ_T_LSYMBOL:
	  scm_asm_gen3(icode, op_pushl,
				   SCM_MKINUM(SCM_LSYM_OFS(sym)), SCM_MKINUM(depth));
	  break;

	default:
	  SCM_ERR("scm_compile_obj: strange atom binding", sym);
	}

	break;
  default:
	scm_asm_gen2(icode, op_pushq, obj);  break;
	
	SCM_ERR("scm_compile_expr: unexpected symbol", obj);
	SCM_ERR("scm_compile_obj: don't know how to compile", obj);
  }
  return(icode);
}

static SOBJ scm_compile_expr(SOBJ obj, SOBJ env)
{
  SOBJ code;
  code = scm_compile_obj(scm_mkarray(0, NULL), obj, env);
  scm_asm_gen1(code, op_return);
  return(scm_optimize1(code));
}

/*************************************************************************
 * optimizer
 *************************************************************************/
static int opt_search_label(SOBJ icode, SOBJ lab)
{
  int i;
  SOBJ l;
  for (i = 0; i < SCM_ASIZE(icode); i++) {
	l = SCM_AREF(icode, i);
	if (SCM_CAR(l) == op_label && SCM_CADR(l) == lab)
	  return(i);
  }
  return(-1);
}

/*** KKK: should use reference counting instead of this slow full scan */
static int opt_used_label(SOBJ icode, SOBJ l)
{
  int i;
  SOBJ opc;
  for (i = 0; i < SCM_ASIZE(icode); i++) {
	opc = SCM_CAR(SCM_AREF(icode, i));
	
	if ( (opc == op_bra || opc == op_brf || opc == op_brt || 
		  opc == op_br_and || opc == op_br_or ||
		  opc == op_br_cond || opc == op_br_while) &&
		 SCM_CADR(SCM_AREF(icode, i)) == l)
	  return(TRUE);
  }
  return(FALSE);
}

static int opt_branch(SOBJ icode, int i)
{
  SOBJ expr = SCM_AREF(icode, i);
  int j, li;

  li = opt_search_label(icode, SCM_CADR(expr));
  if (li < 0) { 
	scm_puts("optimize: oops: label "); scm_cdisplay(SCM_CADR(expr));
	scm_puts(" not known. Current code is:\n");
	scm_cprint(icode);
	return(FALSE);
  }
#ifdef DEBUG_ASM
  scm_puts(";  branch to "); scm_cprint(SCM_CADR(expr));
#endif
  
  /* Found bra and matching label. Try to see if next opc is a return */
  for (j = li; j < SCM_ASIZE(icode); j++) {
	SOBJ o = SCM_CAR(SCM_AREF(icode, j));
	if (o != op_label && o != op_nop) {
	  if (o == op_return) { /* yeah */
#ifdef DEBUG_ASM
		scm_puts(";  points to a return - replacing\n");
#endif
		SCM_AREF(icode, i) =  scm_cons(op_return, NULL);

		if (!opt_used_label(icode, SCM_CADR(SCM_AREF(icode, li)))) 
		  SCM_CAR(SCM_AREF(icode, li)) = op_nop;

		return(TRUE);
	  } else if (o == op_endlet_return) {
#ifdef DEBUG_ASM
		scm_puts(";  points to an endlet_return - replacing\n");
#endif
		SCM_AREF(icode, i) = scm_cons(op_endlet_return, NULL);

		if (!opt_used_label(icode, SCM_CADR(SCM_AREF(icode, li)))) 
		  SCM_CAR(SCM_AREF(icode, li)) = op_nop;

		return(TRUE);
	  } else {
#ifdef DEBUG_ASM
		scm_puts(";  does not point to return\n");
#endif
	  }
	  break;
	}
  }
  return(FALSE);
}

static SOBJ next_instr(SOBJ icode, int i, int *label_between)
{
  SOBJ *p = &SCM_AREF(icode, ++i);
  SOBJ *l = &SCM_AREF(icode, SCM_ASIZE(icode));

  *label_between = FALSE;
  while(p < l) {
	if (SCM_CAR(*p) == op_label) {
	  *label_between = TRUE;
	} else {
	  if(SCM_CAR(*p) != op_nop)  return(*p);
	}
	p++;
  }
  return(NULL);
}

static int opt_endlet_return(SOBJ icode, int i)
{
  int label_between;
  SOBJ next;

  next = next_instr(icode, i, &label_between);
  
  if (next == NULL || SCM_CAR(next) != op_return)	return(FALSE);

#ifdef DEBUG_ASM
  scm_puts("; scm_optimize1: endlet ... return -> endlet-return\n");
#endif
  SCM_CAR(SCM_AREF(icode, i)) = op_endlet_return;

  if (!label_between) SCM_CAR(next) = op_nop;
  return(TRUE);
}

static int opt_call_endlet_return(SOBJ icode, int i)
{
  int label_between;
  SOBJ next;

  next = next_instr(icode, i, &label_between);
  if (next == NULL || SCM_CAR(next) != op_endlet_return) return(FALSE);
#ifdef DEBUG_ASM
  scm_puts("; scm_optimize1: call ... endlet_return -> endlet-jump\n");
#endif

  SCM_CAR(SCM_AREF(icode, i)) = op_endlet_jump;

  /* if they are no label between the call and the endlet_return, we
     can safely remove the endlet_return bcz it can't be reached */

  if (!label_between) SCM_CAR(next) = op_nop;
  return(TRUE);
}

static int opt_call_return(SOBJ icode, int i)
{
  int label_between;
  SOBJ next;

  next = next_instr(icode, i, &label_between);
  
  if (next == NULL || SCM_CAR(next) != op_return)	return(FALSE);

#ifdef DEBUG_ASM
  scm_puts("; scm_optimize1: call ... return -> jump\n");
#endif
  SCM_CAR(SCM_AREF(icode, i)) = op_jump;

  if (!label_between) SCM_CAR(next) = op_nop;
  return(TRUE);
}

static int opt_save_load(SOBJ icode, int i)
{
  SOBJ next;
  int label_between;
  next = next_instr(icode, i, &label_between);
  if (next == NULL || SCM_CAR(next) != op_load_r0) return(FALSE);
  if (label_between) return(FALSE);
  SCM_CAR(SCM_AREF(icode, i)) = op_nop;
  SCM_CAR(next) = op_nop;
  return(TRUE);
}

static SOBJ scm_optimize1(SOBJ icode)
{
  int i, changed;

  /* optimization game */

  /* first optimization:
   * ... (bra 10) ... (label 10) (return)
   * =>
   * ... (return) ... (label 10) (return)
   */
#ifdef DEBUG_ASM
  scm_puts("; optimize phase I\n");
  scm_puts("; before optimization:\n"); scm_cprint(icode);
#endif
  do {
	changed = FALSE;
	for (i = 0; i < SCM_ASIZE(icode); i++) {
	  if (SCM_CAR(SCM_AREF(icode,i)) == op_bra) {		/* got a branch */
#ifdef DEBUG_ASM
		scm_puts("; scm_optimize1: branch ... return -> return\n");
#endif
		if ((changed = opt_branch(icode, i)))	break;
	  }
	  if (SCM_CAR(SCM_AREF(icode,i)) == op_endlet) {
		if ((changed = opt_endlet_return(icode, i)))	break;
	  }
	  if (SCM_CAR(SCM_AREF(icode,i)) == op_call) {
		if ((changed = opt_call_endlet_return(icode, i)))	break;
		if ((changed = opt_call_return(icode, i)))			break;
	  }
	  if (SCM_CAR(SCM_AREF(icode,i)) == op_save_r0) {
		if ((changed = opt_save_load(icode, i)))	break;
	  }		
	}
  } while(changed);
  return(icode);
}

/************************************************************************
 * execute the icode
 ************************************************************************/
SOBJ scm_compile(SOBJ form, SOBJ env)
{
  return(mkcode(scm_assemble(scm_compile_expr(form, env))));
}

SOBJ scm_compile2(SOBJ form, SOBJ env)
{
  return(scm_compile(form, env));
}

void scm_init_asm()
{
  scm_asm_make_table();
  
  scm_gc_protect(&scm_asm_unknown_labels);
  scm_gc_protect(&scm_asm_known_labels);
  
  /*-- syntax expressions */

  /* obsolete features */
  /* scm_sym_code	   = scm_add_syntax("code",     scm_compile_code); */
  /* scm_sym_immediate = scm_add_syntax("immediate",scm_compile_immediate); */

  scm_sym_set 		= scm_add_syntax("set!", 		scm_compile_set);
  scm_sym_quote		= scm_add_syntax("quote", 		scm_compile_quote);
  scm_sym_qquote	= scm_add_syntax("quasiquote",	scm_compile_qquote);
  scm_sym_if		= scm_add_syntax("if", 			scm_compile_if);
  scm_sym_begin		= scm_add_syntax("begin", 		scm_compile_begin);
  scm_sym_lambda	= scm_add_syntax("lambda", 		scm_compile_lambda);
  scm_sym_define	= scm_add_syntax("define", 		scm_compile_define);
  scm_sym_let		= scm_add_syntax("let", 		scm_compile_let);
  scm_sym_letstar	= scm_add_syntax("let*", 		scm_compile_letstar);
  scm_sym_letrec	= scm_add_syntax("letrec", 		scm_compile_letrec);
  scm_sym_env		= scm_add_syntax("the-env", 	scm_compile_the_env);
  scm_sym_and		= scm_add_syntax("and",			scm_compile_and);
  scm_sym_or		= scm_add_syntax("or",			scm_compile_or);
  scm_sym_cond		= scm_add_syntax("cond",		scm_compile_cond);
  scm_sym_catch		= scm_add_syntax("catch",		scm_compile_catch);
  scm_sym_while		= scm_add_syntax("while",		scm_compile_while);
  scm_sym_until		= scm_add_syntax("until",		scm_compile_until);
  scm_sym_do 		= scm_add_syntax("do", 			scm_compile_do);

  scm_sym_else		= scm_symadd("else", scm_unbound);

  scm_add_syntax("asm", 		scm_syntax_asm);

  scm_add_cprim("execute-macro", 	execute_macro, 		3);
  scm_add_cprim("macroexpand",		macroexpand, 		2);
  scm_add_cprim("assemble",			scm_assemble, 		1);
  scm_add_cprim("ncomp",			scm_compile_expr, 	2);
  scm_add_cprim("nopt",				scm_optimize1,		1);
  scm_add_cprim("make-proc",		mkproc,				2);
  scm_add_cprim("make-code",		mkcode,				1);
  scm_add_cprim("ncompile",			scm_compile2,		2);

  scm_atom_local = scm_mkatom("local");
  scm_atom_rest  = scm_mkatom("rest");
  scm_atom_optionnal = scm_mkatom("optionnal");

  /* ??? don't need this because it should be protect by atom-list */
/*    scm_gc_protect(&scm_atom_local); */
/*    scm_gc_protect(&scm_atom_rest); */
/*    scm_gc_protect(&scm_atom_optionnal); */
}

