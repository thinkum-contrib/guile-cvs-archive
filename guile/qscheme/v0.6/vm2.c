/* -*- tab-width:4; -*- */
/*
 * Try for a virtual machine using special GCC features
 *
 *
 * Inspired by gforth
 *
 * $Id$
 *
 */
#define __VM2_INTERNAL__
#include "s.h"
#include "heap.h"
#include "vm2.h"

/*
#define ENGINE_TRACE
*/

/* #define DEBUG_VM_JUMP */
/* #define DEBUG_VM_CLOSURE */

#define USE_TOS
#define INUM_OPTIMIZATION

typedef void *Label;

int scm_op_max;					/* number of opcode */

void *scm_op_low_addr;			/* low and high address for opcodes */
void *scm_op_high_addr;

#ifndef SCM_WITH_THREADS
SCM_VMD scm_vmdata;				/* the only thread */
#endif

/*-- stack macros */
#ifdef STACK_CHECKS
#define check_overflow()	if (sp <= sp_base) goto l_overflow
#define check_sunderflow()	if (sp > sp0) goto l_underflow
#else
#define check_soverflow()
#define check_sunderflow()
#endif


static SOBJ *scm_print_op(SOBJ *code);

/********************************
 * VM and REGS support
 ********************************/

/* reset all vm registers */
void scm_vmd_regs_reset(SCM_VMD *vm)
{
  vm->reg.sp   = vm->stack_limit;
  vm->reg.ip   = NULL;
  vm->reg.cont = NULL;
  vm->reg.env  = NULL;
}

/* dump the contents of a SCM_VMD structure */
void scm_vmd_dump(SCM_VMD *vm)
{
  printf("VM at %p:\n", vm);
  printf("  code = %d\n", vm->code);
  printf("  regs: sp=%p ip=%p cont=%p env=%p\n",
		 vm->reg.sp, vm->reg.ip, vm->reg.cont, vm->reg.env);
  printf("  stack: base=%p limit=%p size=%d\n",
		 vm->stack_base, vm->stack_limit, vm->stack_size);
  printf("arg=%p ret=%p\n", vm->arg.addr, vm->ret.ptr);

}

/* alloc a stack for a vm */
void scm_vmd_stack_alloc(SCM_VMD *vm, int size)
{
  vm->stack_size  = size;
  vm->stack_base  = scm_must_alloc(sizeof(SOBJ)*size);
  vm->stack_limit = vm->stack_base + size;
  scm_vmd_regs_reset(vm);
}

/* free stack */
void scm_vmd_stack_free(SCM_VMD *vm)
{
  if (vm->stack_base)	scm_free(vm->stack_base);
  vm->stack_base = NULL;
  vm->stack_limit = NULL;
  vm->stack_size = 0;
  scm_vmd_regs_reset(vm);		/* clear registers to disable access */
}

SCM_VMD *scm_vmd_new()
{
  SCM_VMD *vmd = scm_must_alloc(sizeof(SCM_VMD));
  return(vmd);
}

/********************************
 * Stack
 ********************************/

void sdump(SCM_VMD *vm)
{
  SOBJ *p;

  scm_puts("stack: ");
#ifdef TOP_TO_BOTTOM
  p = vm->stack_limit;
  while(--p >= vm->reg.sp) {
	scm_cdisplay(*p); scm_putc(' ');
  }
#else
  p = vm->reg.sp;
  while(p < vm->stack_limit) {
	scm_cdisplay(*p++);  scm_putc(' ');
  }
#endif
  scm_putc('\n');
}


/****************************************************************
 * Debugging functions (to be use in the scm_vm function)
 ****************************************************************/

#ifdef ENGINE_TRACE

static void dump_stack(SOBJ *p, SOBJ *l)
{
  while(p < l) {
	printf("  %p: ", p);  scm_cprint(*p++);
  }
}

#define SDUMP()												\
{ 															\
	*sp=TOS; 												\
	printf("S stack: depth=%d\n", vm->stack_limit - sp); 	\
	dump_stack(sp, (SOBJ*)cont); 							\
}

#define EDUMP()										\
{													\
  printf("\ncode: ");  scm_print_op(ip);			\
  printf("ip=%p cont=%p env=%p\n", ip, cont, env);	\
}

#define ENV_DUMP()	scm_dump_env(env)
#define CONT_DUMP()	scm_dump_cont(cont)


#define DUMPSTATE() { SDUMP(); ENV_DUMP(); CONT_DUMP(); EDUMP(); }
#define NEXT 		{ DUMPSTATE(); goto *(*ip++);}
#else
#define NEXT 		{goto *(*ip++);}
#endif

/*-- number of args in a var arg primitive :
 * must be called before the sp moves :-)
 */
#define NARGS			((SOBJ *)cont - sp)

#ifdef WHY_THIS
/*-- return from a variable arg function */
#define VRETURN(value) 									\
{														\
  SOBJ r=value;											\
  sp=(void*)cont+(sizeof(SCM_ContFrame)-sizeof(SOBJ));	\
  cont=cont->next;										\
  TOS=r;												\
  NEXT;													\
}
#endif /* WHY_THIS */

/*-- return from a variable arg function */
#define VRETURN(value) 									\
{														\
  TOS=value;											\
  sp=(void*)cont+(sizeof(SCM_ContFrame)-sizeof(SOBJ));	\
  cont=cont->next;										\
  NEXT;													\
}

#define RETURN(value) { TOS=(value);  NEXT; }

#define Prim(name,str,nargs)	l_##name##: /* str */
#define PrimVarargs(name,str)	l_##name##: /* str */


/****************************************************************
 * Debug and print functions
 ****************************************************************/

void scm_dump_cont(SCM_ContFrame *c)
{
  printf("Continuations:\n");
  while(c) {
	printf("  cont %p: next=%p env=%p ip=%p\n", c, c->next, c->env, c->ip);
	c = c->next;
  }
}

void scm_dump_env(SOBJ e)
{
  int i, l;
  while(e) {
	l = SCM_INUM(SCM_ENV_FRAME(e)->nslots);
	printf("  env %p: next=%p nslots=%d\n",
		   e, SCM_ENV_NEXT(e), l);
	if (l > 20) l = 20;
	for (i = 0; i < l; i++) {
	  printf("  binding[%d] = ", i);  scm_cprint(SCM_ENV_FRAME(e)->binding[i]);
	}
	if (SCM_INUM(SCM_ENV_FRAME(e)->nslots) != l) {
	  printf("  ...\n");
	}	  
	e = SCM_ENV_NEXT(e);
  }
}

/****************************************************************
 * vm function type
 ****************************************************************/
void scm_vmfunc_mark(SOBJ obj)
{
}

void scm_vmfunc_sweep(SOBJ obj)
{
}

/****************************************************************
 * NEW ENGINE
 ****************************************************************/

/****************************************************************
 *
 * Virtual Machine Registers:
 * 	ENV: 	points to the current environment chain 
 *	CONT	points to the current continuation chain
 *	IP		points to the instruction to execute
 *	SP		points to the current stack position
 *	TOS		cache of the top of stack value...
 *	
 ****************************************************************/

/****************************************************************
 * Engine: helper functions and macros
 ****************************************************************/

static SOBJ scm_clone_env(SOBJ env)
{
  SOBJ new;
  SCM_EnvFrame *ef;
  int nbytes;
  
  new = scm_newcell(SOBJ_T_ENV);
  ef = SCM_ENV_FRAME(env);
  nbytes = offsetof(SCM_EnvFrame, binding[SCM_INUM(ef->nslots)]);
  
  /*  SCM_ENV_NEXT(new) = NULL; */
  SCM_ENV_FRAME(new) = scm_must_alloc(nbytes);
  memcpy(SCM_ENV_FRAME(new), ef, nbytes);
  return(new);
}

static SOBJ mk_persistent_env(SOBJ env, void *sbase, void *slimit)
{
  SOBJ e, new, last, chain;

  if ((void*)env >= sbase && (void *)env < slimit) { /* on stack */
#ifdef DEBUG_VM_CLOSURE
	printf("mk_persistent_env: env on stack\n");
#endif
	chain = NULL; last = NULL;
	for (e = env; e; e = SCM_ENV_NEXT(e)) {
	  new = scm_clone_env(e);
#ifdef DEBUG_VM_CLOSURE
	  printf("mk_persistent_env: cloned "); scm_cprint(new);
#endif
	  if (last) {
		SCM_ENV_NEXT(last) = new;
	  } else {
		chain = new;
	  }
	  last = new;
	}
  } else {
#ifdef DEBUG_VM_CLOSURE
	printf("mk_persistent_env: head not on stack\n");
#endif
	for (e = env; e; e = SCM_ENV_NEXT(e)) {
	  if (! ((void*)e >= sbase && (void *)e < slimit) ) {
#ifdef DEBUG_VM_CLOSURE
		printf("mk_persistent_env: EnvFrame still on stack");
		scm_cprint(e);
#endif
	  }
	}
	chain = env;
  }
  return(chain);
}


static void copy_closure_env(SOBJ closure, SOBJ env,
							 void *sbase, void *slimit)
{
  SOBJ clenv;
  SOBJ e, *p;

  /* printf("copy_closure_env: env chain before:\n");  scm_dump_env(env); */

  clenv = SCM_CLOSURE_ENV(closure);
  if (clenv != NULL &&			/* not null */
	  ((void*)clenv >= sbase && (void*)clenv < slimit)) { /* on stack */
	printf("copy_closure_env: env of closure @%p is on stack\n", closure);

	p = &SCM_CLOSURE_ENV(closure);

	for (e = env; e != NULL; e = SCM_ENV_NEXT(e)) {
	  *p = scm_clone_env(e);
	  p = &SCM_ENV_NEXT(*p);
	}
  }
  /* printf("copy_closure_env: env chain after:\n"); 
  scm_dump_env(SCM_CLOSURE_ENV(closure));
  */
}

/*
 * Copy the stack. Works also with overlapping areas.
 */
inline void scm_vm_move_stack(SOBJ *dst, SOBJ *src, int nitems)
{
  if (src == dst || nitems == 0)
	return;

  if (dst < src) {				/* copy down */
	while(nitems-- > 0) 
	  *dst++ = *src++;

  } else {						/* copy up */
	dst += nitems;
	src += nitems;
	while(nitems-- > 0)
	  *(--dst) = *(--src);
  }
}


#define PUSH_CONT() { \
  vm->reg.sp = sp; supdate(); \
  sp -= sizeof(SCM_ContFrame)/sizeof(SOBJ); \
  *sp = TOS = (SOBJ)cont;  \
  cont = (SCM_ContFrame*)sp; \
  cont->env = NULL; cont->ip = NULL; \
}

/*-- pop continuation, restore registers saved in cont */
#define POP_CONT() \
{ \
  sp = (void*)cont + sizeof(SCM_ContFrame) - sizeof(SOBJ); \
  ip = cont->ip;  env = cont->env; \
  cbinding = (env) ? SCM_ENV_FRAME(env)->binding : NULL; \
  cont = cont->next; \
}

/*-- push an environement object on the stack.
 *
 * The stack transformation is :
 *
 *	arg#n-1 ... arg0 -- arg#n-1 ... arg0 nslots [env_object]
 *
 * - the args and nslots are packed in an SCM_EnvFrame structure
 * - the new env_object use this for it's env link.
 * - the env register points to the env_object
 * - the cbinding variable points to first argument (arg0)
 *
 * Note: all this circus is to avoid allocating cell each time a
 * temporary environment is created. Temporary environments (stored on
 * the stack) are copied to heap when a closure object is returned.
 *
 */
#define PUSH_ENV_FRAME(nextenv, nslots)								\
{									 								\
  SCM_EnvFrame *ef;													\
  cbinding = sp;													\
  *(--sp) = SCM_MKINUM(nslots);										\
  ef = (SCM_EnvFrame*)sp;											\
  /* alloc space on stack for an env object and fill it's frame and \
     next fields */													\
  ((void*)sp) -= sizeof(Sobject);									\
  SCM_ENV_FRAME((SOBJ)sp) = ef;										\
  SCM_ENV_NEXT((SOBJ)sp) = nextenv;									\
  env = (SOBJ)sp;													\
}

#define COMPLETE_ENV(proc) \
{ \
  SOBJ env_obj = SCM_PROC_ENV(proc); 								\
  SCM_EnvFrame *sf = SCM_ENV_FRAME(env_obj); 						\
  sp[0] = TOS;														\
  PUSH_ENV_FRAME(SCM_ENV_NEXT(env_obj), SCM_INUM(sf->nslots));		\
}

/* save/restore vm registers to/from a SCM_vmRegisters structure */

#define SCM_SAVE_VMREGS(x) \
{ *sp = TOS; (x)->sp = sp; (x)->ip = ip; (x)->cont = cont; (x)->env = env; }

#define SCM_RESTORE_VMREGS(x) \
{ sp = (x)->sp;  ip = (x)->ip;  cont = (x)->cont;  env = (x)->env; \
  TOS = *sp; }

#define SCM_RESTORE_CBINDING() \
{  cbinding = (env) ? SCM_ENV_FRAME(env)->binding : NULL; }


/*** the vm itself */
void scm_vm(SCM_VMD *vm)
{
  register SOBJ *ip IPREG;
  register SOBJ *sp SPREG;
  register SOBJ TOS TOSREG;
  register SCM_ContFrame *cont;
  register SOBJ env;

  SOBJ r0;						/* gp register */
  SOBJ *cbinding;				/* current bindings */

  SOBJ proc;					/* current proc during callv and jumpv */
  SOBJ *src, *dst;				/* global src and dst for stack copy */

  SOBJ *sp_base;

  static SCM_PRIM_TABLE symbol[] = {
	/* address			name					narg,follow,term*/
	{&&l_nop,			"%nop",					0,	0,	0 },
	{&&l_end,			"%end",					0,	0,	1 },

	{&&l_dolet,			"%dolet",				0,	0,	0 },
	{&&l_doletstar,		"%dolet*",				0,	1,	0 },
	{&&l_endlet_jump,	"%endlet-jump",			0,	0,	0 },

	{&&l_drop,			"%drop",				0,	0,	0 },
	{&&l_push,			"%push",				0,	1,	0 },
	{&&l_pushv,			"%pushv",				0,	1,	0 },
	{&&l_pushn,			"%pushn",				0,	1,	0 },
	{&&l_pushl,			"%pushl",				0,	1,	0 },
	{&&l_pushl0,		"%pushl0",				0,	1,	0 },
	{&&l_pushl1,		"%pushl1",				0,	1,	0 },
	{&&l_pushl2,		"%pushl2",				0,	1,	0 },
	{&&l_pushl3,		"%pushl3",				0,	1,	0 },
	{&&l_alloc,			"%alloc",				0,	1,	0 },
	{&&l_pushlist,		"%pushlist",			0,	1,	0 },

	{&&l_mark,			"%mark",				0,	0,  0 },
	{&&l_mkclosure,		"%mkclosure",			0,	0,	0 },
	{&&l_endlet,		"%endlet",				0,	0,	0 },
	{&&l_return,		"%return",				0,	0,	0 },
	{&&l_callc,			"%callc",				0,	0,	0 },
	{&&l_callc0,		"%callc0",				0,	0,	0 },
	{&&l_callc1,		"%callc1",				0,	0,	0 },
	{&&l_callc2, 		"%callc2",				0,	0,	0 },
	{&&l_callc3,		"%callc3",				0,	0,	0 },
	{&&l_callc4,		"%callc4",				0,	0,	0 },
	{&&l_callc5,		"%callc5",				0,	0,	0 },
	{&&l_callc6,		"%callc6",				0,	0,	0 },
	{&&l_callc7,		"%callc7",				0,	0,	0 },
	{&&l_callc8,		"%callc8",				0,	0,	0 },
	{&&l_callc9,		"%callc9",				0,	0,	0 },
	{&&l_callc10,		"%callc10",				0,	0,	0 },
	{&&l_calls,			"%calls",				0,	0,	0 },

	{&&l_no_call,		"%no-call",				0,	0,	0,},
	{&&l_jump,			"%jump",				0,	0,	0,},
	{&&l_call,			"%call",				0,	0,	0,},
	{&&l_call_prim,		"%call-prim",			0,  0,	0,},
	{&&l_call_cprim,	"%call-cprim",			0,  0,	0,},
	{&&l_call_code,		"%call-code",			0,  0,	0,},
	{&&l_call_proc,		"%call-proc",			0,  0,	0,},
	{&&l_call_closure,	"%call-closure",		0,  0,	0,},
	{&&l_call_macro,	"%call-macro",			0,  0,	0,},
	{&&l_call_extfunc,	"%call-extfunc",		0,  0,	0,},
	{&&l_call_vmfunc,	"%call-vmfunc",			0,  0,	0,},
	
	{&&l_catch,			"%catch",				2,	1,	0,},
	{&&l_uncatch,		"%uncatch",				0,	0,	0,},

	{&&l_store,			"%store-global",		0,	0,	0 },
	{&&l_setl,			"%setl",				0,	1,	0 },
	{&&l_setl0,			"%setl0",				0,	1,	0 },
	{&&l_setl0drop,		"%setl0drop",			0,  1,	0 },
	{&&l_getvar,		"%getvar",				1,	0,	0 },
	{&&l_setvar,		"%setvar",				2,	0,	0 },
	{&&l_br_and,		"%br_and",				0,	1,	0 },
	{&&l_br_or,			"%br_or",				0,	1,	0 },
	{&&l_br_cond,		"%br_cond",				0,	1,	0 },
	{&&l_br_while,		"%br_while",			0,	1,	0 },
	{&&l_bra,			"%bra",					0,	1,	0 },
	{&&l_brf,			"%brf",					0,	1,	0 },
	{&&l_brt,			"%brt",					0,	1,	0 },

	{&&l_save_r0,		"%save_r0",				0,	0,	0 },
	{&&l_load_r0,		"%load_r0",				0,	0,	0 },
	
#include "prim2.x"
#include "number.x"
	{ NULL }
  };

  if (vm->code != SCM_VM_DO_EXECUTE) {
	if (vm->code == SCM_VM_DO_INIT) {
	  /********************************
	   * VM initialization
	   ********************************/

	  int i, opc;

	  /* if already initialized returns symbol */
	  if ((void*)scm_type_hook[0].execute == &&l_nop) {
		printf("scm_vm: already initialized\n");
		return;
	  }

	  /* scm_type_hook and fix the execute field */
	  for (i = 0; i < scm_type_next_descr; i++) {
		opc = scm_type_hook[i].execute;
		if (opc >= 0 && opc < SCM_OP_MAX) {
		  scm_type_hook[i].execute = (long)symbol[opc].address;
		}
	  }

#ifdef CHECK_ODDITY
	  for (i = 0; symbol[i].name; i++) {
		if ( ((long) symbol[i].address & 1) != 0) {
		  printf("scm_vm: oops: opcode %s at %p is not odd\n",
				 symbol[i].name, symbol[i].address);
		}
	  }
#endif

	  /* register primitive functions and set scm_op_max,
	 * scm_op_low_addr and scm_op_high_addr */
	  scm_op_low_addr = (void*)-1;
	  scm_op_high_addr = (void*)0;
	  for (i = 0; symbol[i].name; i++) {

		if (symbol[i].name[0] != '%')	/* don't register vm internals */
		  scm_add_prim(symbol[i].name, symbol + i);

		if (scm_op_low_addr > symbol[i].address)
		  scm_op_low_addr = symbol[i].address;

		if (scm_op_high_addr < symbol[i].address)
		  scm_op_high_addr= symbol[i].address;
	  }
	  scm_op_max = i;
	  return;
	  
	} else if (vm->code == SCM_VM_DO_GET_OPCODE) {
	  if (vm->arg.opcode < 0 || vm->arg.opcode >= scm_op_max) {
		vm->ret.entry = NULL;
	  } else {
		vm->ret.entry = symbol + vm->arg.opcode;
	  }
	  return;
	  
	} else if (vm->code == SCM_VM_DO_GET_OPCODE_BY_NAME) {
	  int i;
	  for (i = 0; symbol[i].name; i++) {
		if (streq(vm->arg.name, symbol[i].name)) {
		  vm->ret.entry = symbol + i;
		  return;
		}
	  }
	  vm->ret.entry = NULL;
	  return;

	} else if (vm->code == SCM_VM_DO_GET_OPCODE_BY_ADDR) {
	  int i;
	  for (i = 0; symbol[i].name; i++) {
		if (symbol[i].address == vm->arg.addr) {
		  vm->ret.entry = symbol + i;
		  return;
		}
	  }
	  vm->ret.entry = NULL;
	  return;
	}
	
	SCM_ERR("bad vm code:", SCM_MKINUM(vm->code));
  }

  /********************************
   * VM executor
   ********************************/
  
  /* load registers with vm registers */
  ip   = vm->reg.ip;
  sp   = vm->reg.sp;
  cont = vm->reg.cont;
  env  = vm->reg.env;
  proc = NULL;
  dst = NULL;
  sp_base = vm->stack_base + STACK_OVERFLOW_GRACE;

  cbinding = (env) ? SCM_ENV_FRAME(env)->binding : NULL;

#ifdef ENGINE_TRACE
  printf("*** new engine started ***\n");
#endif
  
#ifdef USE_TOS
  TOS = sp[0];
#endif
  NEXT;

l_nop: {
	NEXT; 
}

l_end: {
  spop(vm->ret.obj);
  supdate();
  vm->reg.sp = sp;
#ifdef ENGINE_TRACE
  printf("*** engine finished bcz l_end: ***\n");
#endif
  return;
}
  
#ifdef STACK_CHECKS
l_overflow:
{
  SCM_ERR("Stack overflow", NULL);
}

#ifdef NOT_USED
 l_underflow:
{
  SCM_ERR("Stack underflow", NULL);
}
#endif
#endif
 
l_doletstar:
{
  int n = SCM_INUM(*ip++);
  /*-- mark and push n #unbound object */
  PUSH_CONT();
  while(--n >= 0) spush(scm_unbound);
} 	/* FALL THROUGH */

l_dolet: 
{
  /* stack: cont binding_n ... binding_0 */
  int nslots = (SOBJ*)cont - sp;

  cont->env = env;				/* save env */
  *sp = TOS;
  PUSH_ENV_FRAME(env, nslots);
  NEXT;
}


/* simple stack manipulations */

l_drop: {	sdrop();		 NEXT; }
l_push: {	spush( *ip++ );  NEXT; }
l_pushv:
{
  spush( SCM_SYM_VALUE((SOBJ)(*ip++)));
  if (TOS == scm_unbound)
	SCM_ERR("symbol unbound", SCM_SYM_NAME((SOBJ)ip[-1]));
  NEXT;
}

l_pushn:
{
  int n = SCM_INUM(*ip++);
  while(--n >= 0) 
	spush(*ip++);
  NEXT;
}

l_pushl: {		/* lev, ofs */
  SOBJ p = env;
  short n = SCM_INUM(*ip) >> 16;
  while(--n >= 0) 	p = SCM_ENV_NEXT(p);
  spush(SCM_ENV_FRAME(p)->binding[(short)SCM_INUM(*ip++)]);
  NEXT;
}

l_pushl0: {
  spush(cbinding[SCM_INUM(*ip++)]);
  NEXT;
}

l_pushl1: {
  spush(SCM_ENV_FRAME(SCM_ENV_NEXT(env))
		->binding[SCM_INUM(*ip++)]);
  NEXT;
}

l_pushl2: {
  spush(SCM_ENV_FRAME(SCM_ENV_NEXT(SCM_ENV_NEXT(env)))
		->binding[SCM_INUM(*ip++)]);
  NEXT;
}

l_pushl3: {
  spush(SCM_ENV_FRAME(SCM_ENV_NEXT(SCM_ENV_NEXT(SCM_ENV_NEXT(env))))
		->binding[SCM_INUM(*ip++)]);
  NEXT;
}

l_alloc: {					 	/* S: -- [ nil ]n  */
  int n = SCM_INUM(*ip++);
  while(--n >= 0) 
	spush(NULL);
  NEXT;
}

l_pushlist:				/* mark -- mark item#n-1 ... item#0 */
{
  SOBJ p, list = *ip++;
  int i;

  *sp = TOS;
  for (i = 0, p = list; p; p = SCM_CDR(p), i++)
	;
  sp -= i;
  for (i = 0, p = list; p; sp[i++] = SCM_CAR(p), p = SCM_CDR(p))
	;
  TOS = *sp;
  NEXT;
}

#ifdef NOT_USED
l_nonimp: {
  SCM_ERR("not implemented opcode", NULL);
}
#endif

/*-- create a contframe struct on the stack:
 * we push because of possible use of TOS cache:
 * after PUSH_CONT, cont will point to a contframe struct...
 */
l_mark: {						/* push a continuation frame on the stack */
  PUSH_CONT();
  NEXT;
}

l_mkclosure: {					/* proc -- closure */
  SOBJ closure = scm_newcell(SOBJ_T_CLOSURE);
  SCM_CLOSURE_CODE(closure) = TOS;
  SCM_CLOSURE_ENV(closure) = env = mk_persistent_env(env,sp,vm->stack_limit);
  SCM_RESTORE_CBINDING();
  TOS = closure;
  NEXT;
  
#ifdef OLD_CLOSURE
  SOBJ closure = scm_newcell(SOBJ_T_CLOSURE);
  SCM_CLOSURE_CODE(closure) = TOS;
  SCM_CLOSURE_ENV(closure) = env;
#ifdef DEBUG_VM_CLOSURE
  printf("mkclosure: %p (code=%p env=%p)\n", closure, TOS, env);
#endif
  copy_closure_env(closure, env, sp, vm->stack_limit);
  env = SCM_CLOSURE_ENV(closure);
  SCM_RESTORE_CBINDING();
  TOS = closure;
  NEXT;
#endif
}

l_callc: {
  SCM_ERR("%callc: not implemented", NULL);
}

l_callc0: {		/* proc -- n */
  sresync();
  TOS = (*SCM_CPRIM_FUNC((SOBJ)TOS)) ();
  check_overflow();
  NEXT;
}
l_callc1: {		/* a1 proc -- n */
  SOBJ r; 
  sresync();
  r = (*SCM_CPRIM_FUNC((SOBJ)TOS))(sp[1]);
  sp += 1;  TOS=r;
  check_overflow();
  NEXT;
}
l_callc2: {		/* a2 a1 proc -- n */
  SOBJ r;
  sresync();
  r = (*SCM_CPRIM_FUNC((SOBJ)TOS))(sp[1], sp[2]);
  sp += 2; TOS=r;
  check_overflow();
  NEXT;
}
l_callc3: {
  SOBJ r;
  sresync();
  r = (*SCM_CPRIM_FUNC((SOBJ)TOS))(sp[1], sp[2], sp[3]);
  sp += 3; TOS = r;
  check_overflow();
  NEXT;
}
l_callc4: {
  SOBJ r;
  sresync();
  r = (*SCM_CPRIM_FUNC((SOBJ)TOS))(sp[1], sp[2], sp[3], sp[4]);
  sp += 4; TOS = r;
  check_overflow();
  NEXT;
}
l_callc5: {
  SOBJ r;
  sresync();
  r = (*SCM_CPRIM_FUNC((SOBJ)TOS))(sp[1], sp[2], sp[3], sp[4], sp[5]);
  sp += 5; TOS = r;
  check_overflow();
  NEXT;
}
l_callc6: {
  SOBJ r;
  sresync();
  r = (*SCM_CPRIM_FUNC((SOBJ)TOS))(sp[1],sp[2],sp[3],sp[4],sp[5],sp[6]);
  sp += 6; TOS = r;
  check_overflow();
  NEXT;
}
l_callc7: {
  SOBJ r;
  sresync();
  r = (*SCM_CPRIM_FUNC((SOBJ)TOS))(sp[1],sp[2],sp[3],sp[4],sp[5],sp[6],sp[7]);
  sp += 7; TOS = r;
  check_overflow();
  NEXT;
}
l_callc8: {
  SOBJ r;
  sresync();
  r = (*SCM_CPRIM_FUNC((SOBJ)TOS))(sp[1],sp[2],sp[3],sp[4],sp[5],sp[6],sp[7],sp[8]);
  sp += 8; TOS = r;
  check_overflow();
  NEXT;
}
l_callc9: {
  SOBJ r;
  sresync();
  r = (*SCM_CPRIM_FUNC((SOBJ)TOS))(sp[1],sp[2],sp[3],sp[4],sp[5],sp[6],sp[7],sp[8],sp[9]);
  sp += 9; TOS = r;
  check_overflow();
  NEXT;
}
l_callc10: {
  SOBJ r;
  sresync();
  r = (*SCM_CPRIM_FUNC((SOBJ)TOS))(sp[1],sp[2],sp[3],sp[4],sp[5],sp[6],sp[7],sp[8],sp[9],sp[10]);
  sp += 10; TOS = r;
  check_overflow();
  NEXT;
}

l_calls: {						/* cont an..a0 proc -- r */
  SOBJ ret, func;

  spop(func);
  cont->env = env;
  cont->ip  = ip;
  ret = (*SCM_CPRIM_FUNC(func))( (SOBJ*)cont - (SOBJ*)sp, sp);
  spush(ret);
  goto l_return;
}

l_no_call1: 
  proc = TOS;

l_no_call:
{
  /* dirty hack: ip points after the call now: so we should have
   *
   * ip-3:	pushv
   * ip-2:	variable
   * ip-1:	call
   * ip:	???
   */
  scm_puts("\nOOPS: ip="); scm_putx(ip);
  scm_puts(": ");  scm_cdisplay(ip[-2]); scm_puts(" is unbound.");
  SCM_ERR("cannot call", proc);
}

l_endlet_jump:					/* args proc */
{
  src = (SOBJ*)cont;
  cont = cont->next;
  goto l_jump1;
}

l_jump: {
#ifdef DEBUG_VM_JUMP
  scm_puts("jump: ");  scm_cprint(TOS);
#endif
  src = (SOBJ*)cont;
 
 l_jump1:
  if (TOS == NULL || SCM_INUMP(TOS)) goto l_no_call1;

  spop(proc);
  cont = cont->next;
  env = cont->env;
  ip = cont->ip;
  dst = (SOBJ*)cont;
  while(src > sp) { *(--dst) = *(--src); }
  
  /* OOPS: have a bug in egcc ?
   * sp += (dst - src) shoud be sp = dst, but I have not a correct
   * value in sp if I choose the 2nd option. Strange is not it ?
   */
  sp += (dst - src);			/* seems to have a bug in gcc */
  TOS = *sp;
  goto *(scm_type_hook[proc->type].execute); 
}


l_call: {
  if (TOS == NULL || SCM_INUMP(TOS)) goto l_no_call1;

  cont->ip = ip;
  cont->env = env;
  spop(proc);
  goto *(scm_type_hook[proc->type].execute);
}

l_call_prim: {
  SCM_PRIM_TABLE *p = SCM_PRIM(proc);
  if (p->nargs >= 0) {			/* pop continuation frame (not used) */
	cont = cont->next;
  }
  goto *(p->address);
}

l_call_cprim: {
  spush(proc);
  
  switch(SCM_CPRIM_NARGS(proc)) {
  case -1: 	goto l_calls;
  case 0:	cont=cont->next;  goto l_callc0;
  case 1:   cont=cont->next;  goto l_callc1;
  case 2:	cont=cont->next;  goto l_callc2;
  case 3: 	cont=cont->next;  goto l_callc3;
  case 4: 	cont=cont->next;  goto l_callc4;
  case 5: 	cont=cont->next;  goto l_callc5;
  case 6: 	cont=cont->next;  goto l_callc6;
  }
  goto l_no_call;
}

l_call_code: {
  ip = SCM_CODE_CODE(proc);
  NEXT;
}

l_call_proc: {
  ip  = SCM_PROC_CODE(proc)->code;
  if (SCM_PROC_CODE(proc)->optargs == 0) {
	COMPLETE_ENV(proc);
	NEXT;
  }
  SCM_ERR("callv: opt args not supported yet", proc);
  /* have optionnal */
  NEXT;
}

l_call_closure: {
  SCM_Code *code;
  
  code = SCM_PROC_CODE(SCM_CLOSURE_CODE(proc));
  ip  = code->code;
  if (code->optargs != 0) {	/* optionnal arguments */
	SOBJ list, *p, *q;
	if ((q = sp+code->nargs-1) > (SOBJ*)cont)
	  SCM_ERR("callv: bad number of args", proc);
	
	*sp = TOS;  p = (SOBJ*)cont; list = NULL;
	while(--p >= q) list = scm_cons(*p, list);
	if (q == (SOBJ*)cont) {			/* insert space on stack */
	  for (p = sp; p < q; p++) {  p[-1] = p[0]; }
	  q[-1] = list;	sp--;
	} else {
	  p = ((SOBJ*)cont)-1; while(--q >= sp) { *(--p) = *q; }
	  ((SOBJ*)cont)[-1] = list;
	  sp = p;
	}
	TOS=*sp;
  }
  /* alloc room for local arguments */
  sp -= code->nlocals;
  
  /* sp[0] = TOS; */
  PUSH_ENV_FRAME(SCM_CLOSURE_ENV(proc), code->nlocals + code->nargs);
  NEXT;
}


l_call_macro:
{
  SCM_ERR("l_call_macro: call to macro is forbiden", proc);

  if (SCM_MACRO_FUNC(proc)) {
	proc = SCM_MACRO_FUNC(proc);
	goto l_call_closure;
  }
}

l_call_extfunc:
{
  /* don't need supdate(), because last action was poping proc */
  sresync();
  TOS = scm_extfunc_call(proc, (SOBJ*)cont - sp, sp);
  POP_CONT();					/* pop current continuation */
  NEXT;
}

l_call_vmfunc:
{
  SCM_vmRegisters vm;
  void (*func)(SCM_vmRegisters *);

  func = SCM_VMFUNC(proc);
  SCM_SAVE_VMREGS(&vm);
  (*func)(&vm);
  SCM_RESTORE_VMREGS(&vm);
  SCM_RESTORE_CBINDING();
  NEXT;
}

/*-- low level error handling */

l_catch: {						/* handler tag -- */
  SOBJ catch;
  SCM_CatchContext *c;
  
  catch = scm_mkcatch();		/* create a catch context object */
  c = SCM_CATCH_CONTEXT(catch);	/* keep pointer to catch context struct */
  scm_catch_list = scm_cons(catch, scm_catch_list);

  /* fill internal catch context structure */
  spop(c->tag);					/* with the tag */
  spop(c->handler);				/* with the handler */
  SCM_SAVE_VMREGS(&c->vm);
  c->vm.ip	+= SCM_INUM(*ip++);	/* correct address in case of catch */
  if (setjmp(c->env) != 0) {	/* wheepee!!! got a catch */
	SCM_RESTORE_VMREGS(&c->vm);
	SCM_RESTORE_CBINDING();
	scm_puts("*** catch: scm_catch_list="); scm_cprint(scm_catch_list);
	if (c->handler) {
	  PUSH_CONT();
	  spush(scm_thrown_msg);
	  spush(scm_thrown_tag);
	  spush(c->handler);
	  goto l_call;				/* execute handler */
	}
	SCM_ERR("catch: katastroph: no handler", NULL);
  }
  NEXT;
}

l_uncatch: {					/* uncatch: -- */
  if (SCM_PAIRP(scm_catch_list)) {
	scm_catch_list = SCM_CDR(scm_catch_list);
  }
  NEXT;
}

l_endlet: 
{
  cont->ip = ip;
} /* FALL THROUGH */

l_return: {
  SOBJ r;	

  if (TOS && SCM_CLOSUREP(TOS)) {
	if (SCM_CLOSURE_ENV(TOS) >= (SOBJ)sp &&
		SCM_CLOSURE_ENV(TOS) <  (SOBJ)vm->stack_limit) {
	  printf("return: OOPS: closure env still on stack\n");
	}
#ifdef DEBUG_VM_CLOSURE
	printf("return: closure %p env=%p\n", TOS, env);
#endif
	copy_closure_env(TOS, env, sp, vm->stack_limit);
	env = SCM_CLOSURE_ENV(TOS);
	SCM_RESTORE_CBINDING();
  }

  spop(r);
  if (sp >= vm->stack_limit) {
	printf("*** Stack underflow: depth=%d ***\n",
		   vm->stack_limit - vm->reg.sp);
  }
#ifdef OLD 
  if (sp >= vm->reg.sp) { 
    printf("*** engine terminates bcz sp>=sp0 ***\n"); 
	vm->ret.obj = r;
	return;
  }
#endif  
  POP_CONT();  *sp = TOS = r; 
  NEXT;

}

l_store: {						/* value symbol -- value */
  SOBJ sym;
  spop(sym);
  SCM_SYM_VALUE(sym) = TOS;
  NEXT;
}

l_setl: {
  SOBJ p = env;
  short n = SCM_INUM(*ip) >> 16;
  while(--n >= 0) p = SCM_ENV_NEXT(p);
  SCM_ENV_FRAME(p)->binding[(short)SCM_INUM(*ip++)] = TOS;
  NEXT;
}

l_setl0: {
  cbinding[SCM_INUM(*ip++)] = TOS;
  NEXT;
}

l_setl0drop: {
  cbinding[SCM_INUM(*ip++)] = TOS;  sdrop();
  NEXT;
}

l_getvar: {						/* var -- value */
  TOS = (*(SCM_VAR_AUX(TOS)->get))(TOS,NULL);
  NEXT;
}

l_setvar: {						/* value var -- value */
  SOBJ var;
  spop(var);
  (*(SCM_VAR_AUX(var)->set))(var,NULL,TOS);
  NEXT;
}

l_br_and: {						/* #f -- #f +bra || x -- */
  if (TOS == scm_false) {
	ip += SCM_INUM(*ip);
	NEXT;
  }
  sdrop();
  ip++;
  NEXT;
}

l_br_or: {						/* #f -- || x -- x +bra */
  if (TOS != scm_false) {
	ip += SCM_INUM(*ip);
	NEXT;
  }
  sdrop();
  ip++;
  NEXT;
}

l_br_cond: {					/* #f -- +bra || x -- x */
  if (TOS != scm_false) {
	ip++;
	NEXT;
  }
  sdrop();
  ip += SCM_INUM(*ip);
  NEXT;
}

l_br_while: {					/* #f -- #f || x -- +bra */
  if (TOS == scm_false) {
	ip++;
	NEXT;
  }
  sdrop();
  ip += SCM_INUM(*ip);
  NEXT;
}

l_bra: {
  ip += SCM_INUM(*ip);
  NEXT;
}

l_brf: {
  SOBJ flag;
  spop(flag);
  if (flag == scm_false) {
	ip += SCM_INUM(*ip);
	NEXT;
  }
  ip++;
  NEXT;
}	

l_brt: {
  SOBJ flag;
  spop(flag);
  if (flag != scm_false) {
	ip += SCM_INUM(*ip);
	NEXT;
  }
  ip++;
  NEXT;
}

l_save_r0: {  spop(r0);  	NEXT; }
l_load_r0: {  spush(r0);  	NEXT; }


#include "prim2.i"
#include "number.i"

  return;
}

/*-- intialize */

void scm_engine_init()
{
  SCM_VMD vm;

  /* initialize the engine */
  vm.code = SCM_VM_DO_INIT;  scm_vm(&vm);
}


SOBJ scm_run_engine(SOBJ *ip)
{
  SCM_VMD *v = scm_vmd();
  v->code = SCM_VM_DO_EXECUTE;
  v->reg.ip = ip;
  scm_vm(v);
  return(v->ret.obj);
}


SCM_PRIM_TABLE *scm_get_addr(int opc)
{
  SCM_VMD vm;

  vm.code = SCM_VM_DO_GET_OPCODE;
  vm.arg.opcode = opc;
  scm_vm(&vm);
  return(vm.ret.entry);
}

int scm_is_opcode_address(void *p)
{
  return (p >= scm_op_low_addr && p <= scm_op_high_addr);
}


SCM_PRIM_TABLE *scm_search_opcode_address(char *name)
{
  SCM_VMD vm;

  vm.code = SCM_VM_DO_GET_OPCODE_BY_NAME;
  vm.arg.name = name;
  scm_vm(&vm);
  return(vm.ret.entry);
}

static SCM_PRIM_TABLE *scm_search_opcode_by_addr(void *p)
{
  SCM_VMD vm;

  vm.code = SCM_VM_DO_GET_OPCODE_BY_ADDR;
  vm.arg.addr = p;
  scm_vm(&vm);
  return(vm.ret.entry);
}

char *scm_search_opcode_name(void *p)
{
  SCM_PRIM_TABLE *pt = scm_search_opcode_by_addr(p);
  return( (pt == NULL) ? NULL : pt->name);
}

static SOBJ *scm_print_op(SOBJ *code)
{
  int i;
  SCM_PRIM_TABLE *op;

  if (!scm_is_opcode_address(*code)) {
	port_putx(SCM_OUTP, code);
	port_puts(SCM_OUTP, ": ");
	scm_cprint(*code++);
	return(code);
  }

  if ((op = scm_search_opcode_by_addr(*code)) != NULL) {
	port_putx(SCM_OUTP, code);  port_puts(SCM_OUTP, ": ");
	port_puts(SCM_OUTP, op->name);
	code++;
	for (i = 0; i < op->following; i++) {
	  scm_putc(' '); scm_write2(*code++, NULL);
	}
	scm_putc('\n');
	return(code);
  }	
  return(NULL);
}

SOBJ scm_disassemble(SOBJ *code, int nslots)
{
  SOBJ *limit;
  limit = code + nslots;

  while(code < limit) {
	code = scm_print_op(code);
  }
  return(NULL);
}
