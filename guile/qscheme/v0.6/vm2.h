/*
 * virtual machine
 *
 * $Id$
 */ 

#define FAST					/* 2 stacks and TOS  */
#undef  LIGHTSPEED				/* use system stack */

#ifdef FAST
#define FORCE_REG				/* force register choice */
#define USE_TOS					/* don't use tos register */
#undef  USE_SYS_STACK			/* use the same system stack */

#ifdef __i386__
#define IPREG	asm("%esi")
#define SPREG	asm("%edi")
#define TOSREG	asm("%ebx")
#endif

#ifdef __sparc__
/* have trouble with sparc now. Does not want to work... :( */
#define IPREG	/*	asm("%g6") */

/* egcc (2.91.x) can use machine register for SP */
#if (__GNUC__ >= 2) && (__GNUC_MINOR__ >= 91)
#define SPREG	/*	asm("%g6") */
#endif

#define TOSREG		asm("%g7") 
#endif

#endif /* FAST */

#ifdef LIGHTSPEED
#define FORCE_REG				/* force register choice */
#undef  USE_TOS					/* don't use tos register */
#define USE_SYS_STACK			/* use the same system stack */

#define IPREG	asm("%esi")
#define SPREG	asm("%esp")
#define TOSREG
#endif

#ifndef IPREG
#define IPREG
#endif
#ifndef SPREG
#define SPREG
#endif
#ifndef TOSREG
#define TOSREG
#endif

/* when defined, check for stack under/overflow */
#define STACK_CHECKS

/* compress stack as much as possible when optionnal arguments are
 * involved */
#define STACK_OPT_COMPRESS

/* max number of cell we can push before next check */
#define STACK_OVERFLOW_GRACE	100

/* max number of argument with callc opcode */
#define SCM_OP_CALLC_MAX		10

/*-- opcodes : keep it sync with symbols[] in engine */
enum SCM_OPCODES {
  SCM_OP_NOP=0,
  SCM_OP_END,

  SCM_OP_DOLET,					/* proc: */
  SCM_OP_DOLETSTAR,
  SCM_OP_ENDLET_JUMP,			/* used only before a call/jump */

  SCM_OP_DROP,					/* drop value on stack */
  SCM_OP_PUSH,
  SCM_OP_PUSHV,
  SCM_OP_PUSHN,

  SCM_OP_PUSHL,					/* push value of a local assoc */
  SCM_OP_PUSHL0,				/* push value of a local assoc */
  SCM_OP_PUSHL1,				/* push value of a local assoc */
  SCM_OP_PUSHL2,				/* push value of a local assoc */
  SCM_OP_PUSHL3,				/* push value of a local assoc */

  SCM_OP_ALLOC,					/* alloc n slots on stack */

  SCM_OP_PUSHLIST,				/* push a list to the stack */

  SCM_OP_MARK,					/* set rsp */
  SCM_OP_MKCLOSURE,

  SCM_OP_ENDLET,				/* normal end of let clause */
  SCM_OP_RETURN,				/* return from call */

  SCM_OP_CALLC,					/* C prim with fixed number of args */
  SCM_OP_CALLC0,				/* C prim no args */
  SCM_OP_CALLC1,				/* C prim 1  arg  */
  SCM_OP_CALLC2,				/* C prim 2  args */
  SCM_OP_CALLC3,				/* C prim 3  args */
  SCM_OP_CALLC4,				/* C prim 4  args */
  SCM_OP_CALLC5,				/* C prim 5  args */
  SCM_OP_CALLC6,				/* C prim 6  args */
  SCM_OP_CALLC7,				/* C prim 7  args */
  SCM_OP_CALLC8,				/* C prim 8  args */
  SCM_OP_CALLC9,				/* C prim 9  args */
  SCM_OP_CALLC10,				/* C prim 10 args */
  SCM_OP_CALLS,					/* C prim with variable number of args */

  SCM_OP_NO_CALL,
  SCM_OP_JUMP,
  SCM_OP_CALL,
  SCM_OP_CALL_PRIM,
  SCM_OP_CALL_CPRIM,
  SCM_OP_CALL_CODE,
  SCM_OP_CALL_PROC,
  SCM_OP_CALL_CLOSURE,
  SCM_OP_CALL_MACRO,
  SCM_OP_CALL_EXTFUNC,
  SCM_OP_CALL_VMFUNC,

  SCM_OP_CATCH,					/* low level error handling */
  SCM_OP_UNCATCH,

  SCM_OP_STORE,					/* store global variable */
  SCM_OP_SETL,					/* local var store */
  SCM_OP_SETL0,					/* local var store */
  SCM_OP_SETL0DROP,				/* local var store */
  
  SCM_OP_GETVAR,
  SCM_OP_SETVAR,

  SCM_OP_BR_AND,				/* special branch for and */
  SCM_OP_BR_OR,					/* special branch for or */
  SCM_OP_BR_COND,				/* special branch for cond */
  SCM_OP_BR_WHILE,				/* special branch for while */
  SCM_OP_BRA,					/* branch allways */
  SCM_OP_BRF,					/* pop and branch if false */
  SCM_OP_BRT,					/* pop and branch if not false */
  
  SCM_OP_SAVE_R0,
  SCM_OP_LOAD_R0,

  SCM_OP_MAX };

#ifdef __VM2_INTERNAL__

/*-- how to push and pop to the machine */

#ifndef USE_TOS
#ifdef USE_SYS_STACK
#	define supdate()
#	define spush(x) { __asm__ __volatile__ ( "pushl %0" : : "g"(x) ); }
#	define spop(x)  { __asm__ __volatile__ ( "popl %0" : "=g"(x) : ); }
#	define TOS		sp[0]
#else
#	define supdate()
#	define spush(x)	{*(--sp) = (void*)(x); }
#	define spop(x)	{(x)=*sp++; }
#	define sdrop()	{ sp++; }
#	define TOS		sp[0]
#endif

#else
/* TOS is a cache for the top of the stack */
#define supdate()	{ *sp=TOS; }
#define spush(x)	{ supdate(); sp--;  TOS=(x); }
#define spop(x)		{ (x)=TOS;  sp++;  TOS=*sp; }
#define sdrop()		{ sp++; TOS=*sp; }
#endif

#ifndef USE_TOS
#ifdef USE_SYS_STACK
#	define supdate()
#	define spush(x) { __asm__ __volatile__ ( "pushl %0" : : "g"(x) ); }
#	define spop(x)  { __asm__ __volatile__ ( "popl %0" : "=g"(x) : ); }
#	define TOS		sp[0]
#else  /* ! USE_SYS_STACK */
#	define supdate()
#	define spush(x)	{*(--sp) = (void*)(x); }
#	define spop(x)	{(x)=*sp++; }
#	define sdrop()	{ sp++; }
#	define TOS		sp[0]
#endif	/* ! USE_SYS_STACK */

#else	/* USE_TOS */

#if USE_SYS_STACK
#	error "Use sys_stack and TOS not defined yet. Do it yourself"
#else /* !USE_SYS_STACK */
	/* TOS is a cache for the top of the stack */
#	define supdate()	{ *sp=TOS; }
#	define spush(x)	{ supdate(); sp--;  TOS=(x); }
#	define spop(x)		{ (x)=TOS;  sp++;  TOS=*sp; }
#	define sdrop()		{ sp++; TOS=*sp; }
#endif	/* USE_SYS_STACK */
#endif	/* USE_TOS */

#define sresync()	{ supdate();  scm_sp=(SOBJ*)sp;  }

#endif

/*-- forward and prototypes */

extern SCM_PRIM_TABLE *scm_prim_table;		/* opcode table */

void 		scm_vmfunc_mark(SOBJ obj);
void 		scm_vmfunc_sweep(SOBJ obj);

void        scm_engine_init();
SCM_PRIM_TABLE *scm_get_addr(int opc);
int         scm_is_opcode_address(void *p);
SCM_PRIM_TABLE *scm_search_opcode_address(char *name);
char 		*scm_search_opcode_name(void *p);
SOBJ        scm_disassemble(SOBJ *code, int nslots);
