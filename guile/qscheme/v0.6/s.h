/* -*- tab-width:4; -*- */
/*
 * My small scheme: include file
 *
 * $Id$
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <setjmp.h>
#include <limits.h>

#ifdef SCM_WITH_THREADS
# ifdef HAVE_LIBPTHREAD
# include <pthread.h>
# include <semaphore.h>
# include <signal.h>
# else
# error "No posix threads found"
# endif
#endif

#include <gmp.h>

#ifndef LONG_MIN
#define LONG_MIN	0x80000000
#endif
#ifndef LONG_MAX
#define LONG_MAX	0x7fffffff
#endif

#define NORETURN	__attribute__ ((noreturn))
#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

typedef unsigned char Byte;
typedef unsigned long Ulong;
typedef unsigned int  Uint;

#define Sobject struct _Sobject
typedef Sobject *SOBJ;

typedef SOBJ (*SCM_CPRIM)();

#include "port.h"

/*-- table holding the primitives */
typedef const struct {
  void *address;				/* the address to jump to */
  char *name;					/* the print name of the opcode */
  short nargs;					/* the number of args | -1 if var args */
  char following;				/* number of compiled arguments
								   following this opcode */
  char terminates;				/* true when this opcode terminates */
  char varargs;					/* when 1, must have mark before call */
} SCM_PRIM_TABLE;

/*-- string buffer */
#define SCM_STRBUF_MIN_SIZE		8
/* get profiling informations for strings */
/*#define SCM_STRBUF_PROFILE		1 */

/* String buffer utilities */
typedef struct {
  int	len;
  int 	max;
#ifdef SCM_STRBUF_PROFILE
  int 	nalloc;
  int	nrealloc;
  int	bmove;
#endif
  char 	str[1];					/* the string itself */
} SCM_STRBUF;


/*-- environment frame struct: stored in the env heap and in the heap */

#define SCM_EnvFrame	struct _SCM_EnvFrame
SCM_EnvFrame {
  SOBJ			nslots;			/* number of slots */
  SOBJ			binding[1];		/* values */
};

#define SCM_EnvFrameHeaderSize 	(sizeof(SCM_EnvFrame) - sizeof(SOBJ))


/*-- continuation frame structure, as stored on the stack */

#define SCM_ContFrame 	struct _SCM_ContFrame
SCM_ContFrame {
  SCM_ContFrame *next;			/* pointer to next continuation */
  SOBJ			env;			/* pointer to environment */
  SOBJ			*ip;			/* pointer to next ip */
};

/*-- array structure */

#define SCM_Array		struct _SCM_Array
SCM_Array {
  long size;					/* the current size (nrof slots) */
  long alloced;					/* the allocated size (nrof slots) */
  SOBJ item[1];					/* the first data slot is here :) */
};

/*-- code structure */

#define SCM_Code		struct _SCM_Code
SCM_Code {
  SOBJ		envlist;			/* chain of env symbols */
  long		size;				/* number of instruction in code */
  short		nargs;				/* number of arguments */
  short 	optargs;			/* flag: true if optionnal arguments */
  short		nlocals;			/* number of local variables */
  SOBJ 		code[1];			/* instructions follow here */
};

/*-- external function : used by dynamic loader */

#define SCM_EF_MAX_ARGS		16		
#define SCM_ExtFunc			struct _SCM_ExtFunc

SCM_ExtFunc {
  void *func;					/* ptr to func */
  Byte	func_type;				/* ignored now: type of function (C assumed) */
  short	return_t;				/* return type */
  short	argc;					/* arg count */
  Byte	vararg;					/* variable argument flag */
  short	arg_t[SCM_EF_MAX_ARGS];	/* argument type */
};

#define SCM_VarAux	struct _SCM_VarAux
SCM_VarAux {
  char *name;					/* type name */
  SOBJ 	atom;					/* type atom */
  int	type;					/* type of variable */
  int 	size;					/* size in bytes */
  short	align;					/* alignement */
  SOBJ	(*get)(SOBJ var, void *ptr);				/* C getter */
  SOBJ 	getarg;										/* Scheme getter */
  void 	(*set)(SOBJ var, void *ptr, SOBJ value); 	/* C setter */
  SOBJ 	setarg;										/* Scheme setter */
};
/* NOTE: if type < 0: type is fully handled by get and set func.
 * If type >= 0 and scm_type_hook[type] defines ext2obj and or obj2ext,
 * data has to be converted by ext2obj() or obj2ext().
 */

/*-- VM registers */
#define SCM_vmRegisters		struct _SCM_vmRegisters
SCM_vmRegisters {
  SOBJ *sp;						/* the stack pointer */
  SOBJ *ip;						/* instruction pointer */
  SCM_ContFrame *cont;			/* current continuation frame */
  SOBJ env;						/* env chain */
};

/*-- VM interface */
#define SCM_VMD				struct _SCM_VMD
SCM_VMD {
  int 		code;				/* code  */
#ifdef SCM_WITH_THREADS
  pthread_t tid;				/* thread id */
  int 		tflags;				/* thread flags */
#	define SCM_THREAD_FINISHED	1 /* thread has exited */
#	define SCM_THREAD_DETACHED 	2 /* thread will be detached */
#	define SCM_THREAD_MAIN		4 /* main thread only */
  
#endif
  void 		*cstack_limit;		/* top of the C stack for this thread */
  void   	*cstack_ptr;		/* pointer to current sp. Only valid
                                   after a SCM_SIG_SUSPEND signal has
                                   been caught */
  
  SCM_vmRegisters 	reg;		/* vm registers */

  jmp_buf	errjmp;				/* Where to restart in case of
                                   exception. It should be the top
                                   level return point. */

  SOBJ 		*stack_base;		/* VM stack definition */
  SOBJ 		*stack_limit;
  int 		stack_size;
  
  SOBJ		thunk;				/* the thunk to execute when starting
                                   a new thread */

  int 		signal;				/* the signal got when suspended (THREADS)*/
  
  union {
	int 	opcode;				/* opcode, when requesting opcode :) */
	char 	*name;
	void 	*addr;
  } arg;
  
  union {
	void 			*ptr;		/* returned pointer */
	SOBJ 			obj;		/* returned object */
	SCM_PRIM_TABLE 	*entry;		/* returned entry */
  } ret;
};

enum SCM_VM_does {
  SCM_VM_DO_INIT = 0,
  SCM_VM_DO_EXECUTE,
  SCM_VM_DO_GET_OPCODE,			/* return a ptr to maching vm symbol entry*/
  SCM_VM_DO_GET_OPCODE_BY_NAME,
  SCM_VM_DO_GET_OPCODE_BY_ADDR,
  SCM_VM_DO_MAX
};

#ifdef SCM_WITH_THREADS
/*** same as in Boehm's GC */
#define SCM_SIG_SUSPEND		SIGPWR
#define SCM_SIG_RESUME		SIGXCPU
#endif

/*-- catch context */
#define SCM_CatchContext	struct _SCM_CatchContext
SCM_CatchContext {
  SOBJ tag;						/* tags */
  SOBJ handler;					/* handler function */
  SOBJ unwind;					/* unwind functions */
  SCM_vmRegisters	vm;			/* vm register at catch time */
  jmp_buf env;					/* execution context */
};

/*-- hash descriptor */

#define SCM_MAX_HASH_DEPTH	3

#define SCM_Hash	struct _SCM_Hash
SCM_Hash {
  SOBJ *hash;					/* hash array */
  short	type;					/* hash type */
  Uint 	hsize;					/* size of hash array */
  Uint 	nkeys;					/* number of keys in hash */
  Uint 	maxkeys;				/* max number of key for this hash */
};

/*-- modules */
#define SCM_Module	struct _SCM_Module
SCM_Module {
  SOBJ 	name;
  SOBJ 	symbols;
  SOBJ 	imports;
  SOBJ 	exports;
  int	export_all;				/* TRUE when all symbols are exported */
};

/*-- objects */

#define SCM_ObjSlotDesc		struct _SCM_ObjSlotDesc
SCM_ObjSlotDesc {
  SOBJ 	name;
  int 	index;
  SOBJ	getter;
  SOBJ	setter;
};
	
#define SCM_ObjSlotAux		struct _SCM_ObjSlotAux
SCM_ObjSlotAux {
  int 				nslots;		/* number of slots */
  SCM_ObjSlotDesc	desc[1];	/* descriptor for each slots */
};

#define SCM_ObjValue		struct _SCM_ObjValue
SCM_ObjValue {
};

/*-- the mark for GC (msb of type field) */
#define SCM_GCMARK_MASK		(1L << ((sizeof(short)*8)-1))

#define SCM_GCBIT_SET(x) 	(x)->type |= SCM_GCMARK_MASK
#define SCM_GCBIT_CLR(x)	(x)->type &= ~(SCM_GCMARK_MASK)
#define SCM_GCBIT(x)		((x)->type & SCM_GCMARK_MASK)

extern int scm_in_gc;			/* flag: true during gc */


/*-- the circular bit mask, used during list traversal.  Note that
 * gcbit and circbit may be the same, because they should normally not
 * be used at same time.
 */
#define SCM_CIRCULAR_MASK	(1L << ((sizeof(short)*8)-2))

#define SCM_CIRCBIT_SET(x) 	(x)->type |= SCM_CIRCULAR_MASK
#define SCM_CIRCBIT_CLR(x)	(x)->type &= ~(SCM_CIRCULAR_MASK)
#define SCM_CIRCBIT(x)		((x)->type & SCM_CIRCULAR_MASK)

/*-- cell to hold a full or a partial scheme object (partial objects
 * have some more memory allocated
 */
Sobject {
  unsigned short type;		/* type and gcmark: gcmark is the msb */
  union {
	struct { double value; 								} fnum;
	struct { MP_INT *value; 							} bnum;
	struct { SOBJ car;  			SOBJ cdr; 			} pair;
	struct { char *name;			SOBJ next; 			} atom;
	struct { SOBJ name;  			SOBJ value; 		} symbol;
	struct { SOBJ name;				int  ofs;			} lsymbol;
	struct { SCM_Module *aux;							} module;
	struct { char c;									} chr;
	struct { SCM_STRBUF *strbuf;						} string;
	struct { SCM_PRIM_TABLE *entry; 					} prim;
	struct { SCM_CPRIM fn;  		int nargs; 			} cprim;
	struct { SOBJ *code; 			long size;			} code;
	struct { SOBJ env;				SCM_Code *code; 	} proc;
	struct { SOBJ env;				SOBJ code; 			} closure;
	struct { SCM_EnvFrame *frame;	SOBJ next; 			} env;
	struct { SOBJ func;				SOBJ code;			} macro;
	struct { PORT *descr; 								} port;
	struct { void *data; 								} cont;
	struct { SCM_Array *descr; 							} array;
	struct { SCM_Hash *h;								} hash;
	/* POINTER and AUX must be the same */
	struct { void *data; 			short attrib;		} pointer;
	struct { void *aux; 			short attrib;		} aux;
	struct { SCM_ExtFunc *aux; 							} extfunc;
	struct { SCM_VarAux *aux;		void *addr;			} var;
	struct { void (*fn)(SCM_vmRegisters *); 			} vmfunc;
	struct { SCM_CatchContext *cntxt;  					} ccntxt;
  } data;
};

/* type descriptor */
typedef struct {
  long execute;					/* execution address */
  char *name;					/* type name */
  void (*mark)(SOBJ obj);		/* func to mark this type of object */
  void (*sweep)(SOBJ obj);		/* func to free this type of object */
  void (*print)(SOBJ obj, PORT *p);	/* write human readable rep of object */
  void (*write)(SOBJ obj, PORT *p);	/* write machine readable rep of object */

  SCM_STRBUF *(*tostr)(SCM_STRBUF *sb, SOBJ obj, int raw);
										/* convert to string */

  /* token reconizer / parser  */
  int  (*creconize)(PORT *p, int c);	/* starting char reconizer */
  SOBJ (*cparse)(PORT *p, int c);		/* parser for type */
  int  (*wreconize)(PORT *p, char *s);	/* full word reconizer */
  SOBJ (*wparse)(PORT *p, char *s);		/* full word parser */

  /* object comparer */
  SOBJ (*compare)(SOBJ obj1, SOBJ obj2);	/* fonction to compare */

  SOBJ (*ext2obj)(int type, void *ext);	/* create SOBJ from external pointer */
  void *(*obj2ext)(SOBJ obj);			/* reverse operation */

  SOBJ finalize;				/* optionnal finalizer */
} SOBJ_TYPE_DESCR;

/* IMPORTANT: symbol value must be accessible as SCM_CDR(symbol)
 * see compile_set.
 */

/*typedef Sobject *SOBJ; */
#define SOBJ_INUM_TAG	1
#define SOBJ_INUM_MASK	(~1)
#define SOBJ_INUM_SHIFT	1

#define SOBJ_INUM_MAX	(0x3fffffffL)
#define SOBJ_INUM_MIN	(~(SOBJ_INUM_MAX))

#define SCM_INUM_RANGE(x)	(((x) >= SOBJ_INUM_MIN) && ((x) <= SOBJ_INUM_MAX)) 
						 
#define SCM_INUMP(x)	((long)(x) & SOBJ_INUM_TAG)

/*!!!! keep this list sync with scm_type_hook[] in s.c !!!!*/
enum SOBJ_TYPES {
  SOBJ_T_VOID=0,
  SOBJ_T_PAIR,
  SOBJ_T_INUM,
  SOBJ_T_FNUM,
  SOBJ_T_BNUM,
  SOBJ_T_ATOM,
  SOBJ_T_KEYWORD,
  SOBJ_T_SYMBOL,
  SOBJ_T_LSYMBOL,
  SOBJ_T_LABEL,					/* share the same struct than LSYMBOL */
  SOBJ_T_MODULE,
  SOBJ_T_CHAR,
  SOBJ_T_STRING,
  SOBJ_T_PRIM,
  SOBJ_T_CPRIM,
  SOBJ_T_SYNTAX,				/* same struct as cprim */
  SOBJ_T_CODE,
  SOBJ_T_PROC,
  SOBJ_T_CLOSURE,
  SOBJ_T_ENV,
  SOBJ_T_MACRO,
  SOBJ_T_PORT,
  SOBJ_T_BOOLEAN,
  SOBJ_T_UNBOUND,
  SOBJ_T_UNDEFINED,
  SOBJ_T_EOF,
  SOBJ_T_CONT,
  SOBJ_T_ARRAY,
  SOBJ_T_HASH,
  SOBJ_T_POINTER,
  SOBJ_T_EXTFUNC,
  SOBJ_T_VAR,
  SOBJ_T_VMFUNC,				/* can access vm registers */
  SOBJ_T_CCNTXT,				/* catch context */
  SOBJ_T_USER
};

#define SOBJ_T_MAX			256
#define SOBJ_T_FREE			SOBJ_T_MAX /* marker for free cells */

/*-- accessing fields */
#define SCM_OBJREF(x)		(x)
#define SCM_DATA(x)			((x)->data)
#define SCM_VALUE(x,t,f)	((x)->data.t.f)

/*-- some primitive macros */
#define SCM_INUM(x)			((long)(x) >> SOBJ_INUM_SHIFT)
#define SCM_MKINUM(x)		((SOBJ)(((long)(x)<<SOBJ_INUM_SHIFT)|SOBJ_INUM_TAG))

#define SCM_MKBOOL(x)		((x) ? scm_true : scm_false)
#define SCM_FALSEP(x)		((x) == scm_false)
#define SCM_TRUEP(x)		((x) != scm_false)

#define SCM_FNUM(x)			SCM_VALUE(x,fnum,value)
#define SCM_BNUM(x)			SCM_VALUE(x,bnum,value)

#define SCM_CAR(x)			SCM_VALUE(x,pair,car)
#define SCM_CDR(x)			SCM_VALUE(x,pair,cdr)

#define SCM_ATOM_NAME(x)	SCM_VALUE(x,atom,name)
#define SCM_ATOM_NEXT(x)	SCM_VALUE(x,atom,next)

/*-- note: symbol value is a pair when used in the local env
 * ??? don't know what this means
 */
enum SCM_KEYWORD_WRITE_MODE {
  SCM_KEYW_WRITE_DEFLT = 0,
  SCM_KEYW_WRITE_DSSL,
  SCM_KEYW_WRITE_OTHER
};


#define SCM_KEYW_NAME(x)	SCM_VALUE(x,symbol,name)

/* prefix for generated symbols.
 * read will not accept symbol with this prefix as valid one.
 * Note: the '@' is not part of valid symbol starter (r5rs 7 1 1)
 */
#define SCM_GENSYM_PREFIX	"@G"

/* symbols: name are atoms */
#define SCM_SYM_NAME(x)		SCM_VALUE(x,symbol,name)
#define SCM_SYM_VALUE(x)	SCM_VALUE(x,symbol,value)

/* local symbols: name are atoms */
#define SCM_LSYM_NAME(x)	SCM_VALUE(x,lsymbol,name)
#define SCM_LSYM_OFS(x)		SCM_VALUE(x,lsymbol,ofs)

/* local labels: names are atoms */
#define SCM_LABEL_NAME(x)	SCM_VALUE(x,lsymbol,name)
#define SCM_LABEL_OFS(x)	SCM_VALUE(x,lsymbol,ofs)

#define SCM_MODULE(x)		SCM_VALUE(x,module,aux)

#define SCM_CHAR(x)			SCM_VALUE(x,chr,c)

#define SCM_STR(x)			SCM_VALUE(x,string,strbuf)
#define SCM_STR_LEN(x)		SCM_STR(x)->len
#define SCM_STR_VALUE(x)	SCM_STR(x)->str

#ifdef OLD
#define SCM_STR_LEN(x)		SCM_VALUE(x,string,len)
#define SCM_STR_VALUE(x)	SCM_VALUE(x,string,value)

#define SCM_STR_QTUM		32
#define SCM_STR_QTUM1		(SCM_STR_QTUM-1)
#define scm_str_lenq(x)		(SCM_ALIGN_OFS(x,SCM_STR_QTUM))
#endif
/* #define scm_str_lenq(x) (((x)+SCM_STR_QTUM1) & ~SCM_STR_QTUM1) */


#define SCM_PRIM(x)			SCM_VALUE(x,prim,entry)

#ifdef COMMENT
#define SCM_PRIM_ADDR(x)   	SCM_PRIM_ENTRY(x)->address
#define SCM_PRIM_NARGS(x)	SCM_PRIM_ENTRY(x)->nargs
#endif

#define SCM_CPRIM_FUNC(x)	SCM_VALUE(x,cprim,fn)
#define SCM_CPRIM_NARGS(x)	SCM_VALUE(x,cprim,nargs)

#define SCM_SYNTAX_FUNC(x)	SCM_VALUE(x,cprim,fn)

#define SCM_CODE_SIZE(x)	SCM_VALUE(x,code,size)
#define SCM_CODE_CODE(x)	SCM_VALUE(x,code,code)

#define SCM_PROC_ENV(x)		SCM_VALUE(x,proc,env)
#define SCM_PROC_CODE(x)	SCM_VALUE(x,proc,code)

#define SCM_ENV_FRAME(x)	SCM_VALUE(x,env,frame)
#define SCM_ENV_NEXT(x)		SCM_VALUE(x,env,next)

#define SCM_CLOSURE_CODE(x)	SCM_VALUE(x,closure,code)
#define SCM_CLOSURE_ENV(x)	SCM_VALUE(x,closure,env)

#define SCM_MACRO_CODE(x)	SCM_VALUE(x,macro,code)
#define SCM_MACRO_FUNC(x)	SCM_VALUE(x,macro,func)

#define SCM_PORT(x)			SCM_VALUE(x,port,descr)

#define SCM_FILE_PORTP(x)	(SCM_PORT(x)->type == PORT_T_FILE)
#define SCM_STRING_PORTP(x)	(SCM_PORT(x)->type == PORT_T_STRING)
#define SCM_READ_PORTP(x)	((SCM_PORT(x)->io_flag & PORT_IO_R) != 0)
#define SCM_WRITE_PORTP(x)	((SCM_PORT(x)->io_flag & PORT_IO_W) != 0)

#define SCM_CONT(x)			SCM_VALUE(x,cont,data)

#define SCM_ADESCR(x)		SCM_VALUE(x,array,descr)
#define SCM_ARRAY(x)		(SCM_ADESCR(x))->item
#define SCM_ASIZE(x)		(SCM_ADESCR(x))->size
#define SCM_AMAX(x) 		(SCM_ADESCR(x))->alloced
#define SCM_AREF(x,i)		(SCM_ADESCR(x))->item[i]

#define SCM_HASH(x)			SCM_VALUE(x,hash,h)

/*-- types of hash */
#define SCM_HASH_T_GEN		0
#define SCM_HASH_T_SYMBOL	1
#define SCM_HASH_T_ATOM		2


#define SCM_POINTER(x)				SCM_VALUE(x,pointer,data)
#define SCM_POINTER_ATTRIB(x)		SCM_VALUE(x,pointer,attrib)

/*** Pointers attributes */
/* points to an allocated block that must be freed when sweeping */
#define SCM_POINTER_FLAG_ALLOCED	(1 << 0)

/* Points to a SOBJ that must be marked during GC */
#define SCM_POINTER_FLAG_CELL		(1 << 1)


#define SCM_EXTFUNC(x)				SCM_VALUE(x,extfunc,aux)

#define SCM_VAR_ADDR(x)				SCM_VALUE(x,var,addr)
#define SCM_VAR_AUX(x)				SCM_VALUE(x,var,aux)

#define SCM_VMFUNC(x)				SCM_VALUE(x,vmfunc,fn)

#define SCM_CATCH_CONTEXT(x)		SCM_VALUE(x,ccntxt,cntxt)
#define SCM_CATCH_CONTEXT_TAG(x)	SCM_CATCH_CONTEXT(x)->tag
#define SCM_CATCH_CONTEXT_ENV(x)	SCM_CATCH_CONTEXT(x)->env
#define SCM_CATCH_CONTEXT_VM(x)		SCM_CATCH_CONTEXT(x)->vm
#define SCM_CATCH_CONTEXT_HANDLER(x)	SCM_CATCH_CONTEXT(x)->handler
#define SCM_CATCH_CONTEXT_UNWIND(x)		SCM_CATCH_CONTEXT(x)->unwind

#define SCM_AUX(x)					SCM_VALUE(x,aux,aux)

/*-- type predicates */
/*#define SCM_OBJTYPE(x)	(SCM_INUMP(x)?SOBJ_T_INUM:(x?SCM_OBJREF(x)->type:-1)) */
#define SCM_OBJTYPE(x)	\
	(SCM_INUMP(x)?SOBJ_T_INUM:(x?SCM_OBJREF(x)->type & ~(SCM_GCMARK_MASK):-1))

#define SCM_USERTYPEP(x)	(SCM_OBJTYPE(x)>=SOBJ_T_USER && \
							 SCM_OBJTYPE(x)<SOBJ_T_MAX)

#define SCM_TYPEP(x,t)		(SCM_OBJTYPE(x) == t)
	 
#define SCM_NULLP(x)		((x) == NULL)
#define SCM_NNULLP(x)		((x) != NULL)
#define SCM_PAIRP(x)   		(SCM_OBJTYPE(x) == SOBJ_T_PAIR)
#define SCM_FNUMP(x)   		(SCM_OBJTYPE(x) == SOBJ_T_FNUM)
#define SCM_BNUMP(x)   		(SCM_OBJTYPE(x) == SOBJ_T_BNUM)
#define SCM_ATOMP(x)		(SCM_OBJTYPE(x) == SOBJ_T_ATOM)
#define SCM_KEYWORDP(x)   	(SCM_OBJTYPE(x) == SOBJ_T_KEYWORD)
#define SCM_SYMBOLP(x)   	(SCM_OBJTYPE(x) == SOBJ_T_SYMBOL)
#define SCM_LSYMBOLP(x)   	(SCM_OBJTYPE(x) == SOBJ_T_LSYMBOL)
#define SCM_MODULEP(x)   	(SCM_OBJTYPE(x) == SOBJ_T_MODULE)
#define SCM_CHARP(x)		(SCM_OBJTYPE(x) == SOBJ_T_CHAR)
#define SCM_STRINGP(x)		(SCM_OBJTYPE(x) == SOBJ_T_STRING)
#define SCM_PRIMP(x)   		(SCM_OBJTYPE(x) == SOBJ_T_PRIM)
#define SCM_CPRIMP(x)   	(SCM_OBJTYPE(x) == SOBJ_T_CPRIM)
#define SCM_SYNTAXP(x)   	(SCM_OBJTYPE(x) == SOBJ_T_SYNTAX)
#define SCM_CODEP(x)   		(SCM_OBJTYPE(x) == SOBJ_T_CODE)
#define SCM_PROCP(x)   		(SCM_OBJTYPE(x) == SOBJ_T_PROC)
#define SCM_CLOSUREP(x)		(SCM_OBJTYPE(x) == SOBJ_T_CLOSURE)
#define SCM_ENVP(x)   		(SCM_OBJTYPE(x) == SOBJ_T_ENV)
#define SCM_MACROP(x)		(SCM_OBJTYPE(x) == SOBJ_T_MACRO)
#define SCM_PORTP(x)		(SCM_OBJTYPE(x) == SOBJ_T_PORT)
#define SCM_BOOLEANP(x)		(SCM_OBJTYPE(x) == SOBJ_T_BOOLEAN)
#define SCM_ARRAYP(x)		(SCM_OBJTYPE(x) == SOBJ_T_ARRAY)
#define SCM_HASHP(x)		(SCM_OBJTYPE(x) == SOBJ_T_HASH)
#define SCM_POINTERP(x)		(SCM_OBJTYPE(x) == SOBJ_T_POINTER)
#define SCM_EXTFUNCP(x)		(SCM_OBJTYPE(x) == SOBJ_T_EXTFUNC)

#define SCM_VARP(x)			(SCM_OBJTYPE(x) == SOBJ_T_VAR)

#define SCM_NUMBERP(x)		(x && (SCM_INUMP(x) || SCM_FNUMP(x) || SCM_BNUMP(x)))
#define SCM_EXACTP(x)		(SCM_INUMP(x) || SCM_BNUMP(x))

#define SCM_REALP(x)		(SCM_NUMBER(x))
#define SCM_INTEGERP(x)		(SCM_INUMP(x) || SCM_BNUMP(x))

#define SCM_EQ(x,y)		(SCM_OBJREF(x) == SCM_OBJREF(y))
#define SCM_CAAR(x)		SCM_CAR(SCM_CAR(x))
#define SCM_CDAR(x)		SCM_CDR(SCM_CAR(x))
#define SCM_CDDR(x)		SCM_CDR(SCM_CDR(x))
#define SCM_CADR(x)		SCM_CAR(SCM_CDR(x))
#define SCM_CADDR(x)	SCM_CAR(SCM_CDR(SCM_CDR(x)))

#define SCM_GETNUM(x) \
(SCM_INUMP(x) ? SCM_INUM(x) : \
 ((SCM_OBJREF(x)->type == SOBJ_T_FNUM) ? SCM_FNUM(x) : 0))

#define SCM_ANYSTRP(x)	\
  (SCM_STRINGP(x)||SCM_ATOMP(x)||SCM_KEYWORDP(x)||SCM_SYMBOLP(x))

static char *opc_str[];

/*-- config */
#define SCM_SYM_HASH_SIZE	101

#ifndef SCM_DEFAULT_LIB_PATH
#define SCM_DEFAULT_LIB_PATH "."
#endif

/*-- list building macros */
#define SCM_LIST1(a)		scm_cons((a), NULL)
#define SCM_LIST2(a,b) 		scm_cons((a), SCM_LIST1(b))
#define SCM_LIST3(a,b,c)	scm_cons((a), SCM_LIST2((b),(c)))
#define SCM_LIST4(a,b,c,d)	scm_cons((a), SCM_LIST3((b),(c),(d)))
#define SCM_LIST5(a,b,c,d,e)	scm_cons((a), SCM_LIST4((b),(c),(d),(e)))
#define SCM_LIST6(a,b,c,d,e,f)	scm_cons((a), SCM_LIST5((b),(c),(d),(e),(f)))

#define streq(a,b)	(strcmp(a,b)==0)

/*-- Align this to boundary: assume boundary is a power of 2 */
#define SCM_ALIGN_OFS(ofs,bound) ((((Ulong)(ofs))+((bound)-1)) & ~((bound)-1))
#define SCM_ALIGN_PTR(ofs,bound) (void*)(SCM_ALIGN_OFS(ofs,bound))

#define SCM_ALIGNOF(type)	(__alignof__(type))

/* number.c: special definitions */
SOBJ		scm_exp(SOBJ x);
SOBJ		scm_log(SOBJ x);
SOBJ		scm_log10(SOBJ x);
SOBJ		scm_sin(SOBJ x);
SOBJ		scm_cos(SOBJ x);
SOBJ		scm_tan(SOBJ x);
SOBJ		scm_asin(SOBJ x);
SOBJ		scm_acos(SOBJ x);


/* standard ports */

extern SOBJ scm_in_port;
extern SOBJ scm_out_port;
extern SOBJ scm_err_port;
extern SOBJ scm_eof;

/* quick access to PORT * struct */

#define SCM_INP		SCM_PORT(scm_in_port)
#define SCM_OUTP	SCM_PORT(scm_out_port)
#define SCM_ERRP	SCM_PORT(scm_err_port)


/* err code used by longjmp */
enum SCM_ERR_LONGJMP {
  SCM_ERR_NONE	= 0,
  SCM_ERR_ABORT,
  SCM_ERR_THROW,
  SCM_ERR_MAX };

#include "sproto.h"

#define scm_sp			scm_vmd()->reg.sp
#define scm_stack		scm_vmd()->stack_base
#define scm_stack_size	scm_vmd()->stack_size
#define scm_stack_limit scm_vmd()->stack_limit

#ifdef SCM_WITH_THREADS

#define SCM_THREAD(x)		((SCM_VMD*)(SCM_AUX(x)))
#define SCM_MUTEX(x)		((pthread_mutex_t *)SCM_AUX(x))
#define SCM_SEMAPHORE(x)	((sem_t *)SCM_AUX(x))

#define SCM_THREADP(x)		(SCM_OBJTYPE(x) == SOBJ_T_THREAD)
#define SCM_MUTEXP(x)		(SCM_OBJTYPE(x) == SOBJ_T_MUTEX)
#define SCM_SEMAPHOREP(x)	(SCM_OBJTYPE(x) == SOBJ_T_SEMAPHORE)

#define scm_vmd()			((SCM_VMD*)pthread_getspecific(scm_vmd_key))

extern pthread_mutex_t scm_heap_locker;

#define SCM_HEAP_LOCK()		pthread_mutex_lock(&scm_heap_locker)
#define SCM_HEAP_UNLOCK()	pthread_mutex_unlock(&scm_heap_locker)

#else /* no THREADS */

extern SCM_VMD 		scm_vmdata;

#define scm_vmd()		(&scm_vmdata)

#define SCM_HEAP_LOCK()		
#define SCM_HEAP_UNLOCK()	

#endif /* SCM_WITH_THREADS */

/* toplevel restart point */
#define scm_errjmp		(scm_vmd()->errjmp)

#ifdef HAVE_FUNC_STR
#define SCM_ERR(msg,obj)	scm_internal_err(__FUNCTION__,msg,obj)
#else
#define SCM_ERR(msg,obj)	scm_internal_err(NULL,msg,obj)
#endif

