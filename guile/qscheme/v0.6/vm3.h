/*
 * Virtual Machine version 3
 */
#define ENVFRAME	struct _ENVFRAME
ENVFRAME {
  unsigned short  type;			/* mimic Sobject for env */
  SCM_EnvFrame 	*frame;
  ENVFRAME		*next;
  SOBJ			nslots;
  SOBJ			slot[0];
};

/* slot offset for ENVFRAME */
#define ENV_CELL_TYPE 		0
#define ENV_FRAME			1
#define ENV_NEXT			2
#define ENV_NSLOTS			3
#define ENV_SLOT			4
#define ENV_HEADER_SLOTS	4

#define CONTEXT		struct _CONTEXT
CONTEXT {
  CONTEXT	*next;
  ENVFRAME	*env;
  SOBJ		*ip;
};

/* slot offset for CONTEXT */
#define CONT_NEXT			0
#define CONT_ENV			1
#define CONT_IP				2
#define CONT_TOTAL_SLOTS	3


typedef struct {
  void *start, *end;	/* start, end address of an opcode */
  char *name;			/* name of proc */
  char arity;			/* number of argument expected on stack */
  char follow;			/* number of words following the code */
  char term;
  char vararg;
} SCM_VM3_PRIM;

enum SCM_VM3_DOERS {
  SCM_VM3_EXECUTE = 0,
  SCM_VM3_INIT,
  SCM_VM3_ASSEMBLE,
  SCM_VM3_DISASSEMBLE,
  SCM_VM3_MAX_DOERS
};

#define FAST_DISPATCH

#define SCM_CONT_NSLOTS	(sizeof(SCM_ContFrame)/sizeof(SOBJ))
#define SCM_ENV0_NSLOTS	2

enum SCM_VM3_OPCODES {
#include "vm3-ops.h"
};

#define TOS			sp[-1]
#define spush(x)	*sp++ = (x)
#define spop(x)		(x) = *(--sp)
#define supdate()
#define sresync()	vm->reg.sp = sp;

#ifdef FAST_DISPATCH

#define START_VMCODE
#define END_VMCODE

#define NEXT 			goto *(*ip++)

#ifndef VM3_DEBUG
#define INSTR(name)		l_##name##:
#define END(name)		e_##name##: NEXT
#else

#define DUMP(what,name)	\
	fprintf(stderr, "VM3:%s:%s\nVM3:cont=%p env=%p ip=%p sp=%p\n",what,#name,cont,env,ip,sp)
#define INSTR(name)		l_##name##: DUMP("enter",name);
#define END(name)		e_##name##: DUMP("leave",name); NEXT
#endif

#else
#define START_VMCODE	switch(*ip++) {
#define END_VMCODE		}

#define NEXT			break
#define INSTR(name)		case SCM_OP_##name##:

#endif

/* declaration of a new opcode in the SCM_VM3_PRIM structure */
#define DECL_REF(n) &&l_##n,&&e_##n, "%"#n
#define DECL(name,arity,follow,term) {DECL_REF(name),arity,follow,term}


