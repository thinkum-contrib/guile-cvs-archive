/*
 * Include for scheme continuations
 *
 */

typedef struct {
  jmp_buf	env;
  void 		*cbase;				/* base addresse where to restore */
  void		*cstack;
  long		cstack_len;

  SOBJ		*dstack;
  long		dstack_len;

  SOBJ 		*rstack;
  long		rstack_len;
} SCM_ContAux;

#define CONT(x)			((SCM_ContAux*)((x)->data.cont.data))
#define CONT_ENV(x)		CONT(x)->env
#define CONT_BASE(x)	CONT(x)->cbase


#define JUMP_THROW	2


