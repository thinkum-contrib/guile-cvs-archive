/* -*- tab-width:4; -*- */
/*
 * Continuations
 */
#include "s.h"
#include "cont.h"

#ifndef STACK_GROWS_DOWN
#define STACK_GROWS_DOWN
#endif

void scm_cont_mark(SOBJ cont)
{
}

void scm_cont_sweep(SOBJ cont)
{
}

static long cstack_depth()
{
  void *limit;
#ifdef STACK_GROWS_DOWN
  return( (void*)scm_cstack_limit - (void*)&limit );
#else
  return( (void*)&limit - (void*)scm_cstack_limit );
#endif
}

SOBJ scm_mkcont(SOBJ proc)
{
  SOBJ z;
  SCM_ContAux *c;

  z = scm_newcell(SOBJ_T_CONT);
  c = scm_must_alloc(sizeof(SCM_ContAux));
  SCM_CONT(z) = c;
  
  /* save content of C stack */
  c->cbase = &z;
  c->cstack_len = cstack_depth();
  c->cstack = scm_must_alloc(c->cstack_len);
  memcpy(c->cstack, c->cbase, c->cstack_len);

  /* save content of data stack */
  c->dstack_len = scm_stack_limit - scm_sp;
  c->dstack = scm_must_alloc(c->dstack_len * sizeof(SOBJ *));
  memcpy(c->dstack, scm_stack, c->dstack_len * sizeof(SOBJ*));
  
#ifdef OLD
  /* save contents of return stack */
  c->rstack_len = scm_rstack_limit - scm_rp;
  c->rstack = scm_must_alloc(c->rstack_len * sizeof(SOBJ *));
  memcpy(c->rstack, scm_stack, c->rstack_len * sizeof(SOBJ*));
#endif
  return(z);
}

static SOBJ call_cc_escv;

SOBJ scm_call_cc(SOBJ proc)		/* the procedure to call */
{
  printf("Got a call/cc\n");
  return(scm_undefined);
}


void scm_cc_throw(SOBJ cont, SOBJ val)
{
  static SCM_ContAux *c;
  union { SOBJ stack_end;  SOBJ hole[1024]; } u;

#ifdef STACK_GROWS_DOWN
  if ((void*)&u.stack_end > CONT(cont)->cbase) 
	scm_cc_throw(cont, val);


  call_cc_escv = val;			/* save escape function value */
  c = CONT(cont);

  /* restore data stack */
  memcpy(scm_stack_limit - c->dstack_len, 
		 c->dstack,
		 c->dstack_len * sizeof(SOBJ));

#ifdef OLD
  /* restore return stack */
  memcpy(scm_rstack_limit - c->rstack_len,
		 c->rstack,
		 c->rstack_len * sizeof(SOBJ));
#endif
  
  /* restore the c stack */
  memcpy(c->cbase, c->cstack, c->cstack_len);
  longjmp(c->env, JUMP_THROW);

#else  /* STACK_GROWN_UP */
#error "Stack must grow down"
#endif
}
