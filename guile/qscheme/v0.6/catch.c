/* -*- tab-width:4; -*- */
/*
 * Catch/throw error handler
 */
#include "s.h"
#include "vm2.h"
#include "heap.h"
#include "stack.h"

SOBJ scm_catch_list, scm_thrown_tag, scm_thrown_msg;


/*** GC functions ***/
void scm_ccntxt_mark(SOBJ obj)
{
  SCM_CatchContext *c = SCM_CATCH_CONTEXT(obj);
  if (c) {
	if (c->tag)				scm_gc_mark(c->tag);
	if (c->handler != NULL) scm_gc_mark(c->handler);
	if (c->unwind != NULL)  scm_gc_mark(c->unwind);
	if (scm_is_pointer_to_heap(c->vm.env)) {
	  scm_gc_mark(c->vm.env);
	}
  }
}

void scm_ccntxt_sweep(SOBJ obj)
{
  if (SCM_CATCH_CONTEXT(obj))
	scm_free(SCM_CATCH_CONTEXT(obj));
  SCM_CATCH_CONTEXT(obj) = NULL;
}


/*-- make a new catch context node */
SOBJ scm_mkcatch()
{
  SOBJ new = scm_newcell(SOBJ_T_CCNTXT);
  SCM_CATCH_CONTEXT(new) = scm_must_alloc(sizeof(SCM_CatchContext));
  scm_mem_clear(SCM_CATCH_CONTEXT(new), sizeof(SCM_CatchContext));
  return(new);
}

/*-- throw an exception  */
/*S* (throw TAG MSG) => #undefined */
/*D* Send an exception of type TAG and message MSG to the first
  exception handler who has the TAG in it's tag list (or #t). If no
  matching handler are found, the default toplevel handler is
  called.*/
SOBJ scm_throw(SOBJ tag, SOBJ msg)
{
  SOBJ node, list, tagl;
  
#ifdef DEBUG
  scm_puts("*** throw: tag="); scm_cdisplay(tag);
  scm_puts(" msg=");  scm_cprint(msg);
#endif

  /*-- prepare to pass tag and msg to handler */
  scm_thrown_tag = tag;
  scm_thrown_msg = msg;

  if (tag == scm_false) {		/* throw to enclosing exception level */
	if (scm_catch_list == NULL) { /* top level exception */
	  longjmp(scm_errjmp, SCM_ERR_ABORT);
	} else {
	  node = SCM_CAR(scm_catch_list);
	  scm_catch_list = SCM_CDR(scm_catch_list);
	  longjmp(SCM_CATCH_CONTEXT_ENV(node), SCM_ERR_ABORT);
	}
  }
  if (tag == scm_true) {		/* throw to top level */
	scm_catch_list = NULL;
	longjmp(scm_errjmp, SCM_ERR_ABORT);
  }
  
  if (!SCM_ATOMP(tag)) {
	scm_puts("*** ERROR: tag "); scm_cdisplay(tag);
	scm_puts(" is not an atom\n");
	longjmp(scm_errjmp, SCM_ERR_ABORT);
  }
  for (list = scm_catch_list; list; list = SCM_CDR(list)) {
	node = SCM_CAR(list);
	tagl = SCM_CATCH_CONTEXT_TAG(node);

	/* note: #t as taglist match all tags */
	if (tagl == scm_true ||
		(SCM_PAIRP(tagl) && scm_memq(tag, tagl) != scm_false)) {
	  scm_catch_list = SCM_CDR(list); /* consume catchlist to next catch */
	  scm_thrown_tag = tag;	  scm_thrown_msg = msg;
	  longjmp(SCM_CATCH_CONTEXT_ENV(node), SCM_ERR_ABORT);
	}
  }
  /* no tag match: goto toplevel */
#ifdef DEBUG
  scm_puts("*** error: tag="); scm_cdisplay(tag);
  scm_puts(" message="); 
#endif
  scm_cprint(msg);
  longjmp(scm_errjmp, SCM_ERR_ABORT);
}


/*E* (segv) => #undefined */
/*D* Generates a segmentation fault by reading the content of address
  0x0. Used for debugging. */
SOBJ scm_segv()
{
  SOBJ x = *((SOBJ*)0);
  return(x);
}


void scm_init_catch()
{
  scm_gc_protect(&scm_catch_list);
  scm_gc_protect(&scm_thrown_tag);
  scm_gc_protect(&scm_thrown_msg);
  scm_add_cprim("throw", 	scm_throw, 	2);
  scm_add_cprim("segv", 	scm_segv, 	0);
}



