/* -*- tab-width:4; -*- */
/*
 * Implementation of pointer type
 */
#include "s.h"

void scm_pointer_mark(SOBJ obj)
{
  if (SCM_POINTER_ATTRIB(obj) & SCM_POINTER_FLAG_CELL) {
	scm_gc_mark(SCM_POINTER(obj));
  }
}

void scm_pointer_sweep(SOBJ obj)
{
  if (SCM_POINTER_ATTRIB(obj) & SCM_POINTER_FLAG_ALLOCED) 
	scm_free(SCM_POINTER(obj));
  SCM_POINTER(obj) = NULL;
}

void scm_pointer_print(SOBJ obj, PORT *p)
{
  port_puts(p, "#<pointer ");
  port_putx(p, obj);
  port_putc(p, '>');
}

void scm_pointer_write(SOBJ obj, PORT *p)
{
  port_puts(p, "#<pointer ");
  port_putx(p, obj);
  port_puts(p, " pointing to ");
  port_putx(p, SCM_POINTER(obj));
  port_putc(p, '>');
}

SCM_STRBUF *scm_pointer2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  if (raw)
	return(scm_strbuf_concat_sprintf(sb, "#<pointer %p>", obj));

  return(scm_strbuf_concat_sprintf(sb, "#<pointer %p pointing to %p>", obj, SCM_POINTER(obj)));
}

SOBJ scm_mkpointer(void *p)
{
  SOBJ new = scm_newcell(SOBJ_T_POINTER);
  SCM_POINTER(new) = p;
  return(new);
}

SOBJ scm_pointer_compare(SOBJ p1, SOBJ p2)
{
  return(SCM_MKBOOL( SCM_POINTER(p1) == SCM_POINTER(p2) ));
}

/*E* (pointer? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a pointer, #f otherwise. */
SOBJ scm_pointerp(SOBJ ptr)
{
  return(SCM_POINTERP(ptr) ? scm_true : scm_false);
}

/*E* (null-pointer? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a pointer pointing to NULL, #f otherwise. */
SOBJ scm_null_pointerp(SOBJ ptr)
{
  if (SCM_POINTERP(ptr) && SCM_POINTER(ptr) == NULL)
	return(scm_true);
  return(scm_false);
}

void scm_init_pointer()
{
  scm_add_cprim("pointer?",			scm_pointerp, 		1);
  scm_add_cprim("null-pointer?",	scm_null_pointerp, 	1);
}
