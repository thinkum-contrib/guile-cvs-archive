/* -*- tab-width:4; -*- */
/*
 * Code related function
 *
 * $Id$
 */
#include "s.h"
#include "heap.h"


void scm_code_mark(SOBJ obj)
{
  SOBJ *code, *limit;

  code = SCM_CODE_CODE(obj);
  limit = code + SCM_CODE_SIZE(obj);
  while(code < limit) {
	if (scm_is_pointer_to_heap(*code)) {
	  scm_gc_mark(*code);
	}
	code++;
  }
}

void scm_code_sweep(SOBJ obj)
{
  SOBJ *code;

#ifdef DEBUG
  scm_puts("; removing code #");  port_putx(SCM_OUTP, obj);
  scm_puts("\n");
#endif

  if ((code = SCM_CODE_CODE(obj)) != NULL) {
	scm_free(code);
	SCM_CODE_CODE(obj) = NULL;
  }
}

void scm_code_print(SOBJ obj, PORT *p)
{
  if (SCM_CODE_CODE(obj) == NULL) {
	port_puts(p, "#<code null>");
  } else {
	port_puts(p, "#<code ");
	port_putx(p, SCM_CODE_CODE(obj));
	port_puts(p, ">");
  }
}

SCM_STRBUF *scm_code2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  if (SCM_CODE_CODE(obj)) {
	return(scm_strbuf_concat_sprintf(sb, "#<code %p>", SCM_CODE_CODE(obj)));
  }
  return(scm_strbuf_concat_str(sb, "#<code NULL>"));
}

/*-- primitives */
SOBJ scm_mkcode(SOBJ *ptr, long size)
{
  SOBJ new = scm_newcell(SOBJ_T_CODE);
  SCM_CODE_SIZE(new) = size;
  SCM_CODE_CODE(new) = scm_must_alloc(size * sizeof(SOBJ));
  memcpy(SCM_CODE_CODE(new), ptr, size * sizeof(SOBJ));
  return(new);
}

