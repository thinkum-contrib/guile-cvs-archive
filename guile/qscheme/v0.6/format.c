/* -*- tab-width:4; -*-
 *
 * C like formater
 */
#include "s.h"
#include <stdio.h>
#include <stdarg.h>
#include <avcall.h>

/* asprintf : portable asprintf.
 *
 * NOTE: the allocated size is compatible with the sizes used for the
 * str objects. */
char *scm_asprintf(char *fmt, ...)
{
  va_list ap;
  int size;
  char buf[32], *str;
  
  va_start(ap,fmt);

  size = vsnprintf(buf, sizeof(buf), fmt, ap);
  str  = scm_must_alloc(size+1);
  if (size >= sizeof(buf)) {
	vsnprintf(str, size+1, fmt, ap);
  } else {
	memcpy(str, buf, size);
  }
  return(str);
}

/* like sprintf, but returns a string scheme object  */
SOBJ scm_sprintf(char *fmt, ...)
{
  va_list ap;
  int size;
  char buf[32];
  SOBJ str;
  
  va_start(ap,fmt);

  size = vsnprintf(buf, sizeof(buf), fmt, ap);
  str  = scm_str_alloc(size);
  if (size >= sizeof(buf)) {
	vsnprintf(SCM_STR_VALUE(str), size+1, fmt, ap);
  } else {
	memcpy(SCM_STR_VALUE(str), buf, size);
	SCM_STR_VALUE(str)[size] = 0;
  }
  va_end(ap);
  return(str);
}

/*E* (cformat FMT ARG...) => STR */
SOBJ scm_cformat(int nargs, SOBJ *arg)
{
  SOBJ ret;
  av_alist alist;
  SOBJ obj;
  
  if (nargs < 1)	SCM_ERR("bad number of args", NULL);

  if (!SCM_STRINGP(*arg))	SCM_ERR("bad format", *arg);
  
  av_start_ptr(alist, &scm_sprintf, SOBJ, &ret);
  av_ptr(alist, char *, SCM_STR_VALUE(*arg));   arg++; nargs--;
  while(nargs > 0) {
	obj = *arg++; nargs--;
	switch(SCM_OBJTYPE(obj)) {
	case SOBJ_T_INUM:
	case SOBJ_T_BNUM:	av_long(alist, scm_number2long(obj));	  		break;
	case SOBJ_T_FNUM:	av_double(alist, scm_number2double(obj));  	break;
	case SOBJ_T_CHAR:	av_char(alist, SCM_CHAR(obj));			 		break;
	case SOBJ_T_STRING:	av_ptr(alist, void*, SCM_STR_VALUE(obj));		break;
	case SOBJ_T_SYMBOL:	av_ptr(alist, void*, SCM_SYM_NAME(obj));		break;
	case SOBJ_T_POINTER: av_ptr(alist, void*, SCM_POINTER(obj));		break;
	default:
	  SCM_ERR("cformat: bad object type", obj);
	}
  }
  av_call(alist);
  return(ret);
}

void scm_init_format()
{
  scm_add_cprim("cformat",		scm_cformat,		-1);
}

