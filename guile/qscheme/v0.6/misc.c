/* -*- tab-width:4; -*- 
 *
 * * You will find here everything I don't know where to store.
 *
 */
#include "s.h"

/*E* (gc) => #undefined */
/*D* Fires up a gc right now. */
static SOBJ do_gc()
{
  scm_gc();
  return(scm_undefined);
}

/*E* (gc-stat) => #undefined */
/*D* Display current GC statistics */
static SOBJ do_gc_stat()
{
  scm_heap_stat();
  return(scm_undefined);
}

/*E* (gc-verbosity NUMBER) => #undefined */
/*D* Change garbage collector verbosity. 0 => quiet, 1 => min, 2 => max */
static SOBJ gc_verbosity(SOBJ n)
{
  if (!SCM_INUMP(n))	SCM_ERR("bad number", n);
  scm_gc_verbose = SCM_INUM(n);
  return(scm_undefined);
}


/*-- try to convert to string */
char *scm_getstr(SOBJ obj)
{
  if (SCM_STRINGP(obj)) 	return(SCM_STR_VALUE(obj));
  if (SCM_ATOMP(obj))		return(SCM_ATOM_NAME(obj));
  if (SCM_KEYWORDP(obj))	return(SCM_ATOM_NAME(SCM_KEYW_NAME(obj)));
  if (SCM_SYMBOLP(obj))		return(SCM_ATOM_NAME(SCM_SYM_NAME(obj)));
  return(NULL);
}

/*E* (whatis STR) => NUMBER */
/*D* Prints all description of STR found in the whatis data
  base. Returns the number of element found. If the last character of
  the string STR is '~', the description of all words starting with
  STR will be displayed. */
/*X* (whatis "whatis") => display the description of the whatis
  function */
SOBJ scm_whatis(SOBJ obj)
{
  char *str, buf[128], fname[PATH_MAX];
  FILE *fd;
  int len;
  int nfound = 0;
  int wildmatch;
  
  if (!SCM_ANYSTRP(obj)) 	SCM_ERR("bad string", obj);
  str = scm_getstr(obj);
  len = strlen(str);

  if ((fd = fopen("whatis.qs", "r")) == NULL) {
	if (getenv("QS_LIB")) {
	  sprintf(fname, "%s/whatis.qs", getenv("QS_LIB"));
	  if ((fd = fopen(fname, "r")) == NULL) 
		SCM_ERR("cannot open the whatis.qs file", NULL);
	} else {
	  SCM_ERR("whatis.qs not found and QS_LIB not defined", NULL);
	}
  }

  if (len != 0 && str[len-1] == '~') {
	wildmatch = 1;
	len--;
  } else {
	wildmatch = 0;
  }

  while(fgets(buf, sizeof(buf), fd) != NULL) {
  restart:
	if (*buf != '\(' && !isalpha(*buf)) 	continue;
	if (*buf == '\(') {			/* func def */
	  if (strncmp(buf+1, str, len) != 0)		continue;
	  if (!wildmatch && strchr(" \t)",buf[len+1]) == NULL) continue;
	} else {
	  if (strncmp(buf, str, len) != 0) 			continue;
	  if (!wildmatch && !isspace(buf[len]))		continue;
	}
	nfound++;
	scm_puts(buf);
	while(fgets(buf, sizeof(buf), fd) != NULL) {
	  if (!isspace(*buf))	goto restart;
	  scm_puts(buf);
	}
  }
  fclose(fd);
  return(SCM_MKINUM(nfound));
}

/*E* (set-prompt STR) => #undef */
/*D* set a new prompt */
SOBJ scm_set_prompt(SOBJ x)
{
  if (!SCM_STRINGP(x))	SCM_ERR("bad string",x);
  if (scm_prompt_str) scm_free(scm_prompt_str);
  scm_prompt_str = scm_must_strdup(SCM_STR_VALUE(x));
  return(scm_undefined);
}

/*E* (version) => STRING */
/*D* Return current version of qscheme as a string */
SOBJ scm_version()
{
  return(scm_mkstring(VERSION));
}

void scm_init_misc()
{
  scm_add_cprim("gc", 			do_gc, 			0);
  scm_add_cprim("gc-stat", 		do_gc_stat, 	0);
  scm_add_cprim("gc-verbosity",	gc_verbosity,	1);
  scm_add_cprim("set-prompt",	scm_set_prompt,	1);
  scm_add_cprim("whatis",		scm_whatis,		1);
  scm_add_cprim("version",		scm_version, 	0);
}
