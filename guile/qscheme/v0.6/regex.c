/* -*- tab-width:4; -*- */
/*
 * Template of a runtime module adding a new type to qscheme.
 *
 * The new type is supposely named regex. Just replace regex by a better name
 *
 */
#include "s.h"
#include "pcre.h"

int SOBJ_T_REGEX;				/* regex type */


/* forward declaration for type descriptor functions */
/*
static void scm_regex_mark(SOBJ obj);
*/
static void scm_regex_sweep(SOBJ obj);
/*
static void scm_regex_print(SOBJ obj, PORT *p);
static void scm_regex_write(SOBJ obj, PORT *p);
*/
static int  scm_regex_creconize(PORT *p, int c);
static SOBJ scm_regex_cparse(PORT *p, int c);
/*
static int  scm_regex_wreconize(PORT *p, char *s);
static SOBJ scm_regex_wparse(PORT *p, char *s);
static SOBJ scm_regex_compare(SOBJ obj1, SOBJ obj2);
*/

/* the type descriptor */

SOBJ_TYPE_DESCR scm_regex_type = {
  0,
  "regex",
  NULL,
  scm_regex_sweep,
  NULL,
  NULL,
  NULL,
  scm_regex_creconize,
  scm_regex_cparse,
  NULL,
  NULL,
  NULL,
};

/* private functions */
static void scm_regex_sweep(SOBJ obj)
{
  if (SCM_AUX(obj) != NULL) free(SCM_AUX(obj));
}

static int  scm_regex_creconize(PORT *p, int c)
{
  return (c == '#' && port_peekc(p) == '/');
}

static SOBJ scm_regex_cparse(PORT *p, int c)
{
  SOBJ str, re;
  char *error;
  int  errindex;
  int  options;

  re = scm_newcell(SOBJ_T_REGEX);
  str = scm_mkstring("");
  c = port_getc(p);				/* ignore leading / */
  /* build a string containing the source re */
  while((c = port_getc(p)) != EOF) {
	if (c == '\\') {
	  scm_string_append_char(str, SCM_MKINUM(c));
	  scm_string_append_char(str, SCM_MKINUM(port_getc(p)));
	  continue;
	}
	if (c == '/') 	break;
	scm_string_append_char(str, SCM_MKINUM(c));
  }
  if (c != '/') SCM_ERR("eof while reading regex...", NULL);

  options = 0;
  while((c = port_peekc(p)) != EOF &&
		strchr("aismxUX", c) != NULL) {
	c = port_getc(p);
	switch(c) {
	case 'a':		options |= PCRE_ANCHORED;	break;
	case 'i':		options |= PCRE_CASELESS;	break;
	case 's':		options |= PCRE_DOTALL;		break;
	case 'm':		options |= PCRE_MULTILINE;	break;
	case 'x':		options |= PCRE_EXTENDED;	break;
	case 'U':		options |= PCRE_UNGREEDY;	break;
	case 'X':		options |= PCRE_EXTRA;		break;
	}
  }
  if (c == EOF) 	SCM_ERR("eof while reading regex...", NULL);

  SCM_AUX(re) = pcre_compile(SCM_STR_VALUE(str), options, 
							 (void*)&error, &errindex,
							 NULL);
  if (SCM_AUX(re) == NULL) SCM_ERR(error, SCM_MKINUM(errindex));
  return(re);
}

/* public functions */

/*E* (regex::isa? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a regular expression object, #f otherwise */
SOBJ scm_regexp(SOBJ f)
{
  return( (SCM_OBJTYPE(f) == SOBJ_T_REGEX) ? scm_true : scm_false );
}

static int re_match_substr[256];
static int re_match_count;
static SOBJ re_source;			/* source string */

/*E* (regex::match RE STR) => N | #f */
/*D* Try to match string STR with regular expression RE. Returns the
  number of matches found or #f if no match occured. */
  
SOBJ scm_re_match(SOBJ re, SOBJ str)
{
  int r;
  
  if (scm_regexp(re) == scm_false)	SCM_ERR("bad regexp", re);
  if (!SCM_STRINGP(str)) 			SCM_ERR("bad string", str);

  r = pcre_exec(SCM_AUX(re), NULL,
				SCM_STR_VALUE(str), SCM_STR_LEN(str), 0, 0,
				re_match_substr, 256);

  
  if (r <= 0) {
 	re_match_count = 0;
	return(scm_false);
  }
  re_source = str;
  re_match_count = r;
  return(SCM_MKINUM(r));
}

/*E* (regex::sub N) => STR | #f */
/*D* Returns the Nth submatch of last regex::match call or #f if N is
  out of range */
SOBJ scm_re_sub(SOBJ n)
{
  int i,j,len;
  
  if (!SCM_INUMP(n)) 	SCM_ERR("bad number", n);

  i = SCM_INUM(n);
  if (i < re_match_count) {
	i *= 2; j = i+1;  len = re_match_substr[j] - re_match_substr[i];
	return( scm_substring(re_source,
						  SCM_MKINUM(re_match_substr[i]),
						  SCM_MKINUM(re_match_substr[j])));
  }
  return(scm_false);
}

/*E* (regex::$0) => STR | #f */
/*D* Returns submatch 0 of previous regex::match */

/*E* (regex::$1) => STR | #f */
/*D* Returns submatch 1 of previous regex::match */

/*E* (regex::$2) => STR | #f */
/*D* Returns submatch 2 of previous regex::match */

/*E* (regex::$3) => STR | #f */
/*D* Returns submatch 3 of previous regex::match */

/*E* (regex::$4) => STR | #f */
/*D* Returns submatch 4 of previous regex::match */

/*E* (regex::$5) => STR | #f */
/*D* Returns submatch 5 of previous regex::match */

/*E* (regex::$6) => STR | #f */
/*D* Returns submatch 6 of previous regex::match */

/*E* (regex::$7) => STR | #f */
/*D* Returns submatch 7 of previous regex::match */

/*E* (regex::$8) => STR | #f */
/*D* Returns submatch 8 of previous regex::match */

/*E* (regex::$9) => STR | #f */
/*D* Returns submatch 9 of previous regex::match */

SOBJ scm_re_dollar_0() { return(scm_re_sub(SCM_MKINUM(0))); }
SOBJ scm_re_dollar_1() { return(scm_re_sub(SCM_MKINUM(1))); }
SOBJ scm_re_dollar_2() { return(scm_re_sub(SCM_MKINUM(2))); }
SOBJ scm_re_dollar_3() { return(scm_re_sub(SCM_MKINUM(3))); }
SOBJ scm_re_dollar_4() { return(scm_re_sub(SCM_MKINUM(4))); }
SOBJ scm_re_dollar_5() { return(scm_re_sub(SCM_MKINUM(5))); }
SOBJ scm_re_dollar_6() { return(scm_re_sub(SCM_MKINUM(6))); }
SOBJ scm_re_dollar_7() { return(scm_re_sub(SCM_MKINUM(7))); }
SOBJ scm_re_dollar_8() { return(scm_re_sub(SCM_MKINUM(8))); }
SOBJ scm_re_dollar_9() { return(scm_re_sub(SCM_MKINUM(9))); }


void scm_init_regex()
{
  SOBJ cmod;
  SOBJ_T_REGEX = scm_add_type(&scm_regex_type);

  scm_gc_protect(&re_source);

  cmod = scm_get_current_module();
  scm_set_current_module(scm_make_module(scm_mkatom("regex")));

  scm_add_cprim("isa?",			scm_regexp,		1);
  scm_add_cprim("match",		scm_re_match,	2);
  scm_add_cprim("sub",			scm_re_sub,		1);
  scm_add_cprim("$0",			scm_re_dollar_0, 	0);
  scm_add_cprim("$1",			scm_re_dollar_1, 	0);
  scm_add_cprim("$2",			scm_re_dollar_2, 	0);
  scm_add_cprim("$3",			scm_re_dollar_3, 	0);
  scm_add_cprim("$4",			scm_re_dollar_4, 	0);
  scm_add_cprim("$5",			scm_re_dollar_5, 	0);
  scm_add_cprim("$6",			scm_re_dollar_6, 	0);
  scm_add_cprim("$7",			scm_re_dollar_7, 	0);
  scm_add_cprim("$8",			scm_re_dollar_8, 	0);
  scm_add_cprim("$9",			scm_re_dollar_9, 	0);

  /* fill the rest here */

  scm_set_current_module(cmod);
}
