/* -*- tab-width:4; -*- */
/*
 * Char type
 *
 * $Id$
 */
#include "s.h"

#ifdef COMMENT
/*
- les caracteres sont alloues dans un tableau qui n'est jamais collecte.
- on a uniquement des reference sur ce tableau.
*/
#endif

#define BSLASH	'\134'

struct CHR_SYM {
  char *str;
  char chr;
};

struct CHR_SYM csym[] = {
  { "null", 		0 },
  { "bell",			'\a' }, 
  { "backspace",	'\b' }, 
  { "tab",			'\t' }, 
  { "newline",		'\n' }, 
  { "vtab",			'\v' }, 
  { "formfeed",		'\f' }, 
  { "return",		'\r' },
  { "space",		' '  },
  { NULL}
};

Sobject scm_chr_array[256];

int scm_is_pointer_to_chr_array(SOBJ obj)
{
  return(obj >= scm_chr_array && obj < (scm_chr_array + 256));
}

static struct CHR_SYM *lookup_chr(int c)
{
  struct CHR_SYM *s = csym;
  while(s->str) {
	if (c == s->chr)	return(s);
	s++;
  }
  return(NULL);
}

static struct CHR_SYM *lookup_str(char *str)
{
  struct CHR_SYM *s = csym;
  register char *p, *q;
  while(s->str) {
	p = s->str;
	q = str;
	while(*p && *q && *p == tolower(*q)) { p++; q++; }
	if (*p == *q)	return(s);
	s++;
  }
  return(NULL);
}


SOBJ scm_mkchar(int c)
{
  return(scm_chr_array + (c & 0xff));
}

void scm_gc_mark_chars()
{
  int i;
  for (i = 0; i < 256; i++)
	scm_chr_array[i].type = SOBJ_T_CHAR | SCM_GCMARK_MASK;
}

void scm_gc_sweep_chars()
{
  int i;
  for (i = 0; i < 256; i++)
	scm_chr_array[i].type = SOBJ_T_CHAR;
}



void scm_char_print(SOBJ c, PORT *p)
{
  port_putc(p, SCM_CHAR(c));
}

void scm_char_write(SOBJ obj, PORT *p)
{
  int c = SCM_CHAR(obj);
  char buf[8];
  struct CHR_SYM *ps;

  /* \134 is \. #\134 keep cfunctions pacefull */
  port_puts(p, "#\134");
  if (c <= ' ' || c == 127) {
	ps = lookup_chr(c);
	if (ps != NULL) {
	  port_puts(p, ps->str);
	} else {
	  sprintf(buf, "%03o", c & 0xff);  port_puts(p, buf);
	}
  } else {
	port_putc(p, c);
  }
}

SCM_STRBUF *scm_char2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  struct CHR_SYM *ps;
  int c = SCM_CHAR(obj);
  if (raw) 
	return(scm_strbuf_concat_sprintf(sb, "%c", c));

  if (c <= ' ' || c == 127) {
	if ((ps = lookup_chr(c)) != NULL)
	  return(scm_strbuf_concat_sprintf(sb, "#\\%s", ps->str));
	else
	  return(scm_strbuf_concat_sprintf(sb, "#\\%03o", c & 0xff));
  }
  return(scm_strbuf_concat_sprintf(sb, "%c", c));
}


int scm_char_reconize(PORT *port, int  c)
{
  return (c == '#' && port_peekc(port) == BSLASH);
}

SOBJ scm_char_parse(PORT *port, int c)
{
  struct CHR_SYM *ps;
  char str[80], *p;
  
  if (port_getc(port) != BSLASH) {
	fprintf(stderr, "scm_char_parse: internal error\n");
	exit(1);
  }
  c = port_getc(port);
  if (!isalnum(port_peekc(port))) {
	return(scm_mkchar(c));
  }
  str[0] = c; p = str+1;
  while(isalnum(port_peekc(port)) && p < (str + sizeof(str) - 1)) {
	*p++ = port_getc(port);
  }
  *p = 0;
  if (isdigit(*str)) {			/* octal number */
	c = strtol(str, NULL, 8);
  } else {
	ps = lookup_str(str);
	if (ps != NULL) {
	  c = ps->chr;
	} else {
	  /* \134 is \. #\134 keep cfunctions pacefull */
	  scm_puts("#\134");  scm_puts(str);  scm_puts(" is not a char\n");
	  SCM_ERR("illegal char", NULL);
	}
  }
  return(scm_mkchar(c));
}

/*-- library */
/*S* (char? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a character, otherwise returns #f. */

SOBJ scm_charp(SOBJ obj)
{
  return(SCM_MKBOOL(SCM_CHARP(obj)));
}

static int ccmp(SOBJ x, SOBJ y)
{
  if (!SCM_CHARP(x)) 	SCM_ERR("charcmp: bad char", x);
  if (!SCM_CHARP(y)) 	SCM_ERR("charcmp: bad char", y);
  return(SCM_CHAR(x) - SCM_CHAR(y));
}

static int ccmpi(SOBJ x, SOBJ y)
{
  if (!SCM_CHARP(x)) 	SCM_ERR("charcmpi: bad char", x);
  if (!SCM_CHARP(y)) 	SCM_ERR("charcmpi: bad char", y);
  return(tolower(SCM_CHAR(x)) - tolower(SCM_CHAR(y)));
}

#ifdef OLD
#define MKBOOL(x) ((x) ? scm_true: scm_false)
#endif

/*S* (char=? CHAR1 CHAR2) => BOOLEAN */
/*D* Returns #t if CHAR1 equals CHAR2, #f otherwise. */

/*S* (char<? CHAR1 CHAR2) => BOOLEAN */
/*D* Returns #t if CHAR1 less than CHAR2 according to the ASCII char
  set ordering, #f otherwise. */

/*S* (char<=? CHAR1 CHAR2) => BOOLEAN */
/*D* Returns #t if CHAR1 less or equal to CHAR2 according to the ASCII
  char set ordering, #f otherwise. */

/*S* (char>? CHAR1 CHAR2) => BOOLEAN */
/*D* Returns #t if CHAR1 greater than CHAR according to the ASCII char
  set ordering2, #f otherwise. */

/*S* (char>=? CHAR1 CHAR2) => BOOLEAN */
/*D* Returns #t if CHAR1 greater or equal to CHAR2 according to the
  ASCII char set ordering, #f otherwise. */

SOBJ scm_charlt(SOBJ x, SOBJ y) { return(SCM_MKBOOL(ccmp(x,y) <  0)); }
SOBJ scm_charle(SOBJ x, SOBJ y) { return(SCM_MKBOOL(ccmp(x,y) <= 0)); }
SOBJ scm_chareq(SOBJ x, SOBJ y) { return(SCM_MKBOOL(ccmp(x,y) == 0)); }
SOBJ scm_charge(SOBJ x, SOBJ y) { return(SCM_MKBOOL(ccmp(x,y) >= 0)); }
SOBJ scm_chargt(SOBJ x, SOBJ y) { return(SCM_MKBOOL(ccmp(x,y) >  0)); }

/*S* (char-ci=? CHAR1 CHAR2) => BOOLEAN */
/*D* Returns #t if CHAR1 equals CHAR2, #f otherwise. The comparison is
  case insensitive. */

/*S* (char-ci<? CHAR1 CHAR2) => BOOLEAN */
/*D* Returns #t if CHAR1 less than CHAR2 according to the ASCII char
  set ordering, #f otherwise. The comparison is case insensitive. */

/*S* (char-ci<=? CHAR1 CHAR2) => BOOLEAN */
/*D* Returns #t if CHAR1 less or equal to CHAR2 according to the ASCII
  char set ordering, #f otherwise. The comparison is case
  insensitive. */

/*S* (char-ci>? CHAR1 CHAR2) => BOOLEAN */
/*D* Returns #t if CHAR1 greater than CHAR according to the ASCII char
  set ordering2, #f otherwise. The comparison is case insensitive. */

/*S* (char-ci>=? CHAR1 CHAR2) => BOOLEAN */
/*D* Returns #t if CHAR1 greater or equal to CHAR2 according to the
  ASCII char set ordering, #f otherwise. The comparison is case
  insensitive. */

SOBJ scm_charlti(SOBJ x, SOBJ y) { return(SCM_MKBOOL(ccmpi(x,y) <  0)); }
SOBJ scm_charlei(SOBJ x, SOBJ y) { return(SCM_MKBOOL(ccmpi(x,y) <= 0)); }
SOBJ scm_chareqi(SOBJ x, SOBJ y) { return(SCM_MKBOOL(ccmpi(x,y) == 0)); }
SOBJ scm_chargei(SOBJ x, SOBJ y) { return(SCM_MKBOOL(ccmpi(x,y) >= 0)); }
SOBJ scm_chargti(SOBJ x, SOBJ y) { return(SCM_MKBOOL(ccmpi(x,y) >  0)); }

/*S* (char-alphabetic? CHAR) => BOOLEAN */
/*D* Return #t if CHAR is alphabetic, #f otherwise */
SOBJ scm_char_alphap(SOBJ x) 
{
  if (!SCM_CHARP(x)) 	SCM_ERR("char-alphabetic?: bad char", x);
  return(SCM_MKBOOL(isalpha(SCM_CHAR(x))));
}

/*S* (char-numeric? CHAR) => BOOLEAN */
/*D* Return #t if CHAR is numeric, #f otherwise */
SOBJ scm_char_nump(SOBJ x) 
{
  if (!SCM_CHARP(x)) 	SCM_ERR("char-numeric?: bad char", x);
  return(SCM_MKBOOL(isdigit(SCM_CHAR(x))));
}

/*S* (char-whitespace? CHAR) => BOOLEAN */
/*D* Return #t if CHAR is a whitespace, #f otherwise */
SOBJ scm_char_whitep(SOBJ x) 
{
  if (!SCM_CHARP(x)) 	SCM_ERR("char-whitespace?: bad char", x);
  return(SCM_MKBOOL(isspace(SCM_CHAR(x))));
}

/*S* (char-upper-case? CHAR) => BOOLEAN */
/*D* Return #t if CHAR is a upper case, #f otherwise */
SOBJ scm_char_upperp(SOBJ x) 
{
  if (!SCM_CHARP(x)) 	SCM_ERR("char-upper-case?: bad char", x);
  return(SCM_MKBOOL(isupper(SCM_CHAR(x))));
}

/*S* (char-lower-case? CHAR) => BOOLEAN */
/*D* Return #t if CHAR is a lower case, #f otherwise */
SOBJ scm_char_lowerp(SOBJ x) 
{
  if (!SCM_CHARP(x)) 	SCM_ERR("char-lower-case?: bad char", x);
  return(SCM_MKBOOL(islower(SCM_CHAR(x))));
}

/*S* (char->integer CHAR) => INT */
/*D* Returns the ASCII code of the CHAR. */
SOBJ scm_char2int(SOBJ x)
{
  if (!SCM_CHARP(x)) 	SCM_ERR("char->integer: bad char", x);
  return(SCM_MKINUM(SCM_CHAR(x)));
}

/*S* (integer->char INT) => CHAR */
/*D* Returns the character CHAR corresponding to ASCII code INT. */
SOBJ scm_int2char(SOBJ x)
{
  int c = SCM_INUM(x);
  if (c < 0 || c >= 256) 	SCM_ERR("integer->char: bad integer", x);
  return(scm_mkchar(c));
}

/*S* (char-upcase CHAR) => CHAR */
/*D* Return the upper case character of CHAR. */
SOBJ scm_charupc(SOBJ x)
{
  if (!SCM_CHARP(x))	SCM_ERR("char-upcase: bad char", x);
  return(scm_mkchar(toupper(SCM_CHAR(x))));
}

/*S* (char-downcase CHAR) => CHAR */
/*D* Return the lower case character of CHAR. */
SOBJ scm_charlwc(SOBJ x)
{
  if (!SCM_CHARP(x))	SCM_ERR("char-downcase: bad char", x);
  return(scm_mkchar(tolower(SCM_CHAR(x))));
}

void scm_init_chr()
{
  int i;

  for (i = 0; i < 256; i++) {
	scm_chr_array[i].type = SOBJ_T_CHAR;
	SCM_CHAR(scm_chr_array + i) = i;
  }

  scm_add_cprim("char?",		scm_charp, 		1);
  scm_add_cprim("char=?",		scm_chareq, 	2);
  scm_add_cprim("char<?",		scm_charlt, 	2);
  scm_add_cprim("char>?",		scm_chargt, 	2);
  scm_add_cprim("char<=?",		scm_charle, 	2);
  scm_add_cprim("char>=?",		scm_charge, 	2);

  scm_add_cprim("char-ci=?",	scm_chareqi, 	2);
  scm_add_cprim("char-ci<?",	scm_charlti, 	2);
  scm_add_cprim("char-ci>?",	scm_chargti, 	2);
  scm_add_cprim("char-ci<=?",	scm_charlei, 	2);
  scm_add_cprim("char-ci>=?",	scm_chargei, 	2);

  scm_add_cprim("char-alphabetic?",		scm_char_alphap, 	1);
  scm_add_cprim("char-numeric?",		scm_char_nump,		1);
  scm_add_cprim("char-whitespace?",		scm_char_whitep,	1);
  scm_add_cprim("char-upper-case?",		scm_char_upperp,	1);
  scm_add_cprim("char-lower-case?",		scm_char_lowerp,	1);

  scm_add_cprim("char->integer",		scm_char2int,		1);
  scm_add_cprim("integer->char",		scm_int2char,		1);

  scm_add_cprim("char-upcase",		scm_charupc,	1);
  scm_add_cprim("char-downcase",	scm_charlwc,	1);

}

