/* -*- tab-width:4; -*- */
/*
 * The strings
 *
 * $Id$
 */
#include "s.h"
#include "heap.h"
#include <stdarg.h>

/****************************************************************
 * STRBUF utilities
 ****************************************************************/

/* create a new strbuf with enough space to store size+1 chars  */
SCM_STRBUF *scm_strbuf_new(int size)
{
  SCM_STRBUF *new;
  int n, wanted = size + 1;

  n = SCM_STRBUF_MIN_SIZE;
  while (n < wanted) n *= 2;

  new = scm_must_alloc(sizeof(SCM_STRBUF) + n);
#ifdef SCM_STRBUF_DEBUG
  fprintf(stderr, "*** strbuf alloc: %d\n", n);
#endif
  memset(new, 0, sizeof(SCM_STRBUF));
#ifdef SCM_STRBUF_PROFILE
  new->nalloc = 1;
#endif
  new->max = n;
  return(new);
}

SCM_STRBUF *scm_strbuf_resize(SCM_STRBUF *sb, int newlen)
{
  int n;
  int wanted;
  if (newlen < 0)	newlen = 0;

  wanted = newlen + 1;
  n = (sb->max) ? sb->max : SCM_STRBUF_MIN_SIZE;
  while(n < wanted) n *= 2;

  if (n != sb->max) {
#ifdef SCM_STRBUF_DEBUG
	fprintf(stderr, "*** strbuf realloc: wanted=%d realloc=%d\n", newlen, n);
#endif
	sb = scm_must_realloc(sb, n + sizeof(SCM_STRBUF));
#ifdef SCM_STRBUF_PROFILE
	sb->nrealloc++;		sb->bmove += (sb->max + sizeof(SCM_STRBUF));
#endif
  }
  sb->max = n;
  return(sb);
}

SCM_STRBUF *scm_strbuf_clear(SCM_STRBUF *sb)
{
  return(scm_strbuf_resize(sb, 0));
}

SCM_STRBUF *scm_strbuf_set(SCM_STRBUF *sb, char *str)
{
  int len = strlen(str);
  sb = scm_strbuf_resize(sb, len);
  strcpy(sb->str, str);
  sb->len = len;
  return(sb);
}

SCM_STRBUF *scm_strbuf_concat_buf(SCM_STRBUF *sb, char *buf, int len)
{
  sb = scm_strbuf_resize(sb, sb->len + len);
  strncpy(sb->str + sb->len, buf, len);
  sb->len += len;
  return(sb);
}

SCM_STRBUF *scm_strbuf_concat_str(SCM_STRBUF *sb, char *buf)
{
  int len = strlen(buf);
  sb = scm_strbuf_resize(sb, sb->len + len);
  strncpy(sb->str + sb->len, buf, len);
  sb->len += len;
  return(sb);
}

SCM_STRBUF *scm_strbuf_concat_chr(SCM_STRBUF *sb, char c)
{
  char *p;

  sb = scm_strbuf_resize(sb, sb->len + 1);
  p = sb->str + sb->len;
  *p++ = c; *p = 0;
  sb->len++;
  return(sb);
}



SCM_STRBUF *scm_strbuf_sprintf(SCM_STRBUF *sb, char *fmt, ...)
{
  va_list ap;
  int size;

  va_start(ap, fmt);
  sb = scm_strbuf_resize(sb, 0);
  size = vsnprintf(sb->str, SCM_STRBUF_MIN_SIZE, fmt, ap);
  if (size >= sb->max) {
	sb = scm_strbuf_resize(sb, size);
	size = vsnprintf(sb->str, sb->max, fmt, ap);
  }
  sb->len = size;
  return(sb);
}

SCM_STRBUF *scm_strbuf_concat_sprintf(SCM_STRBUF *sb, char *fmt, ...)
{
  va_list ap;
  int size;

  va_start(ap, fmt);
  size = vsnprintf(sb->str + sb->len, sb->max - sb->len, fmt, ap);
  if (sb->len + size >= sb->max) {
	sb = scm_strbuf_resize(sb, sb->len + size);
	size = vsnprintf(sb->str + sb->len, sb->max - sb->len, fmt, ap);
  }
  sb->len += size;
  return(sb);
}

SCM_STRBUF *scm_strbuf_print(SCM_STRBUF *sb)
{
  fwrite(sb->str, 1, sb->len, stdout);
  return(sb);
}

SCM_STRBUF *scm_strbuf_fprint(SCM_STRBUF *sb, FILE *fd)
{
  fwrite(sb->str, 1, sb->len, fd);
  return(sb);
}

void scm_strbuf_stats(SCM_STRBUF *sb)
{
#ifdef SCM_STRBUF_PROFILE
  printf("Alloc %d times, realloc %d times, %d bytes moved\n",
		 sb->nalloc,
		 sb->nrealloc,
		 sb->bmove);
#else
  printf("strbuf: no profile information\n");
#endif
}

/****************************************************************
 * Scheme strings
 ****************************************************************/

/*** create a new string of len char */
SOBJ scm_str_alloc(int len)
{
  SOBJ str = scm_newcell(SOBJ_T_STRING);
  SCM_STR(str) = scm_strbuf_new(len);
  SCM_STR_LEN(str) = len;
  return(str);
}

/*** resize str to newlen chars */
SOBJ scm_str_resize(SOBJ str, int newlen)
{
  SCM_STR(str) = scm_strbuf_resize(SCM_STR(str), newlen);
  SCM_STR_LEN(str) = newlen;
  return(str);
#ifdef OLD
  int qlen, nlen;

  if (SCM_STR_VALUE(str)) {		/* already allocated */
	qlen = scm_str_lenq(SCM_STR_LEN(str));
	nlen = scm_str_lenq(newlen+1);
	if (qlen != nlen) 
	  SCM_STR_VALUE(str) = scm_must_realloc(SCM_STR_VALUE(str), nlen);

  } else {						/* fresh string */
	nlen = scm_str_lenq(newlen+1);
	SCM_STR_VALUE(str) = scm_must_alloc(nlen);
  }
  SCM_STR_LEN(str) = newlen;
  return(str);
#endif
}

/*** make a string: if str == NULL, create a new one else clone str */
SOBJ scm_mkstring(char *str)
{
  SOBJ obj;
  int len;

  if (str) {
	len = strlen(str);
	obj = scm_str_alloc(len);
	strcpy(SCM_STR_VALUE(obj), str);
  } else {
	obj = scm_str_alloc(0);
  }
  return(obj);
}

/*** return the string length or -1 on error */
int scm_strlen(SOBJ str)
{
  if (!SCM_STRINGP(str)) return(-1);
  return(SCM_STR_LEN(str));
}

/*** append a char to string */
SOBJ scm_strcat_chr(SOBJ str, int c)
{
  int l = SCM_STR_LEN(str);
  scm_str_resize(str, l + 1);
  SCM_STR_VALUE(str)[l] = c;
  SCM_STR_VALUE(str)[l+1] = 0;
  return(str);
}

/*** append content of a buffer to string */
SOBJ scm_strcat_buf(SOBJ str, char *buf, int len)
{
  int l = SCM_STR_LEN(str);

  scm_str_resize(str, l + len);
  memcpy(SCM_STR_VALUE(str) + l, buf, len);
  return(str);
}

/*E* (string-append-char STRING CHAR) => STRING */
/*D* Returns a new STRING with CHAR appended. */

SOBJ scm_string_append_char(SOBJ str, SOBJ chr)
{
  if (str == NULL) 	str = scm_mkstring(NULL);
  
  if (!SCM_STRINGP(str)) 	SCM_ERR("want a string", str);
  if (!SCM_INUMP(chr) && !SCM_CHARP(chr)) SCM_ERR("bad int or chr", chr);
  return(scm_strcat_chr(str, SCM_INUMP(chr) ? SCM_INUM(chr) : SCM_CHAR(chr)));
}

/************************************************************************
 * String type definition
 ************************************************************************/

void scm_string_sweep(SOBJ str)
{
  if (str) scm_free(SCM_STR(str));

#ifdef OLD
#ifdef DEBUG
  scm_puts("; removing string at ");  port_putx(SCM_OUTP, str); scm_puts("\n");
#endif
  if (str && SCM_STR_VALUE(str)) {
	scm_free(SCM_STR_VALUE(str));
	SCM_STR_VALUE(str) = NULL;
  }
#endif
}

void scm_string_print(SOBJ str, PORT *p)
{
  port_write(p, str, SCM_STR_LEN(str));
}

void scm_string_write(SOBJ str, PORT *p)
{
  int c, i;
  char buf[32];

  port_putc(p, '\"');
  for (i = 0; i < SCM_STR_LEN(str); i++) {
	c = SCM_STR_VALUE(str)[i] & 0xff;
	if (c < ' ' || c >= 127) {
	  switch(c) {
	  case '\a': port_puts(p, "\\a");	break;
	  case '\b': port_puts(p, "\\b");	break;
	  case '\t': port_puts(p, "\\t");	break;
	  case '\n': port_puts(p, "\\n");	break;
	  case '\v': port_puts(p, "\\v");	break;
	  case '\f': port_puts(p, "\\f");	break;
	  case '\r': port_puts(p, "\\r");	break;
	  default:
		sprintf(buf, "\\%03u", c);
		port_puts(p, buf);
	  }
	} else {
	  switch(c) {
	  case '\\': port_puts(p, "\\\\"); 	break;
	  case '\"': port_puts(p, "\\\"");	break;
	  default:   port_putc(p, c);
	  }
	}
  }
  port_putc(p, '\"');
}

SCM_STRBUF *scm_string2str(SCM_STRBUF *sb, SOBJ str, int raw)
{
  int c, i, len;
  char *p;

  if (raw) 
	return(scm_strbuf_concat_str(sb, SCM_STR_VALUE(str)));

  sb = scm_strbuf_concat_chr(sb, '\"');
  p = SCM_STR_VALUE(str);
  len = SCM_STR_LEN(str);
  for (i = 0; i < len; i++) {
	c = p[i] & 0xff;
	if (c < ' ' || c >= 127) {
	  switch(c) {
	  case '\a': sb = scm_strbuf_concat_str(sb, "\\a");	break;
	  case '\b': sb = scm_strbuf_concat_str(sb, "\\b");	break;
	  case '\t': sb = scm_strbuf_concat_str(sb, "\\t");	break;
	  case '\n': sb = scm_strbuf_concat_str(sb, "\\n");	break;
	  case '\v': sb = scm_strbuf_concat_str(sb, "\\v");	break;
	  case '\f': sb = scm_strbuf_concat_str(sb, "\\f");	break;
	  case '\r': sb = scm_strbuf_concat_str(sb, "\\r");	break;
	  default:
		sb = scm_strbuf_concat_sprintf(sb, "\\%03u", c);
	  }
	} else {
	  switch(c) {
	  case '\\': sb = scm_strbuf_concat_str(sb, "\\\\"); 	break;
	  case '\"': sb = scm_strbuf_concat_str(sb, "\\\"");	break;
	  default:   sb = scm_strbuf_concat_chr(sb, c);
	  }
	}
  }
  sb = scm_strbuf_concat_chr(sb, '\"');
  return(sb);
}

/*-- detect valid string starter char */
int scm_string_reconize(PORT *port, int c)
{
  return(c == '\"');
}

static int xdigit2nibble(int x)
{
  int n = (x <= '9') ? (x - '0') : (tolower(x) - ('a' - 10));
  return(n);
}

static int nibble2xdigit(int x)
{
  static char cv[] = "0123456789ABCDEF";
  return(cv[x & 0x0f]);
}

/*-- read from SCM_INP */
SOBJ scm_string_parse(PORT *port, int start_char)
{
  int c, n, i;
  SOBJ str = scm_mkstring("");

  while( (c = port_getc(port)) != EOF) {
	if (c == '\"') 	break;

	if (c == '\\')	{			/* escape sequence */
	  if ((c = port_getc(port)) == EOF) {
		SCM_ERR("EOF while reading string...", NULL);
	  }
	  c &= 0xff;
	  switch(c) {
	  case 'n':		c = '\n';		break;
	  case 'r':		c = '\r';		break;
	  case 'b':		c = '\b';		break;
	  case 't':		c = '\t';		break;
	  case 'x':
		n = 0;
		for (i = 0; i < 2 && isxdigit(port_peekc(port)); i++) {
		  c = port_getc(port) & 0xff;
		  n = (n << 4) | xdigit2nibble(c);
		}
		c = n;
		break;
	  default:
		n = 0;
		if (isdigit(c) && c < '8') {
		  n = c - '0';
		  for (i = 1; i < 3 && isdigit(port_peekc(port)); i++) {
			c = port_getc(port) & 0xff;
			n = (n << 3) | (c - '0');
		  }
		}
		c = n;
		
	  /* nothing yet */
	  }
	}
	str = scm_strcat_chr(str, c);
  }
  return(str);
}

/*-- library */

SOBJ scm_string_compare(SOBJ s1, SOBJ s2)
{
  return( (strcmp(SCM_STR_VALUE(s1), SCM_STR_VALUE(s2)) == 0) ?
		  scm_true : scm_false);
}

/*S* (string? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a string, #f otherwise */
SOBJ scm_stringp(SOBJ x)
{
  return(SCM_MKBOOL(SCM_STRINGP(x)));
}

SOBJ scm_make_string2(SOBJ l, SOBJ c)
{
  SOBJ s;
  int len, i, j;
  
  if (!SCM_INUMP(l))				SCM_ERR("make-string: bad integer", l);
  if (c != NULL && !SCM_CHARP(c)) 	SCM_ERR("make-string: bad char", c);

  len = SCM_INUM(l);
  s = scm_str_alloc(len);
  j = (c == NULL) ? 0 : SCM_CHAR(c);
  for (i = 0; i < len; i++)  SCM_STR_VALUE(s)[i] = j;
  SCM_STR_VALUE(s)[len] = 0;	/* null terminated for c compatibility */
  return(s);
}

/*S* (make-string LEN [CHAR]) => STRING */
/*D* Returns a newly allocated string of length LEN. If CHAR is given,
  then all elements of the string are initialized to CHAR, otherwise
  the contents of the string are unspecified. */
SOBJ scm_make_string(int nargs, SOBJ *a)
{
  if (nargs < 1 || nargs > 2)	SCM_ERR("make-string: bad number of args", NULL);

  return(scm_make_string2(a[0], (nargs == 2) ? a[1] : NULL));
}

/*S* (string CHAR ...) => STRING */
/*D* Returns a newly allocated string composed of the arguments. NOTE:
 * CHAR may also be a number representing the ASCII code of the
 * char. */
SOBJ scm_string(int nargs, SOBJ *a)
{
  int i;
  SOBJ str = scm_str_alloc(nargs);

  for (i = 0; i < nargs; i++) {
	if (SCM_INUMP(a[i])) {
	  SCM_STR_VALUE(str)[i] = SCM_INUM(a[i]);
	  continue;
	}
	if (SCM_CHARP(a[i])) {
	  SCM_STR_VALUE(str)[i] = SCM_CHAR(a[i]);
	  continue;
	}
	if (SCM_STRINGP(a[i])) {
	  SCM_STR_VALUE(str)[i] = SCM_STR_VALUE(a[i])[0];
	  continue;
	}
	SCM_ERR("bad char", a[i]);
  }
  return(str);
}

/*S* (string-length STRING) => NUMBER */
/*D* Returns the number of characters in the given string. */
SOBJ scm_string_length(SOBJ str)
{
  if (!SCM_STRINGP(str)) 	SCM_ERR("string-length: bad string", str);
  return(SCM_MKINUM(SCM_STR_LEN(str)));
}

/*S* (string-ref STRING K) => CHAR */
/*D* Returns character K of STRING using zero-origin indexing. */
SOBJ scm_string_ref(SOBJ str, SOBJ index)
{
  int i;
  
  if (!SCM_STRINGP(str))	SCM_ERR("string-ref: bad string", str);
  if (!SCM_INUMP(index))	SCM_ERR("string-ref: bad index", index);

  i = SCM_INUM(index);
  if (i < 0 || i >= SCM_STR_LEN(str))
	SCM_ERR("string-ref: index out of range", index);

  return(scm_mkchar(SCM_STR_VALUE(str)[i]));
}

/*S* (string-set! STRING K CHAR) => #undefined */
/*D* Stores CHAR in element K of STRING and returns an unspecified
  value. */
SOBJ scm_string_set(SOBJ str, SOBJ index, SOBJ chr)
{
  int i;

  if (!SCM_STRINGP(str))	SCM_ERR("string-set!: bad string", str);
  if (!SCM_INUMP(index))	SCM_ERR("string-set!: bad index", index);
  if (!SCM_CHARP(chr))		SCM_ERR("string-set!: bad char", chr);
  
  i = SCM_INUM(index);
  if (i < 0 || i >= SCM_STR_LEN(str))
	SCM_ERR("string-set!: index out of range", index);

  SCM_STR_VALUE(str)[i] = SCM_CHAR(chr);

  return(scm_undefined);
}

static int scmp(SOBJ s1, SOBJ s2)
{
  char *p, *q, *lp, *lq;

  if (!SCM_STRINGP(s1)) 	SCM_ERR("string-cmp: bad string", s1);
  if (!SCM_STRINGP(s2)) 	SCM_ERR("string-cmp: bad string", s2);

  p = SCM_STR_VALUE(s1);  lp = p + SCM_STR_LEN(s1);
  q = SCM_STR_VALUE(s2);  lq = q + SCM_STR_LEN(s2);
  while(p < lp && q < lq) {
	if (*p - *q != 0) return(*p - *q);
	p++;
	q++;
  }
  if (p == lp && q == lq) 	return(0);
  return(*p - *q);
}

static int scmpi(SOBJ s1, SOBJ s2)
{
  char *p, *q, *lp, *lq;

  if (!SCM_STRINGP(s1)) 	SCM_ERR("string-cmp: bad string", s1);
  if (!SCM_STRINGP(s2)) 	SCM_ERR("string-cmp: bad string", s2);

  p = SCM_STR_VALUE(s1);  lp = p + SCM_STR_LEN(s1);
  q = SCM_STR_VALUE(s2);  lq = q + SCM_STR_LEN(s2);
  while(p < lp && q < lq) {
	if (tolower(*p) - tolower(*q) != 0) return(tolower(*p) - tolower(*q));
	p++;
	q++;
  }
  if (p == lp && q == lq) 	return(0);
  return(tolower(*p) - tolower(*q));
}

/*S* (string<? STRING1 STRING2) => BOOLEAN */
/*D* Returns #t if STRING1 is lexicographically less than STRING2, #f
  otherwise. */
SOBJ scm_string_lt(SOBJ s1, SOBJ s2) { return(SCM_MKBOOL(scmp(s1,s2) <  0)); }

/*S* (string<=? STRING1 STRING2) => BOOLEAN */
/*D* Returns #t if STRING1 is lexicographically less or equal than
  STRING2, #f otherwise. */
SOBJ scm_string_le(SOBJ s1, SOBJ s2) { return(SCM_MKBOOL(scmp(s1,s2) <= 0)); }

/*S* (string=? STRING1 STRING2) => BOOLEAN */
/*D* Returns #t if STRING1 is the same as STRING2, #f otherwise. */
SOBJ scm_string_eq(SOBJ s1, SOBJ s2) { return(SCM_MKBOOL(scmp(s1,s2) == 0)); }

/*S* (string>? STRING1 STRING2) => BOOLEAN */
/*D* Returns #t if STRING1 is lexicographically greater than STRING2,
  #f otherwise. */
SOBJ scm_string_ge(SOBJ s1, SOBJ s2) { return(SCM_MKBOOL(scmp(s1,s2) >= 0)); }

/*S* (string>=? STRING1 STRING2) => BOOLEAN */
/*D* Returns #t if STRING1 is lexicographically greater or equal than
  STRING2, #f otherwise. */
SOBJ scm_string_gt(SOBJ s1, SOBJ s2) { return(SCM_MKBOOL(scmp(s1,s2) >  0)); }

/*S* (string-ci<? STRING1 STRING2) => BOOLEAN */
/*D* Returns #t if STRING1 is lexicographically less than STRING2, #f
  otherwise. The comparison is case insensitive. */
SOBJ scm_string_ci_lt(SOBJ s1, SOBJ s2){return(SCM_MKBOOL(scmpi(s1,s2) <  0));}

/*S* (string-ci<=? STRING1 STRING2) => BOOLEAN */
/*D* Returns #t if STRING1 is lexicographically less or equal than
  STRING2, #f otherwise. The comparison is case insensitive. */
SOBJ scm_string_ci_le(SOBJ s1, SOBJ s2){return(SCM_MKBOOL(scmpi(s1,s2) <= 0));}

/*S* (string-ci=? STRING1 STRING2) => BOOLEAN */
/*D* Returns #t if STRING1 is the same as STRING2, #f otherwise. The
  comparison is case insensitive. */
SOBJ scm_string_ci_eq(SOBJ s1, SOBJ s2){return(SCM_MKBOOL(scmpi(s1,s2) == 0));}

/*S* (string-ci>? STRING1 STRING2) => BOOLEAN */
/*D* Returns #t if STRING1 is lexicographically greater than STRING2,
  #f otherwise. The comparison is case insensitive. */
SOBJ scm_string_ci_ge(SOBJ s1, SOBJ s2){return(SCM_MKBOOL(scmpi(s1,s2) >= 0));}

/*S* (string-ci>=? STRING1 STRING2) => BOOLEAN */
/*D* Returns #t if STRING1 is lexicographically greater or equal than
  STRING2, #f otherwise. The comparison is case insensitive. */
SOBJ scm_string_ci_gt(SOBJ s1, SOBJ s2){return(SCM_MKBOOL(scmpi(s1,s2) >  0));}

/*S* (substring START END) => STRING */
/*D* Returns a newly allocated string formed from the characters of
  string beginning with index start (inclusive) and ending with index
  end (exclusive). */
SOBJ scm_substring(SOBJ string, SOBJ start, SOBJ end)
{
  SOBJ s;
  int len;
  
  if (!SCM_STRINGP(string)) 	SCM_ERR("substring: bad string", string);
  if (!SCM_INUMP(start))		SCM_ERR("substring: bad start", start);
  if (!SCM_INUMP(end))			SCM_ERR("substring: bad end", start);
  
  len = SCM_INUM(end) - SCM_INUM(start);
  if (len < 0) 					SCM_ERR("substring: start is not <= end", start);

  s = scm_str_alloc(len);
  strncpy(SCM_STR_VALUE(s),
		  SCM_STR_VALUE(string) + SCM_INUM(start),
		  len);
  SCM_STR_VALUE(s)[len] = 0;
  return(s);
}

/*S* (string-append STRING ...) => STRING */
/*D* Returns a newly allocated string whose characters form the
  concatenation of the given strings. */
SOBJ scm_string_append(int nargs, SOBJ *s)
{
  int i, len;
  SOBJ str;
  char *p;
  
  len = 0;
  for (i = 0; i < nargs; i++) {
	if (!SCM_STRINGP(s[i])) 	SCM_ERR("string-append: bad string", s[i]);
	len += SCM_STR_LEN(s[i]);
  }
  str = scm_str_alloc(len);
  p = SCM_STR_VALUE(str);
  for (i = 0; i < nargs; i++) {
	memcpy(p, SCM_STR_VALUE(s[i]), SCM_STR_LEN(s[i]));
	p += SCM_STR_LEN(s[i]);
  }
  return(str);
}

/*S* (string-append2 STR1 STR2) => STRING */
/*D* Returns a newly allocated string whose characters form the
  concatenation of STR1 and STR2 */
SOBJ scm_string_append2(SOBJ str1, SOBJ str2)
{
  SOBJ str;
  if (!SCM_STRINGP(str1)) SCM_ERR("string-append2: bad string", str1);
  if (!SCM_STRINGP(str2)) SCM_ERR("string-append2: bad string", str2);

  str = scm_str_alloc(SCM_STR_LEN(str1) + SCM_STR_LEN(str2));
  strcpy(SCM_STR_VALUE(str), SCM_STR_VALUE(str1));
  strcpy(SCM_STR_VALUE(str) + SCM_STR_LEN(str1), SCM_STR_VALUE(str2));
  return(str);
}

/*E* (string-append! STR1 STR2) => #undefined */
/*D* Append STR2 to STR1. */
SOBJ scm_string_concat(SOBJ str1, SOBJ str2)
{
  int l1, l2;
  if (!SCM_STRINGP(str1)) 	SCM_ERR("string-concat!: bad string", str1);
  if (!SCM_STRINGP(str2)) 	SCM_ERR("string-concat!: bad string", str2);

  l1 = SCM_STR_LEN(str1);
  l2 = SCM_STR_LEN(str2);
  scm_str_resize(str1, l1 + l2);
  memcpy(SCM_STR_VALUE(str1) + l1, SCM_STR_VALUE(str2), l2);
  SCM_STR_VALUE(str1)[l1+l2]=0;
  return(scm_undefined);
}

/*-- string coerse */

/*S* (string->list STRING) => LIST */
/*D* Returns a newly allocated list of the characters that make
  up the given STRING. */
SOBJ scm_string_to_list(SOBJ str)
{
  SOBJ list;
  int i;
  
  if (!SCM_STRINGP(str))	SCM_ERR("string->list: bad string", str);

  list = NULL;
  for (i = SCM_STR_LEN(str)-1; i >= 0; --i) {
	list = scm_cons(scm_mkchar(SCM_STR_VALUE(str)[i]), list);
  }
  return(list);
}

/*S* (string->symbol STRING) => SYMBOL */
/*D* Returns the symbol whose name is string. */
SOBJ scm_string_to_symbol(SOBJ str)
{
  if (!SCM_STRINGP(str))	SCM_ERR("string->symbol: bad string", str);
  return(scm_mksymbol(SCM_STR_VALUE(str)));
}

/*S* (symbol->string SYMBOL) => STRING */
/*D* Returns the name of symbol as a string. */
SOBJ scm_symbol_to_string(SOBJ sym)
{
  switch(SCM_OBJTYPE(sym)) {
  case SOBJ_T_SYMBOL:  return(scm_atom_to_string(SCM_SYM_NAME(sym)));
  case SOBJ_T_ATOM:	   return(scm_atom_to_string(sym));
  }
  SCM_ERR("symbol->string: bad symbol", sym);
  return(scm_undefined);
}

/*S* (list->string LIST) => STRING */
/*D* Returns a newly allocated STRING formed from the characters in
  the list LIST, which must be a list of characters.*/
SOBJ scm_list_to_string(SOBJ list)
{
  SOBJ str;
  int len;
  char *p;

  len = scm_list_length(list);
  if (len < 0) SCM_ERR("list->string: bad list", list);
  
  str = scm_str_alloc(len);
  p = SCM_STR_VALUE(str);
  while(list) {
	if (!SCM_CHARP(SCM_CAR(list)))
	  SCM_ERR("list->string: bad char", SCM_CAR(list));
	*p++ = SCM_CHAR(SCM_CAR(list));
	list = SCM_CDR(list);
  }
  *p = 0;
  return(str);
}

/*S* (string-copy STRING) => STRING */
/*D* Returns a newly allocated copy of the given STRING. */
SOBJ scm_string_copy(SOBJ str)
{
  SOBJ s;
  
  if (!SCM_STRINGP(str))	SCM_ERR("string-copy: bad string", str);

  s = scm_str_alloc(SCM_STR_LEN(str));
  strncpy(SCM_STR_VALUE(s), SCM_STR_VALUE(str), SCM_STR_LEN(str));
  return(s);
}

/*S* (string-fill! STRING CHR) => #undefined */
/*D* Stores CHAR in every element of the given STRING and returns an
  unspecified value. */
SOBJ scm_string_fill(SOBJ str, SOBJ chr)
{
  int i;
  if (!SCM_STRINGP(str))	SCM_ERR("string-fill: bad string", str);
  if (!SCM_CHARP(chr))		SCM_ERR("string-fill: bad char", chr);
  
  for (i = 0; i < SCM_STR_LEN(str); i++) {
	SCM_STR_VALUE(str)[i] = SCM_CHAR(chr);
  }
  return(scm_undefined);
}

/*E* (string-index STRING SEARCHED) => INDEX | #f */
/*D* Returns the index of first occurence of the SEARCHED string in
  STRING. If no occurence of SEARCHED is found, returns #f */
SOBJ scm_string_index(SOBJ instr, SOBJ ssearch)
{
  char *is, *ss, *q;
  int is_len, ss_len;
  char sbuf[2]="a";

  if (!SCM_STRINGP(instr))	  SCM_ERR("string-index: bad string", instr);
  is = SCM_STR_VALUE(instr);   	is_len = SCM_STR_LEN(instr);

  if (SCM_CHARP(ssearch)) {
	sbuf[0] = SCM_CHAR(ssearch);
	ss = sbuf;  ss_len = 1;
  } else if (SCM_STRINGP(ssearch)) {
	ss = SCM_STR_VALUE(ssearch);  ss_len = SCM_STR_LEN(ssearch);
  } else {
	SCM_ERR("string-index: bad search string", ssearch);
	return(scm_false);
  }
  q = is + (is_len - ss_len);
  while( is <= q ) {
	if (memcmp(is, ss, ss_len) == 0)
	  return(SCM_MKINUM((is - SCM_STR_VALUE(instr))));
	is++;
  }
  return(scm_false);
}


/*E* (string-chop STR) => STRING */
/*D* Modifies STR in such way that everything from the first NEWLINE
  to the end of line is removed. */
SOBJ scm_string_chop(SOBJ str)
{
  char *p, *l;
  
  if (!SCM_STRINGP(str)) return(str);
  
  p = SCM_STR_VALUE(str);
  l = p + SCM_STR_LEN(str);
  while(p < l) {
	if (*p == '\n') {
	  *p = 0;
	  SCM_STR_LEN(str) = p - SCM_STR_VALUE(str);
	  break;
	}
	p++;
  }
  return(str);
}

/*E* (string-split DELIM STRING) => LIST */
/*D* Returns a LIST of strings created by splitting STRING at each
  character that is in the DELIM argument. */
/*X* (string-split "." "comp.os.linux") => ("comp" "os" "linux") */
SOBJ scm_string_split(SOBJ delim, SOBJ str)
{
  char *p, *l, *s;
  SOBJ res, *pn;
  
  if (!SCM_STRINGP(str))	SCM_ERR("bad string", str);
  if (!SCM_STRINGP(delim))	SCM_ERR("bad string", delim);

  p = SCM_STR_VALUE(str);
  l = p + SCM_STR_LEN(str);
  s = p;
  res = NULL;
  pn = &res;
  
  while(p < l) {
	if (strchr(SCM_STR_VALUE(delim), *p)) { /* is a delim ? */
	  SOBJ tmp = scm_str_alloc( p - s );
	  strncpy(SCM_STR_VALUE(tmp), s, p - s);
	  SCM_STR_VALUE(tmp)[p-s] = 0;
	  *pn = scm_cons(tmp, NULL);
	  pn = &SCM_CDR(*pn);
	  s = p+1;
	}
	p++;
  }
  *pn = scm_cons(scm_mkstring(s), NULL);
  return(res);
}

/*E* (string-join SEP LIST) => STRING */
/*D* Return a STRING which is the result of the concatenation of each
  string of LIST separated by SEP. */
/*X* (string-join "." '("comp" "os" "linux")) => "comp.os.linux" */
SOBJ scm_string_join(SOBJ sep, SOBJ list)
{
  SOBJ l;
  int len, seplen, nstring;
  char *p;
  SOBJ str;

  if (!SCM_STRINGP(sep)) 		SCM_ERR("bad string", sep);
  len = 0;
  seplen = SCM_STR_LEN(sep);
  nstring = 0;
  for (l = list; l; l = SCM_CDR(l)) {
	if (!SCM_PAIRP(l))				SCM_ERR("bad list", list);
	if (!SCM_STRINGP(SCM_CAR(l)))	SCM_ERR("bad string", SCM_CAR(l));
	len += SCM_STR_LEN(SCM_CAR(l));
	nstring++;
  }
  if (nstring > 0) {
	str = scm_str_alloc(len + ((nstring - 1) * seplen));
	p = SCM_STR_VALUE(str);
	for (l = list; l; l = SCM_CDR(l)) {
	  strncpy(p, SCM_STR_VALUE(SCM_CAR(l)), SCM_STR_LEN(SCM_CAR(l)));
	  p += SCM_STR_LEN(SCM_CAR(l));
	  if (SCM_CDR(l)) {
		strncpy(p, SCM_STR_VALUE(sep), seplen);
		p += seplen;
	  }
	}
	return(str);
  }
  return(scm_mkstring(""));
}

/*E* (string-lower STR1) => STRING */
/*D* Returns a newly allocated STRING which is a copy of STR1 with all
  characters converted to lower case */
SOBJ scm_string_lower(SOBJ str)
{
  SOBJ new;
  char *p, *l, *d;
  
  if (!SCM_STRINGP(str)) SCM_ERR("string-lower: bad string", str);
  
  new = scm_str_alloc(SCM_STR_LEN(str));
  
  d = SCM_STR_VALUE(new);
  p = SCM_STR_VALUE(str);
  l = p + SCM_STR_LEN(str);
  while(p < l) {
	*d = tolower(*p);
	p++;
	d++;
  }
  *d = 0;
  return(new);
}

/*E* (string-upper STRING) => STRING */
/*D* Returns a newly allocated STRING which is a copy of STR1 with all
  characters converted to upper case */
SOBJ scm_string_upper(SOBJ str)
{
  SOBJ new;
  char *p, *l, *d;
  
  if (!SCM_STRINGP(str)) SCM_ERR("string-upper: bad string", str);
  
  new = scm_str_alloc(SCM_STR_LEN(str));
  
  d = SCM_STR_VALUE(new);
  p = SCM_STR_VALUE(str);
  l = p + SCM_STR_LEN(str);
  while(p < l) {
	*d = toupper(*p);
	p++;
	d++;
  }
  *d = 0;
  return(new);
}

/*E* (string-translate STR WHAT REPL) => STRING*/
/*D* Returns a newly allocated string where all chars of STR having a
  match in the WHAT string are replaced by the corresponding char in
  the REPL string. */
/*X* (string-translate "comp.os.linux" "." "-") => "comp-os-linux" */
SOBJ scm_string_translate(SOBJ str, SOBJ fr, SOBJ to)
{
  SOBJ new;
  char *p, *q, *fstr, *tstr;
  if (!SCM_STRINGP(str))	SCM_ERR("bad string", str);
  if (!SCM_STRINGP(fr))		SCM_ERR("bad string", fr);
  if (!SCM_STRINGP(to))		SCM_ERR("bad string", to);

  if (SCM_STR_LEN(fr) != SCM_STR_LEN(to))
	SCM_ERR("length of map string does not match", scm_cons(fr,to));

  fstr = SCM_STR_VALUE(fr);
  tstr = SCM_STR_VALUE(to);

  new = scm_mkstring(SCM_STR_VALUE(str));
  
  p = SCM_STR_VALUE(new);
  while(*p) {
	if ((q = strchr(fstr, *p)) != NULL) {
	  *p = tstr[ q - fstr ];
	}
	p++;
  }
  return(new);
}

/*E* (string-pack TEMPLATE OBJ...) => STRING */
/*D* Return a new string containing a binary structure. The TEMPLATE
  is a string giving order and type of value to convert. Values are
  taken from the list of OBJ. TEMPLATE format is the same as the perl
  one. */

SOBJ scm_string_pack(int nargs, SOBJ *arg)
{
  char *fmt;
  SOBJ str;
  int c, rpt;

  if (nargs < 2)
	SCM_ERR("string-pack: bad number of args", NULL);
  
  if (!SCM_STRINGP(arg[0]))
	SCM_ERR("string-pack: bad format", arg[0]);

  str = scm_mkstring(NULL);
  
  fmt = SCM_STR_VALUE(*arg);
  arg++; nargs--;
  
  while(*fmt != 0) {
	c = *fmt++ & 0xff;
	if (isspace(c)) 	continue;

	if (strchr("@xX", c) == NULL &&	nargs <= 0)
	  SCM_ERR("string-pack: not enough args", NULL);


	/* parse optionnal repetition number */
	if (*fmt != 0 && isdigit((int)(*fmt))) {
	  for (rpt = 0; *fmt != 0 && isdigit((int)(*fmt)); fmt++)
		rpt = (rpt * 10) + (*fmt - '0');

	} else if (*fmt != 0 && *fmt == '*') {
	  fmt++;
	  rpt = -1;
	} else {
	  rpt = 1;
	}

	switch(c) {
	case 'a':					/* string with binary data, pad 0 */
	case 'A':					/* Ascii string, pad ' ' */
	case 'Z':					/* null terminated string, pad 0 */
	  {
		int alen;				/* argument len */
		int slen;				/* string len */
		int fill;

		if (!SCM_STRINGP(*arg))	SCM_ERR("pack: bad string", *arg);

		alen = SCM_STR_LEN(*arg);
		slen = SCM_STR_LEN(str);

		if (rpt == -1)  rpt = alen;
		
		fill = rpt - alen;
		str = scm_str_resize(str, slen + rpt);
		  
		if (fill > 0) {			/* have to fill */
		  memcpy(SCM_STR_VALUE(str) + slen, SCM_STR_VALUE(*arg), alen);
		  memset(SCM_STR_VALUE(str) + slen + alen, (c=='A')?' ':0, fill);
		} else {				/* have to trucate */
		  memcpy(SCM_STR_VALUE(str) + slen, SCM_STR_VALUE(*arg), rpt);
		}
		arg++;
		nargs--;
	  }
	  break;

	case 'b':					/* bit string (ascending bit order). */
	  {
		int slen, nbytes, r, i;
		char *s, *d, *l;

		if (!SCM_STRINGP(*arg)) SCM_ERR("pack: bad string", *arg);

		/* if '*', use arg string length */
		if (rpt == -1) rpt = SCM_STR_LEN(*arg);
		nbytes = (rpt  + 7) / 8;
		slen = SCM_STR_LEN(str);
		str  = scm_str_resize(str, slen + nbytes);

		s = SCM_STR_VALUE(*arg);
		d = SCM_STR_VALUE(str) + slen;	l = d + slen + nbytes;
		while (d < l) {
		  for (r = 0, i = 0; i < 8; i++) {
			r = r >> 1;
			if (*s && *s++ != '0') r |= 0x80;
		  }
		  *d++ = r;
		}
		break;
	  }
	case 'B':					/* bit string (descending bit order). */
	  {
		int slen, nbytes, r, i;
		char *s, *d, *l;

		if (!SCM_STRINGP(*arg)) SCM_ERR("pack: bad string", *arg);

		/* if '*', use arg string length */
		if (rpt == -1) rpt = SCM_STR_LEN(*arg);
		nbytes = (rpt  + 7) / 8;
		slen = SCM_STR_LEN(str);
		str  = scm_str_resize(str, slen + nbytes);

		s = SCM_STR_VALUE(*arg);
		d = SCM_STR_VALUE(str) + slen;	l = d + slen + nbytes;
		while (d < l) {
		  for (r = 0, i = 0; i < 8; i++) {
			r = r << 1;
			if (*s && *s++ != '0') r |= 1;
		  }
		  *d++ = r;
		}
		break;
	  }
	case 'h':					/* hex string (low nybble first). */
	  {
		int slen, nbytes, r, i;
		char *s, *d, *l;

		if (!SCM_STRINGP(*arg)) SCM_ERR("pack: bad string", *arg);

		/* if '*', use arg string length */
		if (rpt == -1) rpt = SCM_STR_LEN(*arg);
		nbytes = (rpt  + 1) / 2;
		slen = SCM_STR_LEN(str);
		str  = scm_str_resize(str, slen + nbytes);

		s = SCM_STR_VALUE(*arg);
		d = SCM_STR_VALUE(str) + slen;	l = d + slen + nbytes;
		while (d < l) {
		  r = 0;
		  if (*s && (i = *s++) && isxdigit(i)) 	r |= xdigit2nibble(i);
		  if (*s && (i = *s++) && isxdigit(i)) 	r |= (xdigit2nibble(i) << 4);
		  *d++ = r;
		}			
		break;
	  }
	  
	case 'H':					/* hex string (high nybble first). */
	  {
		int slen, nbytes, r, i;
		char *s, *d, *l;

		if (!SCM_STRINGP(*arg)) SCM_ERR("pack: bad string", *arg);

		/* if '*', use arg string length */
		if (rpt == -1) rpt = SCM_STR_LEN(*arg);
		nbytes = (rpt  + 1) / 2;
		slen = SCM_STR_LEN(str);
		str  = scm_str_resize(str, slen + nbytes);

		s = SCM_STR_VALUE(*arg);
		d = SCM_STR_VALUE(str) + slen;	l = d + slen + nbytes;
		while (d < l) {
		  r = 0;
		  if (*s && isxdigit( (i = *s++) )) 	r |= (xdigit2nibble(i) << 4);
		  if (*s && isxdigit( (i = *s++) )) 	r |= xdigit2nibble(i);
		  *d++ = r;
		}			
		break;
	  }
	  
	case 'c':					/* signed char value 		*/
	case 'C':					/* unsigned char value 		*/
	  if (rpt == -1) rpt = nargs;
	  if (rpt > nargs) SCM_ERR("string-pack: bad number of args",NULL);

	  while(rpt-- > 0) {
		str = scm_strcat_chr(str, scm_number2long(*arg));
		arg++; nargs--;
	  }
	  break;
	  
	case 's':					/* signed short value 		*/
	case 'S':					/* unsigned short value 	*/
	  if (rpt == -1) rpt = nargs;
	  if (rpt > nargs) SCM_ERR("string-pack: bad number of args",NULL);

	  while(rpt-- > 0) {
		short n;
		n = scm_number2long(*arg);
		str = scm_strcat_buf(str, (void*)&n, sizeof(n));
		arg++; nargs--;
	  }
	  break;
	  
	case 'i':					/* signed integer value 	*/
	case 'I':					/* unsigned integer value 	*/
	  if (rpt == -1) rpt = nargs;
	  if (rpt > nargs) SCM_ERR("string-pack: bad number of args",NULL);

	  while(rpt-- > 0) {
		int n;
		n = scm_number2long(*arg);
		str = scm_strcat_buf(str, (void*)&n, sizeof(n));
		arg++; nargs--;
	  }
	  break;
	  
	case 'l':					/* signed long value 		*/
	case 'L':					/* An unsigned long value 	*/
	  if (rpt == -1) rpt = nargs;
	  if (rpt > nargs) SCM_ERR("string-pack: bad number of args",NULL);

	  while(rpt-- > 0) {
		long n;
		n = scm_number2long(*arg);
		str = scm_strcat_buf(str, (void*)&n, sizeof(n));
		arg++; nargs--;
	  }
	  break;
	  
	case 'n':					/* short "network" (big-endian) 16b */
	  if (rpt == -1) rpt = nargs;
	  if (rpt > nargs) SCM_ERR("string-pack: bad number of args",NULL);

	  while(rpt-- > 0) {
		short n;
		n = scm_number2long(*arg);
		str = scm_strcat_chr(str, n >> 8);
		str = scm_strcat_chr(str, n);
		arg++; nargs--;
	  }
	  break;
	  
	  
	case 'N':					/* long  "network" (big-endian) 32b */
	  if (rpt == -1) rpt = nargs;
	  if (rpt > nargs) SCM_ERR("string-pack: bad number of args",NULL);

	  while(rpt-- > 0) {
		long n;
		n = scm_number2long(*arg);
		str = scm_strcat_chr(str, n >> 24);
		str = scm_strcat_chr(str, n >> 16);
		str = scm_strcat_chr(str, n >> 8);
		str = scm_strcat_chr(str, n);
		arg++; nargs--;
	  }
	  break;
	  
	case 'v':					/* short "VAX" (little-endian)  16b */
	  if (rpt == -1) rpt = nargs;
	  if (rpt > nargs) SCM_ERR("string-pack: bad number of args",NULL);

	  while(rpt-- > 0) {
		short n;
		n = scm_number2long(*arg);
		str = scm_strcat_chr(str, n);
		str = scm_strcat_chr(str, n >> 8);
		arg++; nargs--;
	  }
	  break;
	  
	case 'V':					/* long  "VAX" (little-endian)  32b */
	  if (rpt == -1) rpt = nargs;
	  if (rpt > nargs) SCM_ERR("string-pack: bad number of args",NULL);

	  while(rpt-- > 0) {
		long n;
		n = scm_number2long(*arg);
		str = scm_strcat_chr(str, n);
		str = scm_strcat_chr(str, n >> 8);
		str = scm_strcat_chr(str, n >> 16);
		str = scm_strcat_chr(str, n >> 24);
		arg++; nargs--;
	  }
	  break;
	  
	case 'q':					/* signed quad (64-bit) value. */
	case 'Q':					/* unsigned quad value. */
	  SCM_ERR("string-pack: q and q not supported in format",NULL);
	  
	case 'f':					/* single-precision native float */
	  if (rpt == -1) rpt = nargs;
	  if (rpt > nargs) SCM_ERR("string-pack: bad number of args",NULL);

	  while(rpt-- > 0) {
		float n;
		n = scm_number2double(*arg);
		str = scm_strcat_buf(str, (void*)&n, sizeof(n));
		arg++; nargs--;
	  }
	  break;
	  
	case 'd':					/* double-precision native float */
	  if (rpt == -1) rpt = nargs;
	  if (rpt > nargs) SCM_ERR("string-pack: bad number of args",NULL);

	  while(rpt-- > 0) {
		double n;
		n = scm_number2double(*arg);
		str = scm_strcat_buf(str, (void*)&n, sizeof(n));
		arg++; nargs--;
	  }
	  break;

	case 'x':					/* null byte */
	  while(rpt-- > 0) {
		str = scm_strcat_chr(str, 0);
	  }
	  break;
			
	case 'X':					/* delete a byte */
	  if (rpt > 0) {
		if (SCM_STR_LEN(str) < rpt) {
		  SCM_STR_LEN(str) = 0;
		} else {
		  SCM_STR_LEN(str) -= rpt;
		}
	  }
	  break;

	case 'p':					/* pointer to null terminated string */
	case 'P':					/* pointer to fixed-length string */
	  {
		void *p;
		while(rpt-- > 0) {
		  p = scm_getstr(*arg);
		  str = scm_strcat_buf(str, (void*)&p, sizeof(p));
		  arg++; nargs--;
		}
	  }
	  break;

	case 'u':					/* uuencoded string */
	case 'w':					/* BER int */
	case '@':					/* Null fill to absolute position */
	  SCM_ERR("string-pack: unsupported char in format", scm_mkchar(c));
	}
  }
  SCM_STR_VALUE(str)[SCM_STR_LEN(str)] = 0;
  return(str);
}

/*E* (string-unpack TEMPLATE STRING) => LIST */
/*D* Unpack a string containing a binary structure to a list of
  elements. Convertions is driven by the content of the TEMPLATE
  string */

SOBJ scm_string_unpack(SOBJ tmpl, SOBJ string)
{
  SOBJ l, *cdrp,new;
  char *fmt, *str, *slimit;
  int c, rpt, slen;

  if (!SCM_STRINGP(tmpl)) 	SCM_ERR("string-unpack: bad template", tmpl);
  if (!SCM_STRINGP(string)) SCM_ERR("string-unpack: bad string", string);

  l = NULL;  new = NULL;
  cdrp = &l;
  fmt = SCM_STR_VALUE(tmpl);
  str = SCM_STR_VALUE(string);
  slen = SCM_STR_LEN(string);
  slimit = str + slen;
  
  while(*fmt != 0) {
	c = *fmt++ & 0xff;
	if (isspace(c)) continue;

	/* parse optionnal repetition number */
	if (*fmt != 0 && isdigit((int)(*fmt))) {
	  for (rpt = 0; *fmt != 0 && isdigit((int)(*fmt)); fmt++)
		rpt = (rpt * 10) + (*fmt - '0');
	} else if (*fmt != 0 && *fmt == '*') {
	  fmt++;
	  rpt = -1;
	} else {
	  rpt = 1;
	}
	switch(c) {
	case 'x':					/* null bytes */
	  while(--rpt >= 0 && str < slimit) {
		str++;
	  }
	  continue;
	case 'X':					/* ignored */
	case '@':
	  continue;

	case 'a':
	case 'A':
	case 'Z':
	  {
		char *p, *l;

		l = (rpt == -1) ? slimit : str + rpt;
		if (l > slimit) l = slimit;
		if (c == 'Z') {
		  for (p = str; p < l && *p != 0; p++) ;
		  l = p;
		}
		new = scm_str_alloc(l - str);
		memcpy(SCM_STR_VALUE(new), str, l - str);
		SCM_STR_VALUE(new)[SCM_STR_LEN(new)] = 0;
		str = l;
		break;
	  }
	case 'b':
	case 'B':
	  {
		char *p, *l, *d;
		int n, i, nbytes;

		if (rpt == -1)  rpt = (slimit - str) * 8;
		nbytes = (rpt + 7) / 8;
		new = scm_str_alloc(rpt);
		p = str; l = str + nbytes;	d = SCM_STR_VALUE(new);
		if (l > slimit) l = slimit;
		while(p < l) {
		  n = *p++;
		  for (i = 0; i < 8 && --rpt >= 0; i++) {
			if (c == 'b') {
			  *d++ = (n & 0x01) ? '1' : '0';	n >>= 1;
			} else {
			  *d++ = (n & 0x80) ? '1' : '0';	n <<= 1;
			}
		  }
		}
		*d = 0;	str = l;
		break;
	  }		
	case 'h':					/* hex digit (low nibble first).*/
	case 'H':					/* hex digit (high nibble first). */
	  {
		char *d, *p, *l;
		int n, nbytes;
		if (rpt == -1) rpt = (slimit - str) * 2;
		nbytes = (rpt + 1) / 2;
		new = scm_str_alloc(rpt);
		p = str;  l = str + rpt;  d = SCM_STR_VALUE(new);
		if (l > slimit) l = slimit;
		while(p < l) {
		  n = *p++;
		  if (c == 'h') {
			if (--rpt >= 0) { *d++ = nibble2xdigit(n);    }
			if (--rpt >= 0) { *d++ = nibble2xdigit(n>>4); }
		  } else {
			if (--rpt >= 0) { *d++ = nibble2xdigit(n>>4); }
			if (--rpt >= 0) { *d++ = nibble2xdigit(n);    }
		  }
		}
		*d = 0;	str = l;
	  }
	  break;
	case 'c':					/* signed char */
	case 'C':					/* unsigned char */
	  {
		char *l;
		int n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + rpt) > slimit)	l = slimit;
		}
		while(str < l) {
		  n = *str++;
		  if (c == 'C') 	n &= 0xff;
		  *cdrp = scm_cons(SCM_MKINUM(c), NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	case 's':					/* signed short */
	case 'S':					/* unsigned short */
	  {
		char *l;
		short n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  memcpy(&n, str, sizeof(n));  str += sizeof(n);
		  new = SCM_MKINUM( (c == 'S') ? ((unsigned int)n) : n );
		  *cdrp = scm_cons(new, NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	case 'i':					/* signed int */
	  {
		char *l;
		int n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  memcpy(&n, str, sizeof(n));  str += sizeof(n);
		  *cdrp = scm_cons(scm_int2num(n), NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	case 'I':					/* unsigned int */
	  {
		char *l;
		unsigned int n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  memcpy(&n, str, sizeof(n));  str += sizeof(n);
		  *cdrp = scm_cons(scm_uint2num(n), NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }

	case 'l':					/* signed long */
	  {
		char *l;
		long n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  memcpy(&n, str, sizeof(n));  str += sizeof(n);
		  *cdrp = scm_cons(scm_int2num(n), NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	case 'L':					/* unsigned long */
	  {
		char *l;
		unsigned long n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  memcpy(&n, str, sizeof(n));  str += sizeof(n);
		  *cdrp = scm_cons(scm_uint2num(n), NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	case 'n':
	  {
		char *l;
		unsigned short n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  n  = (*str++ & 0xff) << 8;
		  n |= (*str++ & 0xff);
		  *cdrp = scm_cons(scm_uint2num(n), NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	case 'N':
	  {
		char *l;
		unsigned long n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  n  = (*str++ & 0xff) << 24;
		  n |= (*str++ & 0xff) << 16;
		  n |= (*str++ & 0xff) << 8;
		  n |= (*str++ & 0xff);
		  *cdrp = scm_cons(scm_uint2num(n), NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	case 'v':
	  {
		char *l;
		unsigned short n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  n  = (*str++ & 0xff);
		  n |= (*str++ & 0xff) << 8;
		  *cdrp = scm_cons(scm_uint2num(n), NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	case 'V':
	  {
		char *l;
		unsigned long n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  n  = (*str++ & 0xff);
		  n |= (*str++ & 0xff) << 8;
		  n |= (*str++ & 0xff) << 16;
		  n |= (*str++ & 0xff) << 24;
		  *cdrp = scm_cons(scm_uint2num(n), NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	case 'f':					/* float number */
	  {
		char *l;
		float n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  memcpy(&n, str, sizeof(n));  str += sizeof(n);
		  *cdrp = scm_cons(scm_flt2num(n),NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	  
	case 'd':					/* double number */
	  {
		char *l;
		double n;
		if (rpt == -1) {
		  l = slimit;
		} else {
		  if ((l = str + (rpt * sizeof(n))) > slimit)	l = slimit;
		}
		while(str < l) {
		  memcpy(&n, str, sizeof(n));  str += sizeof(n);
		  *cdrp = scm_cons(scm_flt2num(n),NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }

	case 'p':					/* null terminated string */
	  {
		char *p;
		while(--rpt >= 0) {
		  if (str + sizeof(p) >= slimit)	break;
		  memcpy(&p, str, sizeof(p));	str += sizeof(p);
		  *cdrp = scm_cons(scm_mkstring(p),NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	case 'P':					/* structure */
	  {
		void *p;
		if (rpt >= 0 && (str + sizeof(p)) < slimit) {
		  memcpy(&p, str, sizeof(p));	str += sizeof(p);
		  new = scm_str_alloc(rpt);
		  memcpy(SCM_STR_VALUE(new), p, rpt);
		  *cdrp = scm_cons(new, NULL);
		  cdrp = &(SCM_CDR(*cdrp));
		}
		continue;
	  }
	  
	default:
	  SCM_ERR("string-unpack: unknow type char", scm_mkchar(c));
	}
	*cdrp = scm_cons(new, NULL);
	cdrp = &(SCM_CDR(*cdrp));
  }
  return(l);
}

/*E* (string-resize! STR LEN) => STR */
/*D* Change the size of the string STR to LEN. Returns the STR. */
SOBJ scm_string_resize(SOBJ str, SOBJ len)
{
  if (!SCM_INUMP(len)) SCM_ERR("string-resize!: bad length", len);
  return(scm_str_resize(str, SCM_INUM(len)));
}

void scm_init_str()
{
  /*-- r5rs string */
  scm_add_cprim("string?",			scm_stringp,		1);
  scm_add_cprim("make-string",		scm_make_string,	-1);
  scm_add_cprim("make-string2",		scm_make_string2,	2);
  scm_add_cprim("string",			scm_string,			-1);
  scm_add_cprim("string-length",	scm_string_length,	1);
  scm_add_cprim("string-ref",		scm_string_ref,		2);
  scm_add_cprim("string-set!",		scm_string_set,		3);

  scm_add_cprim("string<?",			scm_string_lt,		2);
  scm_add_cprim("string<=?",		scm_string_le,		2);
  scm_add_cprim("string=?",			scm_string_eq,		2);
  scm_add_cprim("string>=?",		scm_string_ge,		2);
  scm_add_cprim("string>?",			scm_string_gt,		2);

  scm_add_cprim("string-ci<?",		scm_string_ci_lt,		2);
  scm_add_cprim("string-ci<=?",		scm_string_ci_le,		2);
  scm_add_cprim("string-ci=?",		scm_string_ci_eq,		2);
  scm_add_cprim("string-ci>=?",		scm_string_ci_ge,		2);
  scm_add_cprim("string-ci>?",		scm_string_ci_gt,		2);

  scm_add_cprim("substring",		scm_substring,			3);
  scm_add_cprim("string-append",	scm_string_append, 		-1);

  scm_add_cprim("string->list",		scm_string_to_list,		1);
  scm_add_cprim("list->string",		scm_list_to_string,		1);
  scm_add_cprim("string->symbol",	scm_string_to_symbol,	1);
  scm_add_cprim("symbol->string",  	scm_symbol_to_string,	1);
  scm_add_cprim("string-copy",		scm_string_copy,		1);
  scm_add_cprim("string-fill!",		scm_string_fill,		2);
  
  /*-- string extensions */

  scm_add_cprim("string-append2",		scm_string_append2, 	2);
  scm_add_cprim("string-append-char", 	scm_string_append_char, 2);
  scm_add_cprim("string-concat!",		scm_string_concat,		2);

  scm_add_cprim("string-index",		scm_string_index, 		2);
  scm_add_cprim("string-chop",		scm_string_chop,  		1);
  scm_add_cprim("string-split",		scm_string_split, 		2);
  scm_add_cprim("string-join",		scm_string_join,  		2);
  scm_add_cprim("string-lower",		scm_string_lower,		1);
  scm_add_cprim("string-upper",		scm_string_upper,		1);
  scm_add_cprim("string-translate",	scm_string_translate,	3);

  scm_add_cprim("string-pack",		scm_string_pack,		-1);
  scm_add_cprim("string-unpack",	scm_string_unpack,		2);
  scm_add_cprim("string-resize!", 	scm_string_resize,		2);
}
