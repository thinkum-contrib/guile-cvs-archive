/* -*- tab-width:4; -*- */

#include "s.h"
#include <stdio.h>
#include <stdarg.h>
#include <avcall.h>

#define SCM_STRBUF_MIN_SIZE		8

#define SCM_STRBUF_EXPONENTIAL
#define SCM_STRBUF_PROFILE

/* String buffer utilities */

typedef struct {
  int	len;
  int 	max;
#ifdef SCM_STRBUF_PROFILE
  int 	nalloc;
  int	nrealloc;
  int	bmove;
#endif
  char 	str[1];					/* the string itself */
} SCM_STRBUF;

SCM_STRBUF *scm_strbuf_new()
{
  SCM_STRBUF *new = scm_must_alloc(sizeof(SCM_STRBUF));
  memset(new, 0, sizeof(SCM_STRBUF));
#ifdef SCM_STRBUF_PROFILE
  new->nalloc = 1;
#endif  
  return(new);
}

SCM_STRBUF *scm_strbuf_resize(SCM_STRBUF *sb, int newlen)
{
  int n;

#ifdef SCM_STRBUF_EXPONENTIAL
  int wanted;
  if (newlen < 0)	newlen = 0;

  wanted = newlen + 1;
  n = (sb->max) ? sb->max : SCM_STRBUF_MIN_SIZE;
  while(n < wanted) {
	n *= 2;
  }
  if (n != sb->max) {
	fprintf(stderr, "*** strbuf realloc: wanted=%d realloc=%d\n", newlen, n);
	sb = scm_must_realloc(sb, n + sizeof(SCM_STRBUF));
#ifdef SCM_STRBUF_PROFILE
	sb->nrealloc++;		sb->bmove += (sb->max + sizeof(SCM_STRBUF));
#endif
  }
  sb->max = n;
#else
  if (newlen < 0)	newlen = 0;

  n = (newlen + 1) + (SCM_STRBUF_MIN_SIZE - 1);
  n = n - (n % SCM_STRBUF_MIN_SIZE);

  if (n != sb->max) {
	fprintf(stderr, "*** strbuf realloc %d\n", n);
	sb = scm_must_realloc(sb, n);
#ifdef SCM_STRBUF_PROFILE
	sb->nrealloc++;		sb->bmove += (sb->max + sizeof(SCM_STRBUF));
#endif
	}
  }
  sb->max = n;
#endif
  return(sb);
}

SCM_STRBUF *scm_strbuf_clear(SCM_STRBUF *sb)
{
  return(scm_strbuf_resize(sb, 0));
}

SCM_STRBUF *scm_strbuf_concat_buf(SCM_STRBUF *sb, char *buf, int len)
{
  sb = scm_strbuf_resize(sb, sb->len + len);
  strncpy(sb->str + sb->len, buf, len);
  sb->len += len;
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

void scm_strbuf_test()
{
  int i;
  SCM_STRBUF *sb = scm_strbuf_new();
  sb = scm_strbuf_sprintf(sb, "Hello world\n");
  sb = scm_strbuf_print(sb);
  for (i = 0; i < 100000; i++) {
	sb = scm_strbuf_concat_sprintf(sb, "I said %d times\n", i+1);
  }
  /* sb = scm_strbuf_print(sb); */
  printf("len = %d\n", sb->len);
  scm_strbuf_stats(sb);
}

main()
{
  printf("sizeof strbuf = %d\n", sizeof(SCM_STRBUF));
  scm_strbuf_test();
}
