/* -*- tab-width:4; -*-
 * File and string ports
 *
 * $Id$
 *
 */

#include "s.h"
#include "vm2.h"
#include "heap.h"
#include <unistd.h>

#ifdef _IO_getc_unlocked
#define fast_getc	_IO_getc_unlocked
#else
#define fast_getc 	getc_unlocked
#endif

/************************************************************************
 * Variables
 ************************************************************************/
static PORT *scm_inp, *scm_outp, *scm_errp;

SOBJ scm_in_port;
SOBJ scm_out_port;
SOBJ scm_err_port;
SOBJ scm_eof;

static int scm_float_digits = 10;

#define SCM_INP		SCM_PORT(scm_in_port)
#define SCM_OUTP	SCM_PORT(scm_out_port)
#define SCM_ERRP	SCM_PORT(scm_err_port)

/************************************************************************
 * Common port routines
 ************************************************************************/

static PORT *port_new(int type, int flag)
{
  PORT *p = scm_must_alloc(sizeof(PORT));
  scm_mem_clear(p, sizeof(PORT));
  p->type = type;
  p->io_flag = flag;
  return(p);
}


/************************************************************************
 * File ports
 ************************************************************************/

static int port_file_getc(PORT *p)
{
  int c;
  if ((c = fast_getc(p->descr.f)) == EOF)  return(PORT_EOF);
  if (c == '\n') p->line++;
  return(c);
}

static int port_file_peekc(PORT *p)
{
  int c;
  if ((c = fast_getc(p->descr.f)) == EOF)  return(PORT_EOF);
  ungetc(c, p->descr.f);
  return(c);
}

static void port_file_putc(PORT *p, char c)
{
  putc(c, p->descr.f);
  if (c == '\n') p->line++;
}

static void port_file_close(PORT *p)
{
  if (p->descr.f) fclose(p->descr.f);
  scm_free(p);
}

static void port_file_seek(PORT *p, int pos)
{
  fseek(p->descr.f, pos, 0);
}

static int port_file_read(PORT *p, SOBJ str, int len)
{
  scm_str_resize(str, len);
  return(fread(SCM_STR_VALUE(str), 1, len, p->descr.f));
}

static int port_file_write(PORT *p, SOBJ str, int len)
{
  if (len > SCM_STR_LEN(str)) 	SCM_ERR("string not long enough", str);
  return(fwrite(SCM_STR_VALUE(str), 1, len, p->descr.f));
}

static int port_file_getline(PORT *p, SOBJ str)
{
  FILE *fd = p->descr.f;
  SCM_STRBUF *sb = SCM_STR(str);
  int c;

  if (sb->max < 8) {
	sb = scm_strbuf_resize(sb, 64);
  }
  sb->len = 0;
  while((c = fast_getc(fd)) != EOF) {
	if (c == '\n') 	break;
	sb->str[sb->len++] = c;
	if (sb->len >= sb->max) {
	  sb = scm_strbuf_resize(sb,sb->max + 1);
	}
  }
  sb->str[sb->len] = 0;
  SCM_STR(str) = sb;
  return( (c == EOF) ? -1 : sb->len);
}

static int port_file_putline(PORT *p, SOBJ str)
{
  int len = fwrite(SCM_STR_VALUE(str), 1, SCM_STR_LEN(str), p->descr.f);
  if (len >= 0) {
	putc('\n', p->descr.f);
	len++;
  }
  return(len);
}

static PORT *port_file_open(void *filename, int mode)
{
  PORT *port;
  FILE *fd;
  int  eflag, io;

  eflag = 0;
  switch(mode) {
  case PORT_READ:	fd = fopen(filename, "r");	io = PORT_IO_R;  	break;
  case PORT_CREATE:	fd = fopen(filename, "w");	io = PORT_IO_W;  	break;
  case PORT_APPEND:	fd = fopen(filename, "a");	io = PORT_IO_W;  	break;
  case PORT_UPDATE:	fd = fopen(filename, "r+");	io = PORT_IO_RW;  	break;
  case PORT_UPDATE_CREATE: fd = fopen(filename, "w+"); io = PORT_IO_RW; break;
  case PORT_UPDATE_APPEND: fd = fopen(filename, "a+"); io = PORT_IO_RW; break;
  default:
	return(NULL);
  }
  if (fd) {
	port = port_new(PORT_T_FILE, io);
	port->open_mode = mode;
	port->io_flag = io;
	port->descr.f = fd;
	return(port);
  }
  return(NULL);
}


/************************************************************************
 * string ports
 ************************************************************************/

static int port_string_getc(PORT *p)
{
  if (p->descr.s.index >= p->descr.s.length)	return(PORT_EOF);
  return(p->descr.s.data[p->descr.s.index++]);
}

static int port_string_peekc(PORT *p)
{
  if (p->descr.s.index >= p->descr.s.length)	return(PORT_EOF);
  return(p->descr.s.data[p->descr.s.index]);
}

static void port_string_putc(PORT *p, char c)
{
  if (p->descr.s.index >= (p->descr.s.alloced-1)) {
	if (p->descr.s.data != NULL) {
	  p->descr.s.data = scm_must_realloc(p->descr.s.data,
										 p->descr.s.alloced + PORT_STR_QTUM);
	  p->descr.s.alloced += PORT_STR_QTUM;
	} else {
	  p->descr.s.data = scm_must_alloc(PORT_STR_QTUM);
	  p->descr.s.alloced = PORT_STR_QTUM;
	}
  }
  p->descr.s.data[p->descr.s.index++] = c;
  p->descr.s.length = p->descr.s.index;
  p->descr.s.data[p->descr.s.index] = 0;
}

char *port_string_output_string(PORT *p)
{
  p->descr.s.data[p->descr.s.index] = 0;
  return(p->descr.s.data);
}

static void port_string_close(PORT *p)
{
  if (p->descr.s.data) 	scm_free(p->descr.s.data);
  scm_free(p);
}

static void port_string_seek(PORT *p, int pos)
{
  if (pos >= p->descr.s.length) return;
  p->descr.s.index = pos;
}

static int port_string_read(PORT *p, SOBJ str, int len)
{
  char *s, *l, *d, *dl;
  
  s = p->descr.s.data + p->descr.s.index;
  l = p->descr.s.data + p->descr.s.length;
  if (s >= l)	return(-1);

  scm_str_resize(str, len);
  d = SCM_STR_VALUE(str);
  dl= d + len;
  while(s < l && d < dl) {
	*d++ = *s++;
  }
  *d = 0;
  return(d - SCM_STR_VALUE(str));
}

static int port_string_write(PORT *p, SOBJ str, int len)
{
  if (len > SCM_STR_LEN(str))
	SCM_ERR("string not long enough", str);

  if ((p->descr.s.index + len) >= (p->descr.s.alloced - 1)) {
	if (p->descr.s.data != NULL) {
	  p->descr.s.data =
		scm_must_realloc(p->descr.s.data,
						 p->descr.s.alloced + len + PORT_STR_QTUM);
	  p->descr.s.alloced += len + PORT_STR_QTUM;
	} else {
	  p->descr.s.data =
		scm_must_alloc(len + PORT_STR_QTUM);
	  p->descr.s.alloced += len + PORT_STR_QTUM;
	}
  }
  memcpy(p->descr.s.data + p->descr.s.index, SCM_STR_VALUE(str), len);
  p->descr.s.index += len;
  return(len);
}

static int port_string_getline(PORT *p, SOBJ str)
{
  int c, len;
  char *start, *s, *l, *d;
  
  start = p->descr.s.data + p->descr.s.index;
  l = p->descr.s.data + p->descr.s.length;
  if (start >= l) 	return(-1);
  
  s = start;  len = 0;
  while(s < l) {
	c = *s++;
	if (c == '\r') 	continue;
	if (c == '\n') 	break;
	len++;
  }
  scm_str_resize(str, len);
  l = s;
  s = start;
  d = SCM_STR_VALUE(str);
  while(s < l) {
	c = *s++;
	if (c == '\r') 	continue;
	if (c == '\n') 	break;
	*d++ = c;
  }
  *d = 0;
  return(len);
}

static int port_string_putline(PORT *p, SOBJ str)
{
  int len = port_string_write(p, str, SCM_STR_LEN(str));
  if (len >= 0) {
	port_string_putc(p, '\n');
	len++;
  }
  return(len);
}

static PORT *port_string_open(void *p, int flag)
{
  PORT *port = port_new(PORT_T_STRING, 0);

  switch(flag) {
  case PORT_CREATE:
	port->io_flag = PORT_IO_W;
	break;
  case PORT_UPDATE:			
  case PORT_UPDATE_CREATE:	
  case PORT_UPDATE_APPEND:
	port->io_flag = PORT_IO_RW;
	break;
  case PORT_READ:			
	port->descr.s.data    = scm_must_strdup(p);
	port->descr.s.length  = strlen(p);
	port->descr.s.index   = 0;
	port->descr.s.alloced = port->descr.s.length+1;
	port->io_flag = PORT_IO_R;
	break;
  case PORT_APPEND:			
	port->descr.s.data    = scm_must_strdup(p);
	port->descr.s.length  = strlen(p);
	port->descr.s.index   = port->descr.s.length;
	port->descr.s.alloced = port->descr.s.length+1;
	port->io_flag = PORT_IO_W;
	break;
  }
  return(port);
}

/************************************************************************
 * Port table driver
 ************************************************************************/

PORT_DESCR port_driver[PORT_MAX_TYPES] =
{
  {	"file",
	port_file_close,
	port_file_getc,			port_file_peekc, 		port_file_putc,
	port_file_seek,
	port_file_read, 		port_file_write,
	port_file_getline,		port_file_putline
  },

  {	"string",
	port_string_close,
	port_string_getc, 		port_string_peekc, 		port_string_putc,
	port_string_seek,
	port_string_read, 		port_string_write,
	port_string_getline,	port_string_putline
  }
};


/************************************************************************
 * Public interface
 ************************************************************************/

void port_close(PORT *p)
{
  (*port_driver[p->type].close)(p);
}

int port_getc(PORT *p)
{
  return( (*port_driver[p->type].getc)(p) );
}

int port_peekc(PORT *p)
{
  return( (*port_driver[p->type].peekc)(p) );
}

void port_putc(PORT *p, char c)
{
  (*port_driver[p->type].putc)(p, c);
}

void port_seek(PORT *p, int pos)
{
  (*port_driver[p->type].seek)(p, pos);
}

int port_read(PORT *p, SOBJ str, int len)
{
  return (*port_driver[p->type].read)(p, str, len);
}

int port_write(PORT *p, SOBJ str, int len)
{
  return (*port_driver[p->type].write)(p, str, len);
}

int port_getline(PORT *p, SOBJ str)
{
  return (*port_driver[p->type].getline)(p, str);
}

int port_putline(PORT *p, SOBJ str)
{
  return (*port_driver[p->type].putline)(p, str);
}

/*-- high level interface */
PORT *port_open_input_file(char *fname)
{
  return(port_file_open(fname, PORT_READ));
}

PORT *port_open_output_file(char *fname)
{
  return(port_file_open(fname, PORT_CREATE));
}

PORT *port_open_input_string(char *string)
{
  return(port_string_open(string, PORT_READ));
}

PORT *port_open_output_string()
{
  return(port_string_open(NULL, PORT_CREATE));
}

void port_puts(PORT *p, char *str)
{
  SOBJ s = scm_mkstring(str);
  port_write(p, s, SCM_STR_LEN(s));
  scm_freecell(s);
}

void port_putn(PORT *p, long n)
{
  char buf[32];
  sprintf(buf, "%ld", n);
  port_puts(p, buf);
}

void port_putx(PORT *p, void *ptr)
{
  char buf[32];
  sprintf(buf, "%p", ptr);
  port_puts(p, buf);
}

void port_putd(PORT *p, double n)
{
  char buf[128];
  sprintf(buf, "%.*g", (scm_float_digits >= 16) ? 16: scm_float_digits, n);
  port_puts(p, buf);
}

/************************************************************************
 * Type related functions
 ************************************************************************/
SOBJ scm_mkport(PORT *port)
{
  SOBJ new = scm_newcell(SOBJ_T_PORT);
  SCM_PORT(new) = port;
  return(new);
}

/*** make a new port using file number */
SOBJ scm_mk_fn_port(int fn, int is_read)
{
  PORT *port;
  if (is_read) {
	port = port_new(PORT_T_FILE, PORT_READ);
	port->descr.f = fdopen(fn, "r");
  } else {
	port = port_new(PORT_T_FILE, PORT_CREATE);
	port->descr.f = fdopen(fn, "w");
  }
  return(scm_mkport(port));
}


void scm_port_sweep(SOBJ port)
{
  /* scm_puts("; gc: sweep port: "); scm_cprint(port); */
  if (SCM_PORT(port) != NULL) port_close(SCM_PORT(port));
}

void scm_port_print(SOBJ port, PORT *p)
{
  port_puts(p, "#<port ");
  if (SCM_PORT(port)) {
	port_putx(p, SCM_PORT(port));
  } else {
	port_puts(p, "nil");
  }
  port_putc(p, '>');
}

SCM_STRBUF *scm_port2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  if (SCM_PORT(obj)) 
	return(scm_strbuf_concat_sprintf(sb, "#<port %p>", SCM_PORT(obj)));

  return(scm_strbuf_concat_str(sb, "#<port nil>"));
}


void scm_eof_print(SOBJ port, PORT *p)
{
  port_puts(p, "#eof");
}

void scm_eof_write(SOBJ port, PORT *p)
{
  port_puts(p, "#eof");
}

SCM_STRBUF *scm_eof2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  return(scm_strbuf_concat_str(sb, "#<eof>"));
}

/************************************************************************
 * scheme functions
 ************************************************************************/
SCM_STRBUF *scm_list2str(SCM_STRBUF *sb, SOBJ l, int raw)
{
  SOBJ h, t;					/* hare and tortoise */
  char *spacer;

  h = t = l;
  spacer = "(";

  do {
	if (h == NULL || !SCM_PAIRP(h))		break;
	sb = scm_strbuf_concat_str(sb, spacer); spacer = " ";
	sb = scm_iobj2str(sb, SCM_CAR(h), raw);
	h = SCM_CDR(h);
	
	if (h == NULL || !SCM_PAIRP(h))		break;
	sb = scm_strbuf_concat_str(sb, spacer);
	sb = scm_iobj2str(sb, SCM_CAR(h), raw);
	h = SCM_CDR(h);
	t = SCM_CDR(t);
	
  } while( h != t );

  if (h) {
	sb = scm_strbuf_concat_str(sb, " . ");
	sb = scm_iobj2str(sb, h, raw);
  }
  sb = scm_strbuf_concat_str(sb, ")");
  return(sb);
}


SCM_STRBUF *scm_iobj2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  int otype;

  if (obj == NULL)
	return(scm_strbuf_concat_sprintf(sb, "()"));

  if (SCM_INUMP(obj))
	return(scm_strbuf_concat_sprintf(sb, "%d", SCM_INUM(obj)));

  if (scm_is_opcode_address(obj)) {
	return(scm_strbuf_concat_sprintf(sb, "#<opcode %s>",
									 scm_search_opcode_name(obj)));
  }
  if (obj >= (SOBJ)scm_stack && obj <= (SOBJ)scm_stack_limit) {
	return(scm_strbuf_concat_sprintf(sb, "#<sref %p>", obj));
  }
  if (obj->type & SCM_GCMARK_MASK) {
	fprintf(stderr, "scm_write_obj: cell at %p is gc-marked\n",obj);
  }
  otype = obj->type & ~(SCM_GCMARK_MASK);
  switch(otype) {
  case SOBJ_T_VOID:		return(scm_strbuf_concat_str(sb, "#void"));
  case SOBJ_T_FNUM:
	return(scm_strbuf_concat_sprintf(sb, "%.*g",
						(scm_float_digits >= 16) ? 16: scm_float_digits,
						SCM_FNUM(obj)));
  case SOBJ_T_BNUM: {
	char *s1 = mpz_get_str(NULL, 10, SCM_BNUM(obj));
	sb = scm_strbuf_concat_str(sb, s1);
	free(s1);
	return(sb);
  }

  case SOBJ_T_ATOM:
	return(scm_strbuf_concat_str(sb, SCM_ATOM_NAME(obj)));

  case SOBJ_T_SYMBOL:
	return(scm_strbuf_concat_sprintf(sb, "#<symbol %s>",
									 SCM_ATOM_NAME(SCM_SYM_NAME(obj))));

  case SOBJ_T_LSYMBOL:
	return(scm_strbuf_concat_sprintf(sb, "#<lsymbol %s %d>",
						SCM_ATOM_NAME(SCM_LSYM_NAME(obj)),
						SCM_LSYM_OFS(obj)));
  case SOBJ_T_KEYWORD:
	return(scm_strbuf_concat_sprintf(sb, "%s%s%s",
						scm_keyword_write_prefix,
						SCM_ATOM_NAME(SCM_KEYW_NAME(obj)),
						scm_keyword_write_suffix));
  case SOBJ_T_PAIR:
	return(scm_list2str(sb, obj, raw));

  case SOBJ_T_PRIM:
	return(scm_strbuf_concat_sprintf(sb, "#<prim %s arity=%d>",
						SCM_PRIM(obj)->name, SCM_PRIM(obj)->nargs));
	
  case SOBJ_T_CPRIM:
	return(scm_strbuf_concat_sprintf(sb, "#<cprim %p arity=%d>",
						SCM_CPRIM_FUNC(obj), SCM_CPRIM_NARGS(obj)));
  case SOBJ_T_BOOLEAN:
	return(scm_strbuf_concat_str(sb, (obj==scm_false) ? "#f":"#t"));

  case SOBJ_T_UNBOUND:		return(scm_strbuf_concat_str(sb, "#unbound"));
  case SOBJ_T_UNDEFINED:	return(scm_strbuf_concat_str(sb, "#undefined"));
  case SOBJ_T_FREE:			return(scm_strbuf_concat_str(sb, "#<free>"));

  }
  if (otype < SOBJ_T_MAX) {
	if (scm_type_hook[otype].tostr != NULL) {
	  return( (*scm_type_hook[otype].tostr)(sb, obj, raw) );
	}
	return(scm_strbuf_concat_sprintf(sb, "#<%s>", scm_type_hook[otype].name));
  }
  fprintf(stderr, "scm_iobj2str: no driver for object type %d\n", otype);
  return(sb);
}


void scm_write_obj(SOBJ obj, PORT *port, int raw)
{
  SOBJ p;
  int otype;
  SCM_STRBUF *sb;
  Sobject x;
  
  sb = scm_iobj2str(scm_strbuf_new(0), obj, raw);
  x.type = SOBJ_T_STRING;  SCM_STR(&x) = sb;
  port_write((port == NULL) ? SCM_OUTP : port, &x, SCM_STR_LEN(&x));
  scm_free(sb);
  return;

  if (port == NULL) port = SCM_OUTP;

  if (obj == NULL) {	port_puts(port, "()"); 				return; }
  if (SCM_INUMP(obj)) { port_putn(port, SCM_INUM(obj)); 	return; }

  if (scm_is_opcode_address(obj)) {
	port_puts(port, "#<vm-opcode ");
	port_puts(port, scm_search_opcode_name(obj));
	port_putc(port, '>');
	return;
  }
  if (obj >= (SOBJ)scm_stack && obj <= (SOBJ)scm_stack_limit) {
	port_puts(port, "#<sref "); port_putx(port, obj);  port_putc(port, '>');
	return;
  }
  if (!scm_is_pointer_to_heap(obj) && !scm_is_pointer_to_chr_array(obj)) {
	port_puts(port, "#<ptr ");  port_putx(port, obj);  port_putc(port, '>');
	return;
  }

  /* should never occur */
  if (obj->type & SCM_GCMARK_MASK) {
	fprintf(stderr, "scm_write_obj: cell at %p is gc-marked\n",obj);
	return;
  }

  otype = obj->type & ~(SCM_GCMARK_MASK);
  switch(otype) {
  case SOBJ_T_VOID:		port_puts(port, "#void");			break;
  case SOBJ_T_FNUM:		port_putd(port, SCM_FNUM(obj));		break;
  case SOBJ_T_BNUM:
	{
	  char *s = mpz_get_str(NULL, 10, SCM_BNUM(obj));
	  port_puts(port, s);
	  scm_free(s);
	  break;
	}
  case SOBJ_T_KEYWORD:
	port_puts(port, scm_keyword_write_prefix);
	port_puts(port, SCM_ATOM_NAME(SCM_KEYW_NAME(obj)));
	port_puts(port, scm_keyword_write_suffix);
	break;

  case SOBJ_T_SYMBOL:
	port_puts(port, "#<symbol ");
 	port_puts(port, SCM_ATOM_NAME(SCM_SYM_NAME(obj)));
	port_putc(port, '>');
	break;

  case SOBJ_T_LSYMBOL:
	port_puts(port, "#<lsym ");
	port_puts(port, SCM_ATOM_NAME(SCM_LSYM_NAME(obj)));
	port_putc(port, ' ');
	port_putn(port, SCM_LSYM_OFS(obj));
	port_putc(port, '>');
	break;

  case SOBJ_T_PAIR:
	port_putc(port, '(');
	for (p = obj; p; p = SCM_CDR(p)) {
	  if (p != obj) 	port_putc(port, ' ');
	  if (SCM_OBJTYPE(p) != SOBJ_T_PAIR) {
		if (SCM_OBJREF(p) != SCM_OBJREF(obj)) port_puts(port, ". ");
		scm_write_obj(p, port, raw);
		break;
	  }
	  scm_write_obj(SCM_CAR(p), port, raw);
	}
	port_putc(port, ')');
	break;
  case SOBJ_T_PRIM:
	port_puts(port, "#<prim ");	port_puts(port, SCM_PRIM(obj)->name);
	port_putc(port, ' ');			port_putn(port, SCM_PRIM(obj)->nargs);
	port_putc(port, '>');
	break;
  
  case SOBJ_T_CPRIM:
	port_puts(port, "#<cprim ");	port_putx(port, SCM_CPRIM_FUNC(obj));
	port_putc(port, ' ');			port_putn(port, SCM_CPRIM_NARGS(obj));
	port_putc(port, '>');
	break;
	
  case SOBJ_T_BOOLEAN:
	port_puts(port, (obj == scm_false) ? "#f" : "#t");
	break;
  case SOBJ_T_UNBOUND:		port_puts(port, "#unbound");  	break;
  case SOBJ_T_UNDEFINED:	port_puts(port, "#undefined");  break;
  case SOBJ_T_FREE:			port_puts(port, "#<free>");		break;
  default:
	if (otype < SOBJ_T_MAX) {
	  if (!raw && scm_type_hook[otype].write != NULL) {
		(*scm_type_hook[otype].write)(obj, port);
		return;
	  }
	  if (scm_type_hook[otype].print != NULL) {
		(*scm_type_hook[otype].print)(obj, port);
		return;
	  }
	}
	scm_puts("#<");	scm_puts(scm_type_hook[otype].name);
	scm_puts(" "); scm_putx(obj);
	scm_puts(">");
#ifdef COMMENT
	port_puts(SCM_ERRP, "scm_write_obj: object type ");
	port_putn(SCM_ERRP, otype);
	port_puts(SCM_ERRP, " not reconized\n");
#endif
  }
}

/* get PORT from SCM_PORT : signal error if port is not a valid port*/
static PORT *get_port(SOBJ port)
{
  if (port == NULL) return(SCM_OUTP);
  if (!SCM_PORTP(port)) SCM_ERR("bad port", port);
  return(SCM_PORT(port));
}

SOBJ scm_display2(SOBJ obj, SOBJ port)
{
  scm_write_obj(obj, get_port(port), 1);
  return(scm_undefined);
}

SOBJ scm_write2(SOBJ obj, SOBJ port)
{
  scm_write_obj(obj, get_port(port), 0);
  return(scm_undefined);
}

SOBJ scm_newline1(SOBJ port)
{
  port_putc(get_port(port), '\n');
  return(scm_undefined);
}

SOBJ scm_print2(SOBJ obj, SOBJ port)
{
  PORT *p = get_port(port);
  scm_write_obj(obj, p, 1);  port_putc(p, '\n');
  return(scm_undefined);
}

/*-- output to current-output port */
SOBJ scm_putc(int c)
{
  port_putc(SCM_OUTP, c);  return(NULL);
}

SOBJ scm_puts(char *s)
{
  port_puts(SCM_OUTP, s);  return(NULL);
}

SOBJ scm_putn(int n)
{
  port_putn(SCM_OUTP, n);	return(NULL);
}

SOBJ scm_putx(void *ptr)
{
  port_putx(SCM_OUTP, ptr);	return(NULL);
}

SOBJ scm_cdisplay(SOBJ obj)
{
  return(scm_display2(obj, NULL));
}

SOBJ scm_cwrite(SOBJ obj)
{
  return(scm_write2(obj, NULL));
}

SOBJ scm_cprint(SOBJ obj)
{
  return(scm_print2(obj, NULL));
}

/*E* (port? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a port, #f otherwise. */
SOBJ scm_portp(SOBJ x)
{
  return(SCM_MKBOOL(SCM_PORTP(x)));
}

/*S* (input-port? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a port and is a readable port, #f
  otherwise. */
SOBJ scm_input_portp(SOBJ x)
{
  return(SCM_MKBOOL( (SCM_PORTP(x) && 
					  SCM_PORT(x) &&
					  (SCM_PORT(x)->io_flag & PORT_IO_R)) ));
}

/*S* (output-port? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a port and is a writeable port, #f
  otherwise. */
SOBJ scm_output_portp(SOBJ x)
{
  return(SCM_MKBOOL( (SCM_PORTP(x) && 
					  SCM_PORT(x) &&
					  (SCM_PORT(x)->io_flag & PORT_IO_W)) ));
}

/*S* (current-input-port) => PORT */
/*D* Returns the current input port. */
SOBJ scm_current_input_port()
{
  return(scm_in_port);
}

/*S* (current-output-port) => PORT */
/*D* Returns the current output port. */
SOBJ scm_current_output_port()
{
  return(scm_out_port);
}

/*E* (current-error-port) => PORT */
/*D* Returns the current error port. */
SOBJ scm_current_error_port()
{
  return(scm_err_port);
}

/*** Redirector for input and output */

static SOBJ scm_port_redirect(SOBJ *old_port,
							  SOBJ (*open)(), int open_nargs,
							  SOBJ (*close)(SOBJ), int ret_close,
							  char *errmsg, SOBJ filename, SOBJ thunk)
{
  SOBJ port_save;
  SOBJ rval = NULL;
  jmp_buf handler_save;
  char errbuf[128];
  int k;

  if (open_nargs > 0 && !SCM_STRINGP(filename)) {
	sprintf(errbuf, "%s: bad filename", errmsg);
	SCM_ERR(errbuf, filename);
  }
  port_save = *old_port;
  *old_port = (*open)(filename);
  memcpy(handler_save, scm_errjmp, sizeof(jmp_buf));
  if ((k = setjmp(scm_errjmp)) == 0) { /* no err catched */
	rval = scm_apply0(thunk);
  } else {
	rval = NULL;				/* have peace with compiler */
  }
  if (ret_close) {
	rval = (*close)(*old_port);
  } else {
	(*close)(*old_port);
  }
	
  *old_port = port_save;
  memcpy(scm_errjmp, handler_save, sizeof(jmp_buf));
  if (k != 0) {
	sprintf(errbuf, "%s: io error on", errmsg);
	SCM_ERR(errbuf, filename);
  }
  return(rval);
}

/*S* (with-input-from-file FILENAME THUNK) => OBJ */
/*D* The file FILENAME is open for reading and the THUNK procedure is
  evaluated with it's current-input-port pointing to the just opened
  file. */
SOBJ scm_with_input_from_file(SOBJ filename, SOBJ thunk)
{
  return(scm_port_redirect(&scm_in_port,
						   scm_open_input_file, 1, scm_close_port, FALSE,
						   "with-input-from-file", filename, thunk));
}

/*S* (with-output-to-file FILENAME THUNK) => OBJ */
/*D* The file FILENAME is open for writing and the THUNK procedure is
  evaluated with it's current-output-port pointing to the just opened
  file. */
SOBJ scm_with_output_to_file(SOBJ filename, SOBJ thunk)
{
  return(scm_port_redirect(&scm_out_port,
						   scm_open_output_file, 1, scm_close_port, FALSE,
						   "with-output-to-file", filename, thunk));
}


/*E* (with-input-from-string STRING THUNK) => OBJ */
/*D* The file STRING is opened for reading and the THUNK procedure is
  evaluated with it's current-input-port pointing to the just opened
  string. */

SOBJ scm_with_input_from_string(SOBJ str, SOBJ thunk)
{
  return(scm_port_redirect(&scm_in_port,
						   scm_open_input_string, 1,  scm_close_port, FALSE,
						   "with-input-from-string", str, thunk));
}

/*E* (with-output-to-string THUNK)  => OBJ */
/*D* The file STRING is opened for writing and the THUNK procedure is
  evaluated with it's current-output-port pointing to the just opened
  string. */
SOBJ scm_with_output_to_string(SOBJ thunk)
{
  return(scm_port_redirect(&scm_out_port,
						   scm_open_output_string, 0, scm_close_port, TRUE,
						   "with-output-to-string", NULL, thunk));
}

/*S* (open-input-file NAME) => PORT */
/*D* Open file NAME for reading. If file does not exist an error
  occurs. */
SOBJ scm_open_input_file(SOBJ filename)
{
  SOBJ port = NULL;
  PORT *p;
  
  if (!SCM_STRINGP(filename))
	SCM_ERR("open-input-file: bad filename", filename);
  
  p = port_open_input_file(SCM_STR_VALUE(filename));
  if (p == NULL || (port = scm_mkport(p)) == NULL)
 	SCM_ERR("open-input-file: cannot open file", filename);
  return(port);
}

/*S* (open-output-file NAME) => PORT */
/*D* Open file NAME for writing. File is truncated or created. */
SOBJ scm_open_output_file(SOBJ filename)
{
  SOBJ port = NULL;
  PORT *p;

  if (!SCM_STRINGP(filename)) SCM_ERR("open-output-file: bad filename", filename);
  p = port_open_output_file(SCM_STR_VALUE(filename));
  if (p == NULL || (port = scm_mkport(p)) == NULL)
 	SCM_ERR("open-output-file: cannot open file", filename);
  return(port);
}

/*E* (open-input-string STRING) => PORT */
/*D* Open STRING for reading. */
SOBJ scm_open_input_string(SOBJ string)
{
  SOBJ port = NULL;
  PORT *p;

  if (!SCM_STRINGP(string))
	SCM_ERR("open-input-string: bad string", string);

  p = port_open_input_string(SCM_STR_VALUE(string));
  if (p == NULL || (port = scm_mkport(p)) == NULL)
 	SCM_ERR("open-input-string: cannot open string", string);
  
  return(port);
}

/*E* (open-output-string) => PORT */
/*D* Open a new string for writing. The string will be returned when port is closed. */
SOBJ scm_open_output_string()
{
  SOBJ port = NULL;
  PORT *p;

  p = port_open_output_string();
  if (p == NULL || (port = scm_mkport(p)) == NULL)
 	SCM_ERR("open-output-string: cannot open string", NULL);

  return(port);
}

/*E* (get-output-string PORT) => STRING */
/*D* Get current string for an output string port. */

SOBJ scm_get_output_string(SOBJ port)
{
  SOBJ str;
  
  if (!SCM_PORTP(port)) 	SCM_ERR("get-output-string: bad port", port);
  if (!SCM_STRING_PORTP(port) || !SCM_WRITE_PORTP(port)) 
	SCM_ERR("get-output-string: bad port type", port);
  
  str = scm_str_alloc(SCM_PORT(port)->descr.s.length);
  strncpy(SCM_STR_VALUE(str),
		  SCM_PORT(port)->descr.s.data,
		  SCM_PORT(port)->descr.s.length);
  return(str);
}

/*S* (close-port PORT) => BOOLEAN | STRING */
/*D* Close this port. Returns a STRING if port is an output string
  port, a BOOLEAN otherwise. */
SOBJ scm_close_port(SOBJ port)
{
  SOBJ rval;
  
  if (!SCM_PORTP(port)) 	SCM_ERR("close-port: bad port", port);

  rval = scm_true;
  if (SCM_PORT(port)) {

	if (SCM_STRING_PORTP(port) && SCM_WRITE_PORTP(port))
	  rval = scm_get_output_string(port);

	port_close(SCM_PORT(port));
	SCM_PORT(port) = NULL;
  }
  return(rval);
}

/*S* (close-input-port PORT) => BOOLEAN */
/*D* Close PORT and returns #t if no error ocurred, #f otherwise */

SOBJ scm_close_input_port(SOBJ port)
{
  return(scm_close_port(port));
}

/*S* (close-output-port PORT) => BOOLEAN | STRING */
/*D* Close PORT and return a STRING if port was an output string, a
  BOOLEAN otherwise. */

SOBJ scm_close_output_port(SOBJ port)
{
  return(scm_close_port(port));
}

/*S* (read [PORT]) => OBJ */
/*D* Read and parse an object from PORT if specified or from
  current-input-port. */
SOBJ scm_read(int argc, SOBJ *arg)
{
  PORT *port;
  
  if (argc >= 1) {
	if (!SCM_PORTP(arg[0])) 	SCM_ERR("read: bad port", arg[0]);
	port = SCM_PORT(arg[0]);
  } else {
	port = SCM_INP;
  }
  return(scm_read_port(port));
}

/*S* (read-char [PORT]) => CHAR */
/*D* Read a char from a PORT if specified or from
  current-input-port. */
SOBJ scm_read_char(int argc, SOBJ *arg)
{
  PORT *port;
  int c;

  if (argc >= 1) {
	if (!SCM_PORTP(arg[0])) 	SCM_ERR("read-char: bad port", arg[0]);
	port = SCM_PORT(arg[0]);
  } else {
	port = SCM_INP;
  }
  if (port == NULL) 			SCM_ERR("read-char: port closed", NULL);
  if ((c = port_getc(port)) == PORT_EOF)
	return(scm_eof);
  return(scm_mkchar(c));
}

/*S* (peek-char [PORT]) => CHAR */
/*D* Read a char from a PORT if specified or from
  current-input-port. The file pointer is not advanced, so next call
  to read-char or peek-char will return the same CHAR.*/
SOBJ scm_peek_char(int argc, SOBJ *arg)
{
  PORT *port;
  int c;

  if (argc >= 1) {
	if (!SCM_PORTP(arg[0])) 	SCM_ERR("read-char: bad port", arg[0]);
	port = SCM_PORT(arg[0]);
  } else {
	port = SCM_INP;
  }
  if (port == NULL) 			SCM_ERR("read-char: port closed", NULL);
  if ((c = port_peekc(port)) == PORT_EOF)
	return(scm_eof);
  return(scm_mkchar(c));
}

/*S* (eof-object? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is the eof object, #f otherwise */
SOBJ scm_eof_objectp(SOBJ obj)
{
  return(SCM_MKBOOL(obj == scm_eof));
}

/*E* (read-line STR PORT) => BOOL */
/*D* Read a full line from PORT. If end-of file is reached, #f is
 * returned. NOTE: STRING will not contain a newline character. */
SOBJ scm_read_line(SOBJ str, SOBJ port)
{
  if (!SCM_PORTP(port)) SCM_ERR("read-line: bad port", port);
  return(SCM_MKBOOL( port_getline(SCM_PORT(port), str) >= 0 ));
}

/*S* (char-ready? [PORT]) => BOOLEAN */
/*D* NOT IMPLEMENTED */
SOBJ scm_char_readyp(int argc, SOBJ *arg)
{
  SCM_ERR("char-ready?: not implemented", NULL);
  return(NULL);
}

/*S* (write OBJ [PORT]) => #undefined */
/*D* Writes a written representation of obj to the given port. Strings
  that appear in the written representation are enclosed in
  doublequotes, and within those strings backslash and doublequote
  characters are escaped by backslashes. If no PORT argument is given,
  the current-output-port is used.*/

/*S* (display OBJ [PORT]) => #undefined */
/*D* Writes a representation of obj to the given port.  Strings that
  appear in the written representation are not enclosed in
  doublequotes, and no characters are escaped within those strings.
  Character objects appear in the representation as if written by
  write-char instead of by write. If no PORT argument is given, the
  current-output-port is used.*/

/*S* (newline [PORT]) => #undefined */
/*D* Writes an end of line to port. If no PORT argument is given, the
  current-output-port is used. */

/*S* (write-char CHAR [PORT]) */
/*D* Writes the character CHAR to the given PORT. If no PORT argument
  is given, the current-output-port is used. */
SOBJ scm_write_char(int argc, SOBJ *arg)
{
  PORT *p;

  if (argc != 1 && argc != 2) 	SCM_ERR("write-char: bad number of args", NULL);

  if (!SCM_CHARP(arg[0])) SCM_ERR("write-char: bad char", arg[0]);
  if (argc == 1) {
	p = SCM_OUTP;
  }	else {
	if (!SCM_PORTP(arg[1]))	SCM_ERR("write-char: bad port", arg[1]);
	p = SCM_PORT(arg[1]);
  }
  port_putc(p, SCM_CHAR(arg[0]));
  return(scm_true);
}

/*E* (flush-output [PORT]) => #undefined */
/*D* Write anything that have been buffered in the PORT. If no PORT
  argument is given, the current-output-port is used. */
SOBJ scm_flush_output(int argc, SOBJ *arg)
{
  SOBJ port;
  if (argc > 1)		SCM_ERR("flush-output: bad number of args", NULL);
  
  port = (argc == 1) ? arg[0] : scm_out_port;
  if (!SCM_PORTP(port)) 	SCM_ERR("flush-output: bad port", port);

  if (SCM_FILE_PORTP(port) && SCM_WRITE_PORTP(port)) {
	fflush(SCM_PORT(port)->descr.f);
  }
  return(scm_true);
}

/*E* (file-position PORT [POS]) => NUMBER | BOOLEAN */
/*D* When POS is given, set the file position for the port, if
  possible and returns a #t if operation was successfull. If POS is
  not given, returns the position number for the PORT. */
SOBJ scm_file_position(int argc, SOBJ *arg)
{
  SOBJ port, pos;
  
  if (argc > 2 || argc < 1)
	SCM_ERR("file-position: bad number of args", NULL);

  port = arg[0];
  if (!SCM_PORTP(port)) 	SCM_ERR("file-position: bad port", port);
  if (argc == 2) {				/* set position */
	pos = arg[0];
	if (!SCM_NUMBERP(pos))	SCM_ERR("file-position: bad number", pos);
	return( fseek(SCM_PORT(port)->descr.f, scm_number2long(pos), 0) == 0 ?
			scm_true : scm_false);
  } else {						/* get position */
	return(scm_int2num(ftell(SCM_PORT(port)->descr.f)));
  }
}


/*S* (load FILENAME) => VALUE */
/*D* FILENAME should be a string naming an existing file containing
  Scheme source code.  The load procedure reads expressions and
  definitions from the file and evaluates them sequentially. Returns
  the value of last evaluated expression. */


/*S* (transcript-on STRING) => #undefined */
/*D* NOT IMPLEMENTED */

/*S* (transcript-off) => #undefined */
/*D* NOT IMPLEMENTED */

/* The idea of this extensions is to bind FILE and PORT
 * Examples:
 * (define p (open-file "/tmp/tst.txt" "w+"))
 * (fseek (port-file p) 10 0)
 * (fputs (prot-file p) "Hello")
 * (close-port p)
 *
 * (let ((f (make-file-port (fopen "/tmp/tst.txt" "w"))))
 *    (fputs (port->file f) "hello world\n")
 *	  (port-close f))
 */

/*I* (open-file FILENAME MODE) -> PORT */
/*I* (open-string STRING MODE) -> PORT */
/*I* (make-file-port FILE) -> PORT */
/*I* (port->file PORT) -> FILE */

/************************************************************************
 * Initialization
 ************************************************************************/

void scm_port_init_default_files()
{

  /* reopen  stdin out and err to SCM_INP, SCM_OUTP, SCM_ERRP ports*/
#ifdef KILL_STDIO
  int fn;

  fn = dup(0); fclose(stdin);   if (dup(fn) != 0) exit(2);
  SCM_INP = port_new(PORT_T_FILE, PORT_IO_R);
  SCM_INP->descr.f = fdopen(0, "r");

  fn = dup(1); fclose(stdout);  if (dup(fn) != 1) exit(2);
  SCM_OUTP = port_new(PORT_T_FILE, PORT_IO_W);
  SCM_OUTP->descr.f = fdopen(1, "w");
  
  fn = dup(2); fclose(stderr);  if (dup(fn) != 2) exit(2);
  SCM_ERRP = port_new(PORT_T_FILE, PORT_IO_W);
  SCM_ERRP->descr.f = fdopen(2, "w");
#else
  scm_inp  = port_new(PORT_T_FILE, PORT_IO_R);  scm_inp->descr.f = stdin;
  scm_outp = port_new(PORT_T_FILE, PORT_IO_W);  scm_outp->descr.f = stdout;
  scm_errp = port_new(PORT_T_FILE, PORT_IO_W);  scm_errp->descr.f = stderr;
#endif

}

/*E* (float-precision N) => OLDPREC */
/*D* Set the default number of fractional position that will be
  outputed when writing float numbers. Returns the precision that was
  in effect before calling this function. */

SOBJ scm_float_precision(SOBJ x)
{
  int old;
  
  if (!SCM_INUMP(x)) SCM_ERR("bad precision", x);
  if (SCM_INUM(x) < 0 || SCM_INUM(x) > 100)
	SCM_ERR("precision out of [0..100] range", x);
  old = scm_float_digits;
  scm_float_digits = SCM_INUM(x);
  return(SCM_MKINUM(old));
}

SOBJ scm_obj2str(SOBJ obj, SOBJ flag)
{
  SCM_STRBUF *sb;
  SOBJ new;

  sb = scm_iobj2str(scm_strbuf_new(0), obj, flag != scm_false);
  if (sb) {
	new = scm_newcell(SOBJ_T_STRING);
	SCM_STR(new) = sb;
	return(new);
  }
  return(scm_false);
#ifdef OLD  
  SOBJ new;
  char *p = scm_iobj2str(obj, (flag != scm_false));
  
  if (p) {
	new = scm_newcell(SOBJ_T_STRING);
	SCM_STR_VALUE(new) = p;
	SCM_STR_LEN(new) = strlen(p);
	return(new);
  }
  scm_free(p);
  return(scm_false);
#endif
}


void scm_init_port()
{
  scm_in_port  = scm_mkport(scm_inp);
  SCM_PORT(scm_in_port)->io_flag  = PORT_IO_R;

  scm_out_port = scm_mkport(scm_outp);
  SCM_PORT(scm_out_port)->io_flag = PORT_IO_W;

  scm_err_port = scm_mkport(scm_errp);
  SCM_PORT(scm_err_port)->io_flag = PORT_IO_W;
 
  scm_gc_protect(&scm_in_port);
  scm_gc_protect(&scm_out_port);
  scm_gc_protect(&scm_err_port);
  scm_eof= scm_newcell(SOBJ_T_EOF);		scm_gc_protect(&scm_eof);

  scm_add_cprim("port?",				scm_portp,					1);
  scm_add_cprim("input-port?",			scm_input_portp,			1);
  scm_add_cprim("output-port?",			scm_output_portp,			1);
  scm_add_cprim("current-input-port", 	scm_current_input_port, 	0);
  scm_add_cprim("current-output-port", 	scm_current_output_port, 	0);
  scm_add_cprim("current-error-port", 	scm_current_error_port, 	0);

  scm_add_cprim("with-input-from-file", scm_with_input_from_file, 	2);
  scm_add_cprim("with-output-to-file",	scm_with_output_to_file, 	2);

  scm_add_cprim("with-input-from-string",scm_with_input_from_string,2);
  scm_add_cprim("with-output-to-string", scm_with_output_to_string, 1);

  scm_add_cprim("open-input-file",		scm_open_input_file,		1);
  scm_add_cprim("open-output-file",		scm_open_output_file,		1);
  scm_add_cprim("open-input-string",	scm_open_input_string,		1);
  scm_add_cprim("open-output-string",	scm_open_output_string,		0);
  scm_add_cprim("get-output-string",	scm_get_output_string,		1);
  scm_add_cprim("close-port",			scm_close_port,				1);
  scm_add_cprim("close-input-port",		scm_close_input_port,		1);
  scm_add_cprim("close-output-port",	scm_close_output_port,		1);
  scm_add_cprim("read",					scm_read,					-1);
  scm_add_cprim("read-char",			scm_read_char,				-1);
  scm_add_cprim("peek-char",			scm_peek_char,				-1);
  scm_add_cprim("eof-object?",			scm_eof_objectp,			1);
  scm_add_cprim("char-ready?",			scm_char_readyp,			1);
  scm_add_cprim("read-line",			scm_read_line,				2);
  scm_add_cprim("write-char",			scm_write_char,				-1);
  scm_add_cprim("flush-output",			scm_flush_output,			-1);
  scm_add_cprim("file-position",		scm_file_position,			-1);
  scm_add_cprim("float-precision",		scm_float_precision,		1);
  scm_add_cprim("object->string",		scm_obj2str,				2);
}
