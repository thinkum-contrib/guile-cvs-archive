/* -*- tab-width:4; -*-
 *
 * My small scheme
 *
 * $Id$
 */
#include "s.h"
#include "vm2.h"
#include "heap.h"
#include "proc.h"
#include "stack.h"

/* #define OLD_INTERP */
/* #define SHOW_OPTIMIZE_PHASE */

/*-- arguments */
long scm_default_hs = 32;		/* default heap size in kilo cells */
long scm_default_ds = 32;		/* default data stack size in ptr to cells */
int scm_no_init=FALSE;			/* read default initfile */
int scm_interractive = TRUE;	/* interractive mode */
int scm_force_interractive = FALSE;

char *scm_progname;				/* program name (argv[0]) */
char *scm_prompt_str;			/* the string used to prompt */
char *scm_qs_lib;				/* QS_LIB environment variable */


/*-- cstack */
void *scm_cstack_limit;			/* top of C stack (system stack) */

SOBJ scm_last_read_object;		/* last object read (used by err)*/


/*-- This table is used to describe various object type aspects.
 * execute - the code to be compiled when object of this type is used
 * 			 like a function call. ex (<object> ...)
 * name - the type name
 * mark - the code to mark object during GC
 * sweep- howto destroy object when not used
 * print- pretty print of object
 * write- debug print of object
 * creconizer - used by read to reconize this type
 * cparse - call by read after object has been reconized to parse it
 * wreconizer - used by read to reconize this type
 * wparse - call by read after object has been reconized to parse it
 * compare - how to compare of 2 objects of this type
 *
 * NOTE: this table must be keept in sync with SOBJ_TYPES enum in s.h
 */

SOBJ_TYPE_DESCR scm_type_hook[SOBJ_T_MAX] = {
  /* builtin types */
  /* execute,			name, 			mark, 			sweep,
	 print, 					write, 			
	 creconize, 				cparse,
	 wreconize, 				wparse,
	 compare */

  { SCM_OP_NO_CALL,		"void", 		},
  { SCM_OP_NO_CALL, 	"pair", 		},
  { SCM_OP_NO_CALL, 	"inum", 		},
  { SCM_OP_NO_CALL, 	"fnum", 		},
  { SCM_OP_NO_CALL, 	"bignum", 		},
  { SCM_OP_NO_CALL, 	"atom", 		scm_atom_mark,   scm_atom_sweep,
    scm_atom_print,		scm_atom_print  },
  { SCM_OP_NO_CALL, 	"keyword", 		},
  { SCM_OP_NO_CALL, 	"symbol", 		scm_symbol_mark, scm_symbol_sweep },
  { SCM_OP_NO_CALL, 	"lsymbol", 		},
  { SCM_OP_NO_CALL, 	"label", 		},

  { SCM_OP_NO_CALL, 	"module", 		scm_module_mark, scm_module_sweep,
	scm_module_print,	scm_module_write,	scm_module2str,
	NULL,				NULL,
	scm_module_wreconize,  scm_module_wparse
  },

  { SCM_OP_NO_CALL, 	"char", 		NULL,			NULL,
	scm_char_print,				scm_char_write,		scm_char2str,
	scm_char_reconize,			scm_char_parse,
  },

  { SCM_OP_NO_CALL, 	"string", 		NULL,		   	scm_string_sweep,
	scm_string_print,			scm_string_write,	scm_string2str,
	scm_string_reconize,		scm_string_parse,
	NULL,						NULL,
	scm_string_compare
  },

  { SCM_OP_CALL_PRIM, 	"prim",			},
  { SCM_OP_CALL_CPRIM, 	"cprim", 		},

  { SCM_OP_NO_CALL, 	"syntax",		NULL,			NULL,
	scm_syntax_print },

  { SCM_OP_CALL_CODE, 	"code",			scm_code_mark, 	scm_code_sweep,
	scm_code_print,		NULL,			scm_code2str},

  { SCM_OP_CALL_PROC, 	"proc",			scm_proc_mark, 	scm_proc_sweep,
	scm_proc_print,		NULL,			scm_proc2str},
  
  { SCM_OP_CALL_CLOSURE,"closure",		scm_clos_mark,	NULL,
	scm_clos_print,		NULL,			scm_clos2str },
  
  { SCM_OP_NO_CALL, 	"env",			scm_env_mark,	scm_env_sweep,
	scm_env_print, 		NULL,			scm_env2str },
  
  { SCM_OP_CALL_MACRO, 	"macro",		scm_macro_mark,	NULL,
	NULL,			},
  
  { SCM_OP_NO_CALL, 	"port", 		NULL,			scm_port_sweep,
	scm_port_print,		NULL,			scm_port2str },

  { SCM_OP_NO_CALL, 	"boolean", 		},
  { SCM_OP_NO_CALL, 	"unbound", 		},
  { SCM_OP_NO_CALL, 	"undefined", 	},

  { SCM_OP_NO_CALL, 	"eof",			NULL,			NULL,
	scm_eof_print,		scm_eof_write
  },
  
  { SCM_OP_NO_CALL, 	"continuation", scm_cont_mark,	scm_cont_sweep,
	NULL },
  
  { SCM_OP_NO_CALL, 	"array",		scm_array_mark,	scm_array_sweep,
	scm_vector_print,	scm_vector_write,	scm_array2str,
	scm_array_reconize, scm_array_parse,
	NULL,				NULL,
	scm_array_compare   },

  { SCM_OP_NO_CALL, 	"hash",			scm_hash_mark,	scm_hash_sweep,
	scm_hash_print, 	scm_hash_write,		scm_hash2str,
	scm_hash_reconize,	scm_hash_parse,
	NULL
  },

  { SCM_OP_NO_CALL, 	"pointer",		scm_pointer_mark,	scm_pointer_sweep,
	scm_pointer_print,	scm_pointer_write, 	scm_pointer2str,
	NULL,				NULL,
	NULL,				NULL,
	scm_pointer_compare
  },
  
  { SCM_OP_CALL_EXTFUNC,"extfunc",		scm_extfunc_mark,	scm_extfunc_sweep,
	scm_extfunc_print,  scm_extfunc_write,	NULL },

  { SCM_OP_NO_CALL,		"var",			scm_var_mark,		scm_var_sweep,
	scm_var_print,   	scm_var_write,		NULL },

  { SCM_OP_CALL_VMFUNC,	"vmfunc",		scm_vmfunc_mark,	scm_vmfunc_sweep,
	NULL },

  { SCM_OP_NO_CALL,		"catch-context", scm_ccntxt_mark,	scm_ccntxt_sweep,
	NULL },
};

int scm_type_next_descr = SOBJ_T_USER;		/* next free descriptor */

/*-- predefined symbols */

SOBJ scm_true;					/* true */
SOBJ scm_false;					/* false */
SOBJ scm_unbound;				/* unbound */
SOBJ scm_undefined;				/* undefined  */

SOBJ scm_env,
  scm_sym_quote,
  scm_sym_qquote, scm_sym_unquote, scm_sym_unquote_splicing,
  scm_sym_set, scm_sym_if,
  scm_sym_lambda, scm_sym_begin, scm_sym_define,
  scm_sym_code, scm_sym_let, scm_sym_letstar, scm_sym_letrec,
  scm_sym_env,
  scm_sym_immediate,
  scm_sym_and, scm_sym_or,
  scm_sym_cond, scm_sym_else,
  scm_sym_while, scm_sym_until,
  scm_sym_do,
  scm_sym_catch;

/********************************
 * Disassembler
 ********************************/

SOBJ scm_disasm(SOBJ obj)
{
  SCM_Code *c;
  SOBJ env, code;
  int i,j;
  
  switch(SCM_OBJTYPE(obj)) {
  case SOBJ_T_PROC:
	c = SCM_PROC_CODE(obj);
	scm_puts("; ");
	port_puts(SCM_OUTP, "nargs="); port_putn(SCM_OUTP, c->nargs);
	port_puts(SCM_OUTP, " opt=");  port_putn(SCM_OUTP, c->optargs);
	port_puts(SCM_OUTP, " size="); port_putn(SCM_OUTP, c->size);
	scm_puts("\n");
	scm_puts("(lambda ");
	env = c->envlist;
	scm_cdisplay(env);
	scm_puts("\n");
	scm_disassemble(c->code, c->size);
	break;
  case SOBJ_T_CODE:
	scm_puts("(code\n");
	scm_puts("; ");
	port_puts(SCM_OUTP, "size=");  port_putn(SCM_OUTP, SCM_CODE_SIZE(obj));
	scm_puts("\n");
	scm_disassemble(SCM_CODE_CODE(obj), SCM_CODE_SIZE(obj));
	break;
  case SOBJ_T_CLOSURE:
	code = SCM_CLOSURE_CODE(obj);
	env  = SCM_CLOSURE_ENV(obj);
	scm_puts("; closure environment:\n");
	j = 0;
	while(env) {
	  scm_puts("; level #"); scm_putn(j); scm_puts(": ");
	  if (SCM_ENV_FRAME(env) == NULL) {
		scm_puts("**no frame**");
	  } else {
		for (i = 0; i < SCM_INUM(SCM_ENV_FRAME(env)->nslots); i++) {
		  scm_cdisplay(SCM_ENV_FRAME(env)->binding[i]);  scm_puts(" ");
		}
	  }
	  scm_puts("\n");
	  env = SCM_ENV_NEXT(env);
	  j++;
	}
	scm_disasm(code);
	return(scm_undefined);
  default:
	SCM_ERR("disasm: bad closure, code or proc", obj);
  }
  scm_puts(")\n");
  return(scm_undefined);
}


/********************************
 * Reader
 ********************************/

static char scm_tokbuf[1024];
static int  scm_tokbuf_len;		/* length of current token */
static SOBJ scm_tokval;			/* for user defined tokens */

enum SCM_TOKENS {
  SCM_TOK_EOF = 0,
  SCM_TOK_LPAR,
  SCM_TOK_RPAR,
  SCM_TOK_DOT,
  SCM_TOK_NUM,
  SCM_TOK_SYM,
  SCM_TOK_SHARP,
  SCM_TOK_BOOLEAN,
  SCM_TOK_VECTOR,
  SCM_TOK_QUOTE,
  SCM_TOK_QQUOTE,
  SCM_TOK_UNQUOTE,
  SCM_TOK_UNQUOTE_SPLICING,
  SCM_TOK_DQUOTE,
  SCM_TOK_EVAL,
  SCM_TOK_UDEF
};


/* jump over blanks: return the first non blank char */
int scm_skip_blanks(PORT *port)
{
  int c;
  while((c = port_getc(port)) != EOF && isspace(c))
	;
  return(c);
}

/* get next word to tokbuf */
int scm_get_word(PORT *port, int c)
{
  char *p;
  p = scm_tokbuf;
  *p++ = c;
  while( (c = port_peekc(port)) != EOF ) {
	if (isspace(c) || c == '(' || c == ')' || c == ';') 	break;
	*p++ = port_getc(port);
  }
  *p = 0;
  scm_tokbuf_len = p - scm_tokbuf;
  return(c);
}

int scm_read_token(PORT *port)
{
  int c, next, i;

start_again:

  /* ignore comments: ';' */
  while( (c = scm_skip_blanks(port)) == ';') {
	while((c = port_getc(port)) != '\n' && c != EOF)
	  ;
  }

  if (c == EOF) {	return(SCM_TOK_EOF);  }
  if (c == '(') {	return(SCM_TOK_LPAR); }
  if (c == ')') {	return(SCM_TOK_RPAR); }
  if (c == '\'') {	return(SCM_TOK_QUOTE); }
  if (c == '`') {	return(SCM_TOK_QQUOTE);	 }
  if (c == ',') {
	next = port_peekc(port);
	if (next != '@') 	return(SCM_TOK_UNQUOTE);
	port_getc(port);
	return(SCM_TOK_UNQUOTE_SPLICING);
  }
  if (c == '.') {
	next = port_peekc(port);
	if (isspace(next))	return(SCM_TOK_DOT);
  }

  if (c == '#') {
	next = port_peekc(port);
	if (strchr("bodxie", next)) { /* number marker */
	  scm_get_word(port, c);
	  return(SCM_TOK_NUM);		/* signal number */
	}
	
	if (next == 't' || next == 'f') {
	  scm_tokbuf[0] = next;	  scm_tokbuf[1] = 0;
	  port_getc(port);
	  return(SCM_TOK_BOOLEAN);
	}
	/* jump over the next char (just peeked) */	
	if (next == '.') {
	  port_getc(port);
	  return(SCM_TOK_EVAL);
	}

	/* try to detect keywords with #!<keyword> form */

	if (next == '!') {
	  port_getc(port);			/* get next_char */
	  if (!isalpha(port_peekc(port))) {
		while((c = port_getc(port)) != '\n' && c != EOF)
		  ;
		goto start_again;
	  }
	  scm_get_word(port, ':');
	  scm_tokval = scm_keyword_add(scm_tokbuf + 1);
	  return(SCM_TOK_UDEF);
	}
  }
  for (i = 0; i < scm_type_next_descr; i++) {
	if (scm_type_hook[i].creconize != NULL &&
		(*scm_type_hook[i].creconize)(port, c)) {
	  scm_tokval = (*scm_type_hook[i].cparse)(port, c);
	  return(SCM_TOK_UDEF);
	}
  }

  scm_get_word(port, c);

  if (isdigit((int)scm_tokbuf[0])) {		return(SCM_TOK_NUM); }
  if ((scm_tokbuf[0] == '-' || scm_tokbuf[0] == '+') &&
	  isdigit((int)scm_tokbuf[1])) {		return(SCM_TOK_NUM); }

  if (scm_tokbuf[0] == '.' &&
	  isdigit((int)scm_tokbuf[1])) {		return(SCM_TOK_NUM); }

  if (scm_tokbuf[0] == ':') {
	scm_tokval = scm_keyword_add(scm_tokbuf + 1);
	return(SCM_TOK_UDEF);
  }

  if (scm_tokbuf[scm_tokbuf_len - 1] == ':') {
	scm_tokbuf[scm_tokbuf_len-1] = 0;
	scm_tokval = scm_keyword_add(scm_tokbuf);
	return(SCM_TOK_UDEF);
  }

  /* trying to use a symbol looking like a generated one is an error */
  if (strncmp(scm_tokbuf, SCM_GENSYM_PREFIX, strlen(SCM_GENSYM_PREFIX)) == 0) {
	SCM_ERR("read: bad symbol name", scm_mkstring(scm_tokbuf));
  }

  for (i = 0; i < scm_type_next_descr; i++) {
	if (scm_type_hook[i].wreconize != NULL &&
		(*scm_type_hook[i].wreconize)(port, scm_tokbuf)) {
	  scm_tokval = (*scm_type_hook[i].wparse)(port, scm_tokbuf);
	  return(SCM_TOK_UDEF);
	}
  }
  return(SCM_TOK_SYM);
}

SOBJ scm_read_list(PORT *port)
{
  int tok;
  SOBJ list, last, obj;

  list = last = obj = NULL;

  while( (tok = scm_read_token(port)) != SCM_TOK_RPAR) {
	switch(tok) {
	case SCM_TOK_EOF:
	  fprintf(stderr, "scm_read_list: EOF while reading list\n");
	  return(scm_eof);

	case SCM_TOK_NUM:
	  obj = scm_cstr2number(scm_tokbuf, 10);
	  if (obj != scm_false)
		break;
	  /* FALL THROUGH */

	case SCM_TOK_SYM:	obj = scm_mkatom(scm_tokbuf);				break;
	case SCM_TOK_LPAR:	obj = scm_read_list(port);					break;
	case SCM_TOK_DOT:
	  if (SCM_NNULLP(last)) {
		SCM_CDR(last) = scm_read_port(port);
		if (scm_read_token(port) != SCM_TOK_RPAR) {
		  fprintf(stderr, "scm_read_list: illegal '.' expression\n");
		  return(NULL);
		}
		return(list);
	  }
	  SCM_ERR("scm_read_list: unexpected '.'", NULL);

	case SCM_TOK_SHARP:			/* special form */
	  break;

	case SCM_TOK_BOOLEAN:
	  obj = (scm_tokbuf[0] == 'f') ? scm_false : scm_true;
	  break;

	case SCM_TOK_UDEF:
	  obj = scm_tokval;
	  break;

	case SCM_TOK_QUOTE:
	  obj = SCM_LIST2(scm_sym_quote, scm_read_port(port));
	  break;

	case SCM_TOK_QQUOTE:
	  obj = SCM_LIST2(scm_sym_qquote,	scm_read_port(port));
	  break;
	case SCM_TOK_UNQUOTE:
	  obj = SCM_LIST2(scm_sym_unquote, scm_read_port(port));
	  break;

	case SCM_TOK_UNQUOTE_SPLICING:
	  obj = SCM_LIST2(scm_sym_unquote_splicing, scm_read_port(port));
	  break;
	  
	case SCM_TOK_EVAL:
	  obj = scm_eval(scm_read_port(port), NULL);
	  break;

	default:
	  
	  fprintf(stderr, "scm_read_list: unreconized token %d\n", tok);
	  exit(1);

	}
	if (SCM_NNULLP(last)) {
	  SCM_CDR(last) = scm_cons(obj, NULL);
	  last = SCM_CDR(last);
	} else {
	  list = last = scm_cons(obj, NULL);
	}
  }
  return(list);
}

SOBJ scm_read_port(PORT *port)
{
  int tok;

  if (port == NULL) {  port = SCM_INP; }

  tok = scm_read_token(port);
  switch(tok) {
  case SCM_TOK_EOF:		return(scm_eof);
  case SCM_TOK_LPAR:	return(scm_read_list(port));
	
  case SCM_TOK_NUM:
	{
	  SOBJ obj;
	  obj = scm_cstr2number(scm_tokbuf, 10);
	  if (obj != scm_false)	return(obj);
	}
	/* FALL THROUGH */
	
  case SCM_TOK_SYM:		return(scm_mkatom(scm_tokbuf));
  case SCM_TOK_BOOLEAN:	return(scm_tokbuf[0] == 'f' ? scm_false : scm_true);
  case SCM_TOK_RPAR:	SCM_ERR("scm_read: unexpected ')'", NULL);
  case SCM_TOK_DOT:		SCM_ERR("scm_read: unexpected '.'", NULL);
  case SCM_TOK_UDEF:	return(scm_tokval);

  case SCM_TOK_QUOTE:	return(SCM_LIST2(scm_sym_quote,  scm_read_port(port)));
  case SCM_TOK_QQUOTE:	return(SCM_LIST2(scm_sym_qquote, scm_read_port(port)));
  case SCM_TOK_UNQUOTE:	return(SCM_LIST2(scm_sym_unquote,scm_read_port(port)));

  case SCM_TOK_UNQUOTE_SPLICING:
	return(SCM_LIST2(scm_sym_unquote_splicing, scm_read_port(port)));

  case SCM_TOK_EVAL:	return(scm_eval(scm_read_port(port), NULL));
	
  default:
	SCM_ERR("scm_read: unreconized token", NULL);
  }
  return(NULL);
}

/*** print an error */
void scm_internal_err(char *cfunc, char *msg, SOBJ obj)
{
  PORT *p;
  SOBJ errstr;
  char *pfunc, *s;
  
  p = port_open_output_string();
  port_puts(p, "\nerror:");

  if (SCM_INP) {
	port_puts(p,"line="); port_putn(p, SCM_INP->line+1); port_puts(p, ":");
  }
  if (cfunc) {
	pfunc = alloca(strlen(cfunc)+1);
	if (strncmp(cfunc, "scm_", 4) == 0) { /* std preamble */
	  strcpy(pfunc, cfunc + 4);
	  for (s = pfunc; *s; s++) {
		if (*s == '_') *s = '-';
	  }
	  *s = 0;
	  if (s > pfunc && s[-1] == 'p')  s[-1] = '?';
	  port_puts(p, pfunc);  port_putc(p, ':');
	} else {
	  port_puts(p,"cfunc="); port_puts(p, cfunc); port_puts(p, ":");
	}
  }
  port_puts(p, " ");
  port_puts(p, msg);  port_puts(p, " ");  scm_write_obj(obj, p, FALSE);
  port_puts(p, "\n");

  if (scm_last_read_object != NULL) {
	port_puts(p, "  last read:");
	scm_write_obj(scm_last_read_object, p, FALSE);
	port_puts(p, "\n");
  }
  scm_last_read_object = NULL;
  errstr = scm_mkstring(port_string_output_string(p));
  port_close(p);
  scm_throw(scm_mkatom("error"), errstr);
}

#ifdef OLD
/*-- err */
void SCM_ERR(char *msg, SOBJ obj)
{
  PORT *p;
  SOBJ errstr;
  
  p = port_open_output_string();
  if (SCM_INP) {
	port_puts(p, "\nerror: line ");	port_putn(p, SCM_INP->line + 1);
	port_puts(p, ": ");
  }
  port_puts(p, msg);  port_puts(p, " ");  scm_write_obj(obj, p, FALSE);
  port_puts(p, "\n");

  if (scm_last_read_object != NULL) {
	port_puts(p, "  last read:");
	scm_write_obj(scm_last_read_object, p, FALSE);
	port_puts(p, "\n");
  }
  scm_last_read_object = NULL;
  errstr = scm_mkstring(port_string_output_string(p));
  port_close(p);
  scm_throw(scm_mkatom("error"), errstr);
}
#endif

SOBJ scm_error(SOBJ string, SOBJ object)
{
  if (!SCM_STRINGP(string)) SCM_ERR("error: bad string", string);
  SCM_ERR(SCM_STR_VALUE(string), object);
  return(NULL);					/* never returns */
}

SOBJ scm_execute(SOBJ code)
{
  static SOBJ vmcode_template[5];
  SOBJ result, vmcode[5];
	
  if (!SCM_CODEP(code))	SCM_ERR("execute: bad code", code);
  
  if (vmcode_template[0] == NULL) { /* initialize once */
	vmcode_template[0] = SCM_OPCODE(SCM_OP_MARK);
	vmcode_template[1] = SCM_OPCODE(SCM_OP_PUSH);
	vmcode_template[2] = NULL;
	vmcode_template[3] = SCM_OPCODE(SCM_OP_CALL);
	vmcode_template[4] = SCM_OPCODE(SCM_OP_END);
  }

  memcpy(vmcode, vmcode_template, sizeof(vmcode_template));
  vmcode[2] = code;
  result = scm_run_engine(vmcode);
  return(result);
}

SOBJ scm_eval(SOBJ expr, SOBJ env)
{
  return(scm_execute(scm_compile(expr, env)));
}

/*** apply family */

SOBJ scm_apply(SOBJ func, SOBJ argl)
{
  SOBJ *vmcode, *p;
  int i, argc;

  argc = scm_list_length(argl);
  if (argc < 0) SCM_ERR("bad list", argl);
  
  vmcode = alloca( sizeof(SOBJ) * (argc + 6));
  p = vmcode;
  *p++ = SCM_OPCODE(SCM_OP_MARK);
  *p++ = SCM_OPCODE(SCM_OP_PUSHN);
  *p++ = SCM_MKINUM(argc+1);
  i = argc - 1;
  while(argl) {
	p[i] = SCM_CAR(argl);
	argl = SCM_CDR(argl);
	i--;
  }
  p += argc;
  *p++ = func;
  *p++ = SCM_OPCODE(SCM_OP_CALL);
  *p++ = SCM_OPCODE(SCM_OP_END);

  return(scm_run_engine(vmcode));

}

SOBJ scm_apply_v(SOBJ func, int argc, SOBJ *argv)
{
  int i;
  SOBJ *p, *vmcode = alloca( sizeof(SOBJ) * (argc + 6));

  p = vmcode;
  *p++ = SCM_OPCODE(SCM_OP_MARK);
  *p++ = SCM_OPCODE(SCM_OP_PUSHN);
  *p++ = SCM_MKINUM(argc+1);
  i = argc;
  while(--i >= 0) {
	*p++ = argv[i];
  }
  *p++ = func;
  *p++ = SCM_OPCODE(SCM_OP_CALL);
  *p   = SCM_OPCODE(SCM_OP_END);
  return(scm_run_engine(vmcode));
}


SOBJ scm_apply0(SOBJ func)
{
  SOBJ vmcode[] = {
	SCM_OPCODE(SCM_OP_MARK),
	SCM_OPCODE(SCM_OP_PUSH),
	func,
	SCM_OPCODE(SCM_OP_CALL),
	SCM_OPCODE(SCM_OP_END)
  };
  return(scm_run_engine(vmcode));
}


SOBJ scm_apply1(SOBJ func, SOBJ a1)
{
  SOBJ vmcode[] = {
	SCM_OPCODE(SCM_OP_MARK),
	SCM_OPCODE(SCM_OP_PUSH),	a1,
	SCM_OPCODE(SCM_OP_PUSH),	func,
	SCM_OPCODE(SCM_OP_CALL),
	SCM_OPCODE(SCM_OP_END)
  };
  return(scm_run_engine(vmcode));
}

SOBJ scm_apply2(SOBJ func, SOBJ a1, SOBJ a2)
{
  static SOBJ vmtmpl[8];
  SOBJ vmcode[8];
  SCM_VMD *vmd = scm_vmd();
  
  if (vmtmpl[0] == NULL) {
	vmtmpl[0] = SCM_OPCODE(SCM_OP_MARK);
	vmtmpl[1] =	SCM_OPCODE(SCM_OP_PUSHN);
	vmtmpl[2] = SCM_MKINUM(3);
	/* index=3 : placeholder for a2 */
	/* index=4 : placeholder for a1 */
	/* index=5 : placeholder for func */
	vmtmpl[6] = SCM_OPCODE(SCM_OP_CALL);
	vmtmpl[7] = SCM_OPCODE(SCM_OP_END);
  }
  
  memcpy(vmcode, vmtmpl, sizeof(vmtmpl));
  vmcode[3] = a2;  vmcode[4] = a1;  vmcode[5] = func;

  vmd->code = SCM_VM_DO_EXECUTE;
  vmd->reg.ip = vmcode;
  scm_vm(vmd);
  return(vmd->ret.obj);

  return(scm_run_engine(vmcode));
}

SOBJ scm_apply3(SOBJ func, SOBJ a1, SOBJ a2, SOBJ a3)
{
  SOBJ vmcode[] = {
	SCM_OPCODE(SCM_OP_MARK),
	SCM_OPCODE(SCM_OP_PUSHN),	SCM_MKINUM(4),	a3, a2, a1, func,
	SCM_OPCODE(SCM_OP_CALL),
	SCM_OPCODE(SCM_OP_END)
  };
  return(scm_run_engine(vmcode));
}

/* try to open filename: if filename is not a path (contains /) and
 * open in current directory failed, try to open in directories
 * mentionned in the scm_qs_lib.
 * Returns a port on success or NULL on failure
 */
PORT *scm_load_try_open(SOBJ filename)
{
  PORT *port;
  char *fname, *path, *p, *q;
  int fname_len;

  fname = SCM_STR_VALUE(filename);

  if ((port = port_open_input_file(fname)) != NULL)
	return(port);

  /* absolute path => do not check for QS_LIB */
  if (strchr(fname, '/') != NULL || scm_qs_lib == NULL)
	return(NULL);

  /* for each path in qs lib, try to load */
  fname_len = strlen(fname);
  p = scm_qs_lib;
  q = scm_qs_lib;
  path = NULL;
  while(*p) {
	if ((q = strchr(p, ':')) == NULL) {	/* no more ':' */
	  q = p + strlen(p);
	}
	/* alloc space for path + / + filename + \0 */
	path = scm_must_alloc((q - p) + fname_len + 2);
	strncpy(path, p, (q-p));
	path[ (q - p) ] = '/';
	strcpy(path + (q - p) + 1, fname);
	scm_puts("; trying to load "); scm_puts(path); scm_puts("\n");
	port = port_open_input_file(path);
	scm_free(path);
	if (port != NULL) 	return(port);
	p = (*q != 0) ? q+1 : q;
  }
  return(NULL);
}


SOBJ scm_load(SOBJ filename)
{
  SOBJ obj, r;
  int k;
  jmp_buf   old_handler;
  PORT 		*old_input;

  old_input = NULL;

  if (!SCM_STRINGP(filename)) SCM_ERR("bad string", filename);

  /*-- save the old handler and install a new one */
  memcpy(old_handler, scm_errjmp, sizeof(jmp_buf));
  old_input   = SCM_INP;
  
  if ( (k = setjmp(scm_errjmp)) != 0) {
	fprintf(stderr, "; load aborted: status=%d\n", k);
	if (SCM_INP) port_close(SCM_INP);	
	SCM_INP = old_input;
	memcpy(scm_errjmp, old_handler, sizeof(jmp_buf));
	SCM_ERR("load aborted", filename);
  }

  if ((SCM_INP = scm_load_try_open(filename)) == NULL)
	SCM_ERR("cannot open", filename);

  k = port_getc(SCM_INP);

  /* if file starts with a ctrl char or a binary char, try to load-lib */
  if ((k < 32 && strchr("\t\n\r", k) == NULL) || (k & 0xff) > 126) {
	port_close(SCM_INP);
	SCM_INP = old_input;
	memcpy(scm_errjmp, old_handler, sizeof(jmp_buf));
	return(scm_load_library(filename));
  }

  port_seek(SCM_INP, 0);
  r = NULL;
  while( (scm_last_read_object = obj = scm_read_port(SCM_INP)) != scm_eof) {
	r = scm_execute(scm_compile(obj, NULL));
	/* scm_gc(); */
  }
  port_close(SCM_INP);
  SCM_INP  = old_input;
  memcpy(scm_errjmp, old_handler, sizeof(jmp_buf));
  if (scm_interractive) {
	scm_puts("; "); scm_cdisplay(filename); scm_puts(" ok\n");
  }
  return(r);
}

SOBJ scm_loadstr(char *str)
{
  return(scm_load(scm_mkstring(str)));
}


SOBJ scm_eval_string(SOBJ str)
{
  SOBJ obj, r;
  int k;
  jmp_buf   old_handler;
  PORT 		*old_input;

  old_input = NULL;

  if (!SCM_STRINGP(str)) SCM_ERR("bad string", str);

  /*-- save the old handler and install a new one */
  *old_handler = *scm_errjmp;
  old_input   = SCM_INP;
  
  if ( (k = setjmp(scm_errjmp)) != 0) {
	if (SCM_INP) port_close(SCM_INP);	
	SCM_INP = old_input;
	memcpy(scm_errjmp, old_handler, sizeof(jmp_buf));
	SCM_ERR("load aborted", str);
  }

  SCM_INP = port_open_input_string(SCM_STR_VALUE(str));
  if (SCM_INP == NULL) 	SCM_ERR("cannot open", str);
  r = NULL;
  while( (scm_last_read_object = obj = scm_read_port(SCM_INP)) != scm_eof) {
	r = scm_execute(scm_compile(obj, NULL));
	/* scm_gc(); */
  }
  port_close(SCM_INP);
  SCM_INP  = old_input;
  memcpy(scm_errjmp, old_handler, sizeof(jmp_buf));
  return(r);
}

SOBJ scm_evalstr(char *str)
{
  return(scm_eval_string(scm_mkstring(str)));
}

/********************************
 * interpreter
 ********************************/
void scm_add_cprim(char *name, SCM_CPRIM f, int n)
{
  SOBJ sym, prim;

  prim = scm_newcell(SOBJ_T_CPRIM);
  SCM_CPRIM_FUNC(prim) = f;  SCM_CPRIM_NARGS(prim) = n;

  sym = scm_symadd(name, prim);
}

void scm_add_vmfunc(char *name, void (*f)(SCM_vmRegisters *))
{
  SOBJ sym, vmfunc;

  vmfunc = scm_newcell(SOBJ_T_VMFUNC);
  SCM_VMFUNC(vmfunc) = f;

  sym = scm_symadd(name, vmfunc);
}

void scm_add_prim(char *name, SCM_PRIM_TABLE *entry)
{
  SOBJ sym, prim;
  prim = scm_newcell(SOBJ_T_PRIM);
  SCM_PRIM(prim) = entry;

  sym = scm_symadd(name, prim);
}

SOBJ scm_add_syntax(char *name, SCM_CPRIM f)
{
  SOBJ sym, prim;
  prim = scm_newcell(SOBJ_T_SYNTAX);
  SCM_SYNTAX_FUNC(prim) = f;

  sym = scm_symadd(name, prim);  
  return(sym);
}

void scm_syntax_print(SOBJ obj, PORT *p)
{
  port_puts(p, "#<syntax>");
}

/*-- add a new type */

int scm_add_type(SOBJ_TYPE_DESCR *type)
{
  int type_nr = scm_type_next_descr++;
  scm_type_hook[type_nr] = *type;

  /*-- force no call for this type */
  scm_type_hook[type_nr].execute = (long)SCM_OPCODE(SCM_OP_NO_CALL);
  
  return(type_nr);
}

/*-- search for an existing type : return NULL if not found */
SOBJ_TYPE_DESCR *scm_lookup_type(char *name)
{
  SOBJ_TYPE_DESCR *p = scm_type_hook;
  while(p->name) {
	if (streq(p->name, name)) return(p);
	p++;
  }
  return(NULL);
}


/*E* (internal-type-list) => LIST */
/*D* Return the list of the name of all known types. */
SOBJ scm_type_list()
{
  int i;
  SOBJ l = NULL;
  for (i = 0; i < scm_type_next_descr; i++) {
	l = scm_cons(scm_mkstring(scm_type_hook[i].name), l);
  }
  return(l);
}

/*-- default type functions */

static void scm_deflt_sweep(SOBJ x)
{
  printf("; gc : default sweeper for type '%s'\n",
		 scm_type_hook[SCM_OBJTYPE(x)].name);
}

static SOBJ scm_deflt_compare(SOBJ x, SOBJ y)
{
  return(SCM_AUX(x) == SCM_AUX(y) ? scm_true : scm_false);
}

static SOBJ scm_deflt2obj(int type, void *p)
{
  SOBJ new = scm_newcell(type);
  SCM_AUX(new) = p;
  return(new);
}

static void *scm_obj2deflt(SOBJ obj)
{
  return(SCM_AUX(obj));
}

static SOBJ_TYPE_DESCR scm_default_type_descr = {
  0,
  "undef",
  NULL,							/* code to mark */
  scm_deflt_sweep,				/* code to sweep */
  NULL,							/* howto print */
  NULL,							/* howto write */
  NULL,							/* howto convert to string */
  NULL,							/* char reconizer */
  NULL,							/* char parser */
  NULL,							/* word reconizer */
  NULL,							/* word parser */
  scm_deflt_compare,			/* type value comparison */
  scm_deflt2obj,				/* convert to obj */
  scm_obj2deflt,				/* convert to xxx */
};

/*E* (make-type NAME) => INT */
/*D* Creates a new internal type with name STR. Returns the type number. */
SOBJ scm_mktype(SOBJ name)
{
  if (!SCM_STRINGP(name)) SCM_ERR("bad type name", name);

  if (scm_lookup_type(SCM_STR_VALUE(name)))
	  SCM_ERR("type already defined", name);

  scm_default_type_descr.name = scm_must_strdup(SCM_STR_VALUE(name));
  return(SCM_MKINUM(scm_add_type(&scm_default_type_descr)));
}

/*E* (add-type-finalizer TYPE PROC) */
/*D* Add a procedure PROC to be called when an object of type TYPE is
  about to be destroyed. */
SOBJ scm_add_type_finalizer(SOBJ type, SOBJ proc)
{
  SOBJ_TYPE_DESCR *p;
  
  if (!SCM_STRINGP(type)) 	SCM_ERR("bad type", type);
  if (!SCM_CLOSUREP(proc)) 	SCM_ERR("bad procedure", proc);

  if ((p = scm_lookup_type(SCM_STR_VALUE(type))) == NULL)
	SCM_ERR("type does not exist", type);

  p->finalize = proc;
  scm_gc_protect(&p->finalize);
  return(scm_undefined);
}

/*E* (null-aux? OBJ) => BOOLEAN */
/*D* Returns #t if aux of OBJ is NULL, #f otherwise */

SOBJ scm_null_auxp(SOBJ obj)
{
  return(SCM_MKBOOL(SCM_AUX(obj) == NULL));
}

/*E* (set-aux! OBJ VAL) => NULL */
/*D* Associate a value to the aux pointer of object. THIS IS A
  DANGEROUS FUNCTION. */
SOBJ scm_set_aux(SOBJ obj, SOBJ val)
{
  SCM_AUX(obj) = val;
  return(NULL);
}

/*E* (get-aux OBJ) => AUX */
/*D* Returns the content of the aux field of OBJ. THIS IS A DANGEROUS
  FUNCTION. */
SOBJ scm_get_aux(SOBJ obj)
{
  return(SCM_AUX(obj));
}

/*E* (clear-aux! OBJ) */
/*D* Stores NULL in the aux field of OBJ. THIS IS A DANGEROUS
  FUNCTION. */
SOBJ scm_clear_aux(SOBJ obj)
{
  return(scm_set_aux(obj, NULL));
}

/****************************************************************
 * Initialisation stuff
 ****************************************************************/

/*-- init the scm_type_hook array:
 * set the address of the execute pointer to something real.
 */
void scm_init_type()
{
  scm_add_cprim("internal-type-list", 	scm_type_list, 			0);
  scm_add_cprim("make-type",		  	scm_mktype, 			1);
  scm_add_cprim("add-type-finalizer", 	scm_add_type_finalizer, 2);
  scm_add_cprim("null-aux?",			scm_null_auxp, 			1);
  scm_add_cprim("clear-aux!",			scm_clear_aux,  		1);
  scm_add_cprim("get-aux",				scm_get_aux, 			1);
  scm_add_cprim("set-aux!",				scm_set_aux, 			2);
}

/*E* (unbound) => #unbound */
/*D* Returns a reference to the #unbound object */
SOBJ scm_unbound_func()
{
  return(scm_unbound);
}

/*-- fill the scheme argv array */

void scm_set_argv(int argc, char **argv)
{
  int n, i;
  SOBJ scm_argv;
  
  n = 0;
  for (i = 0 ; i < argc; i++) if (argv[i] != NULL) n++;

  scm_argv = scm_mkarray(n, NULL);
  n = 0;
  for (i = 0; i < argc; i++) {
	if (argv[i]) {
	  SCM_AREF(scm_argv, n) = scm_mkstring(argv[i]);
	  n++;
	}
  }
  scm_symadd("argv", scm_argv);
}

/*-- Fill the scheme envhash hash */
void scm_set_environ()
{
  extern char **environ;
  char **p, *q, *tmp;
  SOBJ envhash;

  envhash = scm_mkhash(SCM_HASH_T_GEN);
  p = environ;
  while(*p) {
	tmp = strdup(*p++);
	if ((q = strchr(tmp, '=')) == NULL) {
	  fprintf(stderr, "Bad environement entry: %s\n", tmp);
	} else {
	  *q++ = 0;
	  scm_hash_set(envhash, scm_mkstring(tmp), scm_mkstring(q));
	  /*  scm_puts("; hash after adding:"); scm_cprint(envhash); */
	}
	free(tmp);
  }
  scm_symadd("environ", envhash);
}

/*-- the init function. does whatever is necessary to startup a new
 * scheme interpreter.
 *
 * NOTE: this function must be called only once
 */
void scm_init(int argc, char **argv)
{
  int stack_limit;
  static char *fload;			/* current file */
  char *p;
  
  /* remember the top stack position for gc. It's not expected to have
   * SOBJ reference behind this point */

  scm_cstack_limit = &stack_limit;

  /* init heap and stack */
  scm_heap_init(scm_default_hs*1024);

#ifdef SCM_WITH_THREADS
  scm_init_main_thread();
#endif
  scm_vmd_stack_alloc(scm_vmd(), scm_default_ds * 1024);
  
  /* intialize default ports */
  scm_port_init_default_files();

  /* capture important env variables */
  if ((p = getenv("QS_LIB")) == NULL) {
	scm_qs_lib = scm_must_alloc(strlen(SCM_DEFAULT_LIB_PATH) + 1);
	strcpy(scm_qs_lib, SCM_DEFAULT_LIB_PATH);
  } else {
	scm_qs_lib = scm_must_alloc(strlen(SCM_DEFAULT_LIB_PATH) +
								strlen(p) + 2);
	strcpy(scm_qs_lib, p);
	strcat(scm_qs_lib, ":");
	strcat(scm_qs_lib, SCM_DEFAULT_LIB_PATH);
  }
  /*  printf("; library search path=%s\n", scm_qs_lib); */

  /* init the rest */
  scm_init_symbol_hash();
  scm_init_atom();				/* prepare to hold atoms */

  scm_true = scm_newcell(SOBJ_T_BOOLEAN);	   	scm_gc_protect(&scm_true);
  scm_false= scm_newcell(SOBJ_T_BOOLEAN);	   	scm_gc_protect(&scm_false);
  scm_unbound= scm_newcell(SOBJ_T_UNBOUND);	   	scm_gc_protect(&scm_unbound);
  scm_undefined= scm_newcell(SOBJ_T_UNDEFINED);	scm_gc_protect(&scm_undefined);

  scm_sym_unquote = scm_symadd("unquote", scm_unbound);
  SCM_SYM_VALUE(scm_sym_unquote) = scm_sym_quote;

  scm_sym_unquote_splicing = scm_symadd("unquote-splicing", scm_unbound);
  SCM_SYM_VALUE(scm_sym_unquote_splicing) = scm_sym_unquote_splicing;

  scm_add_cprim("disasm",		scm_disasm,			1);
  scm_add_cprim("read",			scm_read,			0);
  scm_add_cprim("error",		scm_error,			2);

  scm_add_cprim("apply2",		scm_apply,			2);
  scm_add_cprim("unbound",		scm_unbound_func,	0);

  scm_init_port();
  scm_init_macro();
  scm_init_variable();			/* generic variable system */
  scm_init_symbol();			/* now init symbol and keyword module */
  scm_init_boolean();			/* boolean */
  scm_init_number();			/* numbers */
  scm_init_chr();				/* characters */
  scm_init_str();				/* strings */
  scm_init_list();				/* list */
  scm_init_array();				/* arrays */
  scm_init_hash();				/* hashes */
  scm_init_proc();				/* procedure stuff */
  scm_init_pointer();			/* generic pointer */
  scm_init_dyn();
  scm_init_catch();				/* catch/throw */
  scm_init_format();			/* c like format */
  scm_init_vm3();				/* init the vm3 engine */
  scm_engine_init();			/* init the vm engine */
#ifdef SCM_WITH_THREADS
  scm_init_thread();			/* init the thread type */
#endif
  scm_init_type();
  scm_init_module();			/* init the module system */
  scm_init_asm();				/* init the assembler */
  scm_init_misc();				/* init the misc system */
  scm_init_process();
  scm_init_file();

  if (argc > 0) {
	if (streq(argv[0], "--")) {
	  fload = NULL;
	} else {
	  fload = argv[0];
	}
	argv++; argc--;
  }
  /* build the argv array and the environ hash */
  scm_set_argv(argc, argv);
  scm_set_environ();

  /* set the prompt string */
  if (scm_prompt_str) free(scm_prompt_str);
  scm_prompt_str = scm_must_strdup("QScheme> ");

  if (!scm_no_init) {
	if (setjmp(scm_errjmp) == 0) {
	  scm_loadstr("s.scm");
	} else {
	  scm_puts("; load failed, trying to continue\n");
	}
  }
  if (fload != NULL) {
	if (setjmp(scm_errjmp) != 0) {
	  scm_puts("; loading of '"); scm_puts(fload); scm_puts("' failed\n");
	  exit(1);
	}
	scm_loadstr(fload);
  }
}

void scm_main_loop()
{
  SOBJ obj, code, result;
  SOBJ *oldsp;
  SCM_VMD *vm;
  int k;

  if ((k = setjmp(scm_errjmp)) != 0) {
	printf("TOPLEVEL restart. setjmp returned %d\n", k);
	scm_current_module = scm_global_module;
	fflush(stdout);
  }

  vm = scm_vmd();

  /*KKK: should have a new SCM_VMD alloc here */

  vm->reg.sp = vm->stack_limit; /* clear stack */

  while(1) {
	if ((vm->stack_limit - vm->reg.sp) != 0) {
	  fprintf(stderr, "ERROR: Stack depth=%d. clearing stack\n", 
			  vm->stack_limit - vm->reg.sp);
	  vm->reg.sp = vm->stack_limit; /* clear stack */
	}
	scm_gc();
	scm_puts(scm_prompt_str);
	oldsp = vm->reg.sp;

	scm_last_read_object = obj = scm_read_port(SCM_INP);
	if (obj == scm_eof) 	break;
	code = scm_compile(obj, NULL);
	result = scm_execute(code);
	scm_write_obj(result, SCM_OUTP, 0);		scm_putc('\n');
	if (vm->reg.sp != oldsp) {
	  printf("scm_repl: stack not empty: old=%p new=%p\n",
			 oldsp, vm->reg.sp);
	}
	vm->reg.sp = oldsp;
  }
}

void scm_announce()
{
  printf("; QScheme %s - A fast implementation of the Scheme language\n",
		 VERSION);
  printf("; Copyright (C) 1998-2000 Daniel Crettol <dan@sof.ch>\n");
  printf("; QScheme is distributed under the GNU General Public Licence.\n");
  printf("; See the COPYING and LICENCE_EXCEPTION files for more informations.\n");
#ifdef SCM_WITH_THREADS
  printf(";\n; Native threads enabled...\n");
#endif
  if (isatty(0)) printf(";\n; QScheme started on a tty...\n");
  printf(";\n");
}

void scm_usage()
{
  scm_announce();

  printf("Usage: %s [options] [file] [--] [arguments]\n", scm_progname);
  printf("\n");
  printf("Where options are:\n");
  printf("  -h   |--help          Print this help\n");
  printf("  -hs=N|--heap-size=N   Set heap size to N kilo cells (default=%ld)\n", scm_default_hs);
  printf("  -ds=N|--stack-size=N  Set data stack to N kilo ptrs (default=%ld)\n", scm_default_ds);
  printf("  -ni  |--no-init       Don't load initialisation files\n");
  printf("  -i   |--interractive  Always enter interractive loop\n");
  exit(1);
}


void scm_boot(int argc, char **argv)
{
  char *p, *q;
  int done;

  scm_interractive = TRUE;
  scm_force_interractive = FALSE;

  scm_progname = *argv++;  argc--;
  done = FALSE;
  while(argc > 0 && **argv == '-' && !done) {
	p = *argv++;
	argc--;
	while(*p) {
	  q = p;  while(!isspace(*q) && *q != ',' && *q != 0) q++;
	  *q = 0;
	  if (*p != '-') break;

	  if (streq(p, "-h") || streq(p, "--help")) {
		scm_usage();
	  } else if (strncmp(p, "-hs=", 4) == 0) {
		scm_default_hs = atol(p+4); 	
	  } else if (strncmp(p, "-ds=", 4) == 0) {
		scm_default_ds = atol(p+4);
	  } else if (strncmp(p, "--heap-size=", 12) == 0) {
		scm_default_hs = atol(p+4); 	
	  } else if (strncmp(p, "--stack-size=", 13) == 0) {
		scm_default_ds = atol(p+4); 	
	  } else if (streq(p, "-i") || streq(p, "--interractive")) {
		scm_force_interractive = TRUE;
	  } else if (streq(p, "-ni") || streq(p, "--no-init")) {
		scm_no_init = TRUE;
	  } else if (strncmp(p, "-*", 2) == 0) {
		break;				/* ignore everything following -* */
	  } else if (streq(p, "--")) {
		argv--; argc++;
		done = TRUE;
		break;				/* end of argument processing */
	  } else {
		scm_usage();
	  }
	  p = q;
	}
  }
  if (argc > 0) {
	if (access(*argv, R_OK) == 0) { /* seems to be valid */
	  scm_interractive = FALSE;
	}
  }
  if (scm_interractive) 
	scm_announce();

  scm_init(argc, argv);
  if (scm_interractive || scm_force_interractive) {
	scm_interractive = TRUE;
	scm_main_loop();
	putchar('\n');
  }

}
