/* -*- tab-width:4; -*- */
/*
 * Dynamic loading and ffi
 */

#include "s.h"
#include <dlfcn.h>
#include <avcall.h>

/*-- FFI type marker
 *
 * Note: keywords passed for types: don't need to be gc protected
 * because all keywords are referenced in the keyword hash. They never
 * move or will be removed.
 */

static SOBJ
  EXT_KEYW_ANY,
  EXT_KEYW_VOID,
  EXT_KEYW_CHAR,
  EXT_KEYW_SHORT,
  EXT_KEYW_USHORT,
  EXT_KEYW_INT,
  EXT_KEYW_UINT,
  EXT_KEYW_LONG,
  EXT_KEYW_ULONG,
  EXT_KEYW_FLOAT,
  EXT_KEYW_DOUBLE,
  EXT_KEYW_STATIC_PTR,
  EXT_KEYW_ITEM,				/* alias for static-pointer */
  EXT_KEYW_DYNAMIC_PTR,
  EXT_KEYW_STRING,
  EXT_KEYW_BOOLEAN;

/* This value is stored in the ExtFunc structure. Conversion between
   keyword and type is performed in the make_ext_func */

enum ExtTypes {
  EXT_T_VOID 		= -1,
  EXT_T_CHAR 		= -2,
  EXT_T_SHORT		= -3,
  EXT_T_USHORT		= -4,
  EXT_T_INT			= -5,
  EXT_T_UINT		= -6,
  EXT_T_LONG		= -7,
  EXT_T_ULONG		= -8,
  EXT_T_FLOAT		= -9,
  EXT_T_DOUBLE		= -10,
  EXT_T_STATIC_PTR	= -11,
  EXT_T_DYNAMIC_PTR	= -12,
  EXT_T_STRING		= -13,
  EXT_T_BOOLEAN		= -14,
  EXT_T_ERROR 		= -20,
  EXT_T_ANY 		= -21,
  EXT_T_MAX
};

/* table used to convert keyword to type */

struct ExtKeywType {
  SOBJ 	*atom;
  short type;
};

static struct ExtKeywType keyw2type_ref[] = {
  { &EXT_KEYW_ANY,  		EXT_T_ANY},
  { &EXT_KEYW_VOID,  		EXT_T_VOID},
  { &EXT_KEYW_CHAR,  		EXT_T_CHAR},
  { &EXT_KEYW_SHORT, 		EXT_T_SHORT},
  { &EXT_KEYW_USHORT,  		EXT_T_USHORT},
  { &EXT_KEYW_INT,  		EXT_T_INT},
  { &EXT_KEYW_UINT,  		EXT_T_UINT},
  { &EXT_KEYW_LONG,  		EXT_T_LONG},
  { &EXT_KEYW_ULONG,  		EXT_T_ULONG},
  { &EXT_KEYW_FLOAT,  		EXT_T_FLOAT},
  { &EXT_KEYW_DOUBLE,  		EXT_T_DOUBLE},
  { &EXT_KEYW_STATIC_PTR,  	EXT_T_STATIC_PTR},
  { &EXT_KEYW_ITEM,			EXT_T_STATIC_PTR},
  { &EXT_KEYW_DYNAMIC_PTR,  EXT_T_DYNAMIC_PTR},
  { &EXT_KEYW_STRING,  		EXT_T_STRING},
  { &EXT_KEYW_BOOLEAN,  	EXT_T_BOOLEAN},
  { NULL },
};

/*-- convert the keyword to an external type */
static int keyword_to_ext_type(SOBJ keyw)
{
  int i;
  struct ExtKeywType *k = keyw2type_ref;
  char *keywstr;
  
  while(k->atom) {
	if (*k->atom == SCM_KEYW_NAME(keyw))
	  return(k->type);
	k++;
  }

  keywstr = SCM_ATOM_NAME(SCM_KEYW_NAME(keyw));
  for (i = 0; i < scm_type_next_descr; i++) {
	if (streq(keywstr, scm_type_hook[i].name)) return(i);
  }

  return(EXT_T_ERROR);
}

#define DLFLAGS	(RTLD_LAZY|RTLD_GLOBAL)

static SOBJ scm_dl_list;

void *scm_find_extsym(char *path, char *sym_name, int must)
{
  void *handle, *sym;
  SOBJ l, str;

  str = scm_mkstring(path);
  
  if (scm_dl_list == NULL) 	SCM_ERR("dl_list not initialized", NULL);
  if ((l = scm_member(str, scm_dl_list)) != scm_false) {
	handle = SCM_POINTER(SCM_CAR(SCM_CDR(l)));
  } else {
	if ((handle=dlopen(path, DLFLAGS)) == NULL) SCM_ERR(dlerror(), str);
	scm_dl_list = scm_cons(str,
						   scm_cons(scm_mk_static_pointer(handle),
									scm_dl_list));
  }
  sym = dlsym(handle, sym_name);

  if (must && sym == NULL) {
	SCM_ERR("find-func: error: ", scm_mkstring(dlerror()));
  }
  return(sym);
}

static void load_and_call(char *path, char *sym_name)
{
  void (*init_func)();
  SOBJ str = scm_mkstring(path);

  if ((init_func = scm_find_extsym("", sym_name, FALSE)) != NULL) {
	scm_puts("; load-and-call: module '"); scm_cdisplay(str);
	scm_puts("' already (statically) loaded\n");
  }
  if (scm_member(str, scm_dl_list) != scm_false) {
	scm_puts("; load-and-call: module '"); scm_cdisplay(str);
	scm_puts("' already (dynamically) loaded\n");
  }

  init_func = scm_find_extsym(path, sym_name, TRUE);
  (*init_func)();
}

#define INIT_FUNC_PREFIX		"scm_init_"
#define INIT_FUNC_PREFIX_LEN	strlen(INIT_FUNC_PREFIX)

#define INIT_SYM_NAME_MAX		64


static void load_library(char *path)
{
  char init_sym_name[INIT_SYM_NAME_MAX];
  char *p, *q;
  int len;
  
  p = strrchr(path, '/');
  if (p == NULL)				/* no / => current path */
	p = path;
  else 
	p++;
  
  q = strchr(p, '.');			/* search for possible suffix */
  if (q == NULL)				/* no suffix ? set q to end of string */
	q = p + strlen(p);

  len = INIT_FUNC_PREFIX_LEN + q-p;
  if (len >= (INIT_SYM_NAME_MAX-1))
	SCM_ERR("load-library: init symbol too long", NULL);

  strcpy(init_sym_name, INIT_FUNC_PREFIX);
  strncpy(init_sym_name + INIT_FUNC_PREFIX_LEN, p, q-p);
  init_sym_name[len] = 0;
  load_and_call(path, init_sym_name);
}

SOBJ scm_mk_static_pointer(void *p)
{
  SOBJ new = scm_mkpointer(p);
  return(new);
}

SOBJ scm_mk_dynamic_pointer(void *p)
{
  SOBJ new = scm_mkpointer(p);
  SCM_POINTER_ATTRIB(new) = SCM_POINTER_FLAG_ALLOCED;
  return(new);
}

/*E* (load-library NAME) => #t */
/*D* Load a dynamic library and link it to the current scheme
  interpreter. Functions and variables of this library can be accessed
  with 'make-extfunc' and 'make-extvar' */

SOBJ scm_load_library(SOBJ x)
{
  if (!SCM_STRINGP(x)) SCM_ERR("load-library: bad string", x);
  load_library(SCM_STR_VALUE(x));
  return(scm_true);
}

/****************************************************************
 * FFI
 ****************************************************************/

/*-- gc function for the extfunc */

void scm_extfunc_mark(SOBJ extfunc)
{
}

void scm_extfunc_sweep(SOBJ extfunc)
{
  if (SCM_EXTFUNC(extfunc)) {
	scm_free(SCM_EXTFUNC(extfunc));
  }
  SCM_EXTFUNC(extfunc) = NULL;
}

SOBJ scm_mkextfunc(SCM_ExtFunc *f)
{
  SOBJ obj = scm_newcell(SOBJ_T_EXTFUNC);
  SCM_EXTFUNC(obj) = f;
  return(obj);
}

void scm_extfunc_print(SOBJ x, PORT *p)
{
  port_puts(p, "#<extfunc>");
}

void scm_extfunc_write(SOBJ x, PORT *p)
{
  char buf[128];
  sprintf(buf, "#<extfunc %p: ret=%d func=%p nargs=%d optargs=%d>",
		  x,
		  SCM_EXTFUNC(x)->return_t,
		  SCM_EXTFUNC(x)->func,
		  SCM_EXTFUNC(x)->argc,
		  SCM_EXTFUNC(x)->vararg);
  port_puts(p, buf);
}

SCM_STRBUF *scm_extfunc2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  if (raw) 	return(scm_strbuf_concat_sprintf(sb, "#<extfunc>"));
  return(scm_strbuf_concat_sprintf(sb, "#<extfunc %p: ret=%d func=%p nargs=%d optargs=%d>",
					  obj,
					  SCM_EXTFUNC(obj)->return_t,
					  SCM_EXTFUNC(obj)->func,
					  SCM_EXTFUNC(obj)->argc,
					  SCM_EXTFUNC(obj)->vararg));
}


static SCM_ExtFunc *scm_new_extfunc()
{
  return(scm_must_alloc(sizeof(SCM_ExtFunc)));
}

/*-- create an external function object.
 *
 * Syntax: (make-external-func <lib> <ret-type> <name> <arglist> )
 * Example: (make-external-function "" 0 'printf '(:string . :any))
 */
static void errext(char *string, SOBJ x)
{ 
  char buf[128];
  sprintf(buf, "make-external-function: %s", string);
  SCM_ERR(buf, x);
}

static void errcall(char *string, SOBJ x)
{ 
  char buf[128];
  sprintf(buf, "call-external-function: %s", string);
  SCM_ERR(buf, x);
}

/*E* (make-extfunc LIB RET NAME '(ARG...)) => EXTFUNC */
/*D* Create a new external function that can be called just like if
  it's a native scheme procedure. RET and ARG are keyword reprenting
  the type of return and argument. NAME is the name of the function as
  defined in the symbol table of the dynamic library LIB. */

/*X* (define printf (make-extfunc "" :int "printf" '(:string . :any))) */

SOBJ scm_make_extfunc(SOBJ lib, SOBJ ret, SOBJ name, SOBJ argl)
{
  SCM_ExtFunc *f;
  void *func;
  int iret = 0, itype = 0;

  if (!SCM_STRINGP(lib))	errext("bad library name", lib);
  if (!SCM_STRINGP(name))	errext("bad function name", name);
  if (!SCM_KEYWORDP(ret) ||
	  (iret = keyword_to_ext_type(ret)) == EXT_T_ERROR)
	errext("bad return type", ret);

  if (iret >= 0) {				/* check if internal type has converters */
	if (scm_type_hook[iret].ext2obj == NULL ||
		scm_type_hook[iret].obj2ext == NULL) {
	  errext("internal type has no caster", ret);
	}
  }

  func = scm_find_extsym(SCM_STR_VALUE(lib), SCM_STR_VALUE(name), FALSE);
  if (func == NULL) 		errext("function not found", scm_cons(lib, name));
  
  f = scm_new_extfunc();
  f->func = func;
  f->return_t = iret;
  f->argc = 0;

  while(argl) {
	if (!SCM_PAIRP(argl)) {
	  f->vararg = 1;
	  break;
	}
	if (!SCM_KEYWORDP(SCM_CAR(argl)) ||
		(itype = keyword_to_ext_type(SCM_CAR(argl))) == EXT_T_ERROR) 
	  errext("bad argument type", SCM_CAR(argl));
	
	if (itype >= 0) {			/* check if internal type has converters */
	  if (scm_type_hook[itype].ext2obj == NULL ||
		  scm_type_hook[itype].obj2ext == NULL) {
		errext("internal type has no caster", SCM_CAR(argl));
	  }
	}

	f->arg_t[f->argc++] = itype;
	argl = SCM_CDR(argl);
  }
  return(scm_mkextfunc(f));
}

static void pushargs(av_alist *al, SCM_ExtFunc *f, int nargs, SOBJ *arg)
{
  int i, type;
  SOBJ obj;

  if (f->vararg) {
	if (nargs < f->argc) 	errcall("not enough args", NULL);
  } else {
	if (nargs != f->argc)	errcall("bad number of args", NULL);
  }

  for (i = 0; i < f->argc; i++) {
	obj = arg[i];
	type = f->arg_t[i];
	switch(type) {
	case EXT_T_VOID:	errcall("void argument not allowed", NULL);
	case EXT_T_CHAR:
	case EXT_T_SHORT:	case EXT_T_USHORT:
	case EXT_T_INT:		case EXT_T_UINT:
	case EXT_T_LONG:	case EXT_T_ULONG:
	  if (SCM_CHARP(obj))  { av_long(*al, SCM_CHAR(obj));  			break;}
	  if (SCM_NUMBERP(obj)){ av_long(*al, scm_number2long(obj)); 	break;}
	  if (SCM_BOOLEANP(obj)){av_long(*al, obj!=scm_false);			break;}
	  errcall("bad integer", obj);

	case EXT_T_FLOAT:
	  if (SCM_NUMBERP(obj)){ av_float(*al, scm_number2double(obj)); break;}
	  errcall("bad float", obj);

	case EXT_T_DOUBLE:
	  if (SCM_NUMBERP(obj)){ av_double(*al, scm_number2double(obj)); break;}
	  errcall("bad double", obj);

	case EXT_T_STATIC_PTR:	case EXT_T_DYNAMIC_PTR:	case EXT_T_STRING:
	  if (obj == NULL)      	{ av_ptr(*al,void*,obj); 				break;}
	  if (SCM_POINTERP(obj)) 	{ av_ptr(*al,void*,SCM_POINTER(obj)); 	break;}
	  if (SCM_STRINGP(obj))  	{ av_ptr(*al,void*,SCM_STR_VALUE(obj));	break;}
	  errcall("bad pointer", obj);
	  
	case EXT_T_BOOLEAN:
	  av_long(*al, obj != scm_false);
	  break;

	default:
	  if (type < 0)
		errcall("bad argument type", SCM_MKINUM(type));

	  if (scm_type_hook[type].obj2ext == NULL)
		errcall("can't convert", scm_mkstring(scm_type_hook[type].name));
	  
	  av_ptr(*al,void*,(*scm_type_hook[type].obj2ext)(obj));
	  
	}
  }

  /* for variable arguments, try to guess type */
  while(i < nargs) {
	obj = arg[i];
	type = SCM_OBJTYPE(obj);
	switch(type) {
	case SOBJ_T_INUM:
	case SOBJ_T_BNUM:	av_long(*al,   scm_number2long(obj));	break;
	case SOBJ_T_FNUM:	av_double(*al, scm_number2double(obj)); break;
	case SOBJ_T_CHAR:	av_char(*al,   SCM_CHAR(obj));			break;
	case SOBJ_T_STRING:	av_ptr(*al, void*, SCM_STR_VALUE(obj));	break;
	case SOBJ_T_SYMBOL:	av_ptr(*al, void*, SCM_SYM_NAME(obj));	break;
	case SOBJ_T_POINTER:av_ptr(*al, void*, SCM_POINTER(obj));	break;
	default:
	  if (obj == NULL) {
		av_ptr(*al, void*, NULL);
	  } else if (scm_type_hook[type].obj2ext != NULL) {
		av_ptr(*al, void*, (*scm_type_hook[type].obj2ext)(obj));
	  } else {
		errcall("don't know how to convert", obj);
	  }
	}
	i++;
  }
}


SOBJ scm_extfunc_call(SOBJ proc, int nargs, SOBJ *arg)
{
  av_alist alist;
  SCM_ExtFunc *f = SCM_EXTFUNC(proc);
  int type;

  type = f->return_t;

  switch(type) {
  case EXT_T_VOID:
	av_start_void(alist, f->func);
	pushargs(&alist, f, nargs, arg);
	av_call(alist);
	return(scm_undefined);

  case EXT_T_CHAR:
	{
	  char result;
	  av_start_char(alist, f->func, &result);
	  pushargs(&alist, f, nargs, arg);
	  av_call(alist);
	  return(scm_int2num(result));
	}
  case EXT_T_SHORT:
	{
	  short result;
	  av_start_short(alist, f->func, &result);
	  pushargs(&alist, f, nargs, arg);
	  av_call(alist);
	  return(scm_int2num(result));
	}
  case EXT_T_INT:
	{
	  int result;
	  av_start_int(alist, f->func, &result);
	  pushargs(&alist, f, nargs, arg);
	  av_call(alist);
	  return(scm_int2num(result));
	}
  case EXT_T_LONG:
	{
	  long result;
	  av_start_long(alist, f->func, &result);
	  pushargs(&alist, f, nargs, arg);
	  av_call(alist);
	  return(scm_int2num(result));
	}
  case EXT_T_DOUBLE:
	{
	  double result;
	  av_start_double(alist, f->func, &result);
	  pushargs(&alist, f, nargs, arg);
	  av_call(alist);
	  return(scm_mkfnum(result));
	}
  case EXT_T_STATIC_PTR:	case EXT_T_DYNAMIC_PTR:
	{
	  void *p;

	  av_start_ptr(alist, f->func, void *, &p);
	  pushargs(&alist, f, nargs, arg);
	  av_call(alist);
	  return( type == EXT_T_STATIC_PTR ?
			  scm_mk_static_pointer(p) :
			  scm_mk_dynamic_pointer(p) );
	}

  case EXT_T_STRING:
	{
	  void *p;
	  av_start_ptr(alist, f->func, void *, &p);
	  pushargs(&alist, f, nargs, arg);
	  av_call(alist);
	  return((p != NULL) ? scm_mkstring(p) : NULL);
	}
	
  case EXT_T_BOOLEAN:
	{
	  int result;
	  av_start_int(alist, f->func, &result);
	  pushargs(&alist, f, nargs, arg);
	  av_call(alist);
	  return(SCM_MKBOOL(result));
	}

  default:
	{
	  void *p;
	  
	  if (type < 0) 				/* ext type */
		errcall("return type not supported", SCM_MKINUM(type));

	  if (scm_type_hook[type].ext2obj == NULL) 
		errcall("return cannot be converted",
				scm_mkstring(scm_type_hook[type].name));

	  av_start_ptr(alist, f->func, void *, &p);
	  pushargs(&alist, f, nargs, arg);
	  av_call(alist);
	  return( (*scm_type_hook[type].ext2obj)(type, p) );
	}
  }
  return(NULL);
}
					  

/*E* (external-exists? LIB NAME) => BOOLEAN */
/*D* Return #t if symbol NAME is defined in the dynamic library LIB. */
SOBJ scm_external_existsp(SOBJ lib, SOBJ entry)
{
  if (!SCM_STRINGP(entry)) 	SCM_ERR("external-exists?: bad string", entry);
  if (!SCM_STRINGP(lib)) 	SCM_ERR("external-exists?: bad string", lib);

  return(SCM_MKBOOL(scm_find_extsym(SCM_STR_VALUE(lib),
							  SCM_STR_VALUE(entry), FALSE)));
}


static void push_arg(av_alist *al, int type, SOBJ arg)
{
  switch(type) {
  case EXT_T_VOID:	SCM_ERR("external-call: cannot push void arg", arg);

  case EXT_T_CHAR:
  case EXT_T_SHORT:		case EXT_T_USHORT:
  case EXT_T_INT:		case EXT_T_UINT:
  case EXT_T_LONG:		case EXT_T_ULONG:
	if (SCM_CHARP(arg)) 	{ av_long(*al, SCM_CHAR(arg)); break; }
	if (SCM_NUMBERP(arg)) 	{ av_long(*al, scm_number2long(arg)); break; }
	SCM_ERR("external-call: bad integer", arg);
	  
  case EXT_T_FLOAT:
	if (SCM_CHARP(arg)) 	{ av_float(*al, SCM_CHAR(arg)); break; }
	if (SCM_NUMBERP(arg)) 	{ av_float(*al, scm_number2double(arg)); break; }
	SCM_ERR("external-call: bad float", arg);

  case EXT_T_DOUBLE:
	if (SCM_CHARP(arg)) 	{ av_double(*al,SCM_CHAR(arg)); break; }
	if (SCM_NUMBERP(arg)) 	{ av_double(*al,scm_number2double(arg)); break; }
	SCM_ERR("external-call: bad double", arg);

  case EXT_T_STATIC_PTR:	case EXT_T_DYNAMIC_PTR:	case EXT_T_STRING:
	if (SCM_POINTERP(arg)) 	{ av_ptr(*al, void*, SCM_POINTER(arg)); break;}
	if (SCM_STRINGP(arg))  	{ av_ptr(*al, void*, SCM_STR_VALUE(arg));break;}
	SCM_ERR("external-call: bad pointer", arg);

  case EXT_T_BOOLEAN:
	if (SCM_BOOLEANP(arg)) 	{ av_long(*al, (arg != scm_false)); break; }
	if (SCM_NUMBERP(arg)) 	{ av_long(*al, scm_number2long(arg)); break; }
	SCM_ERR("external-call: bad boolean", arg);
  default:
	SCM_ERR("external-call: bad argument type", arg);
  }
}

static void push_list(av_alist *al, SOBJ list)
{
  SOBJ arg;
  while(list) {
	arg = SCM_CAR(list);
	switch(SCM_OBJTYPE(arg)) {
	case SOBJ_T_INUM:
	case SOBJ_T_BNUM:		av_long(*al, scm_number2long(arg));	  		break;
	case SOBJ_T_FNUM:		av_double(*al, scm_number2double(arg));  	break;
	case SOBJ_T_CHAR:		av_char(*al, SCM_CHAR(arg));			 	break;
	case SOBJ_T_STRING:		av_ptr(*al, void*, SCM_STR_VALUE(arg));		break;
	case SOBJ_T_SYMBOL:		av_ptr(*al, void*, SCM_SYM_NAME(arg));		break;
	case SOBJ_T_POINTER: 	av_ptr(*al, void*, SCM_POINTER(arg));		break;
	default:
	  SCM_ERR("push_list: don't know how to convert", arg);
	}
	list = SCM_CDR(list);
  }
}

static void push_args(av_alist *al, SOBJ argtype, SOBJ argval)
{
  /* prepare arguments */
  while(argtype) {
	if (!SCM_PAIRP(argtype)) {	/* have a rest */
	  push_list(al, argval);
	  break;
	}
	if (argval == NULL)	SCM_ERR("call-external: not enough arguments", argtype);

	if (!SCM_INUMP(SCM_CAR(argtype)))
	  SCM_ERR("call-external: argtype is not a number", SCM_CAR(argtype));

	push_arg(al, SCM_INUM(SCM_CAR(argtype)), SCM_CAR(argval));
	argtype = SCM_CDR(argtype);
	argval  = SCM_CDR(argval);
  }
  
}

/*E* (call-external LIB RET NAME '(TYPE ...) (ARG ...)) */
/*D* Directly call a library function. Argument are converted
  according to type and the value returned is converted to scheme
  value as specified by the RET keyword */

SOBJ scm_external_call(SOBJ lib, SOBJ ret, SOBJ entry, 
					   SOBJ argtype, SOBJ argval)
{
  av_alist alist;
  void *func;

  /*-- type checking */
  if (!SCM_STRINGP(lib))	SCM_ERR("external-call: bad library name", lib);
  if (!SCM_STRINGP(entry))	SCM_ERR("external-call: bad entry name", entry);
  if (!SCM_INUMP(ret))		SCM_ERR("external-call: bad return type", ret);

  func = scm_find_extsym(SCM_STR_VALUE(lib), SCM_STR_VALUE(entry), FALSE);
  if (func == NULL)
	SCM_ERR("external-call: cannot find func", scm_cons(lib, entry));

  switch(SCM_INUM(ret)) {
  case EXT_T_VOID:
	av_start_void(alist, func);
	push_args(&alist, argtype, argval);
	av_call(alist);
	return(scm_undefined);

  case EXT_T_INT:
	{
	  int result;
	  av_start_int(alist, func, &result);
	  push_args(&alist, argtype, argval);
	  av_call(alist);
	  return(scm_int2num(result));
	}

  case EXT_T_DOUBLE:
	{
	  double result;
	  av_start_double(alist, func, &result);
	  push_args(&alist, argtype, argval);
	  av_call(alist);
	  return(scm_mkfnum(result));
	}

  default:
	SCM_ERR("external-call: unsupported return type", ret);
  }
  return(scm_undefined);
}

/*E* (make-external-pointer LIB NAME) => POINTER */

SOBJ scm_make_extptr(SOBJ lib, SOBJ name)
{
  if (!SCM_STRINGP(lib))	errext("bad library name", lib);
  if (!SCM_STRINGP(name))	errext("bad symbol name", name);

  return(scm_mkpointer(scm_find_extsym(SCM_STR_VALUE(lib),
									   SCM_STR_VALUE(name),
									   FALSE)));
}

/*-- initialize this module */

void scm_init_dyn()
{
  void *handle = dlopen(NULL, DLFLAGS);
  if (handle == NULL) SCM_ERR("dyn: cannot init", scm_mkstring(dlerror()));

  scm_dl_list = SCM_LIST2(scm_mkstring(""), scm_mk_static_pointer(handle));
  scm_add_cvar("library-list", &scm_dl_list);

  EXT_KEYW_ANY			= scm_mkatom("any");
  EXT_KEYW_VOID			= scm_mkatom("void");
  EXT_KEYW_CHAR			= scm_mkatom("char");
  EXT_KEYW_SHORT		= scm_mkatom("short");
  EXT_KEYW_USHORT		= scm_mkatom("ushort");
  EXT_KEYW_INT			= scm_mkatom("int");
  EXT_KEYW_UINT			= scm_mkatom("uint");
  EXT_KEYW_LONG			= scm_mkatom("long");
  EXT_KEYW_ULONG		= scm_mkatom("ulong");
  EXT_KEYW_FLOAT		= scm_mkatom("float");
  EXT_KEYW_DOUBLE		= scm_mkatom("double");
  EXT_KEYW_STATIC_PTR	= scm_mkatom("pointer");
  EXT_KEYW_ITEM			= scm_mkatom("item");
  EXT_KEYW_DYNAMIC_PTR	= scm_mkatom("dynamic-ptr");
  EXT_KEYW_STRING		= scm_mkatom("string");
  EXT_KEYW_BOOLEAN 		= scm_mkatom("boolean");

  scm_add_cprim("make-extfunc",		scm_make_extfunc, 4);
  scm_add_cprim("load-library", 	scm_load_library, 1);
  scm_add_cprim("external-exists?", scm_external_existsp, 2);
  scm_add_cprim("external-call", 	scm_external_call, 5);
  scm_add_cprim("make-external-pointer",	scm_make_extptr, 2);
}

