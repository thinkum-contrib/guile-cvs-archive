/* -*- tab-width:4; -*- */
/*
 * Generic variable system: may inteface any type of Variable
 *
 * $Id$
 */
#include "s.h"

#define FETCH(t,ptr) 		(*((t *)(ptr)))
#define STORE(t,ptr,value) { *((t *)(ptr)) = value; }

#define VFETCH(t,var) 			FETCH(t, SCM_VAR_ADDR(var))
#define VSTORE(t,var,value)		STORE(t, SCM_VAR_ADDR(var), value)

/*** driver functions for various types of external variables:
 * This functions are used in the scm_var_hook array.
 */
extern SCM_VarAux scm_var_hook[]; /* default variable type table */

static SOBJ get_ext_char(SOBJ var, void *ptr) {
  return(scm_mkchar( (ptr) ? FETCH(char,ptr) : VFETCH(char,var) ));
}

static void set_ext_char(SOBJ var, void *ptr, SOBJ value) {
  if (!SCM_CHARP(value)) SCM_ERR("set_ext_char: bad char", value);

  if (ptr) { STORE(char, ptr, SCM_CHAR(value)); }
  else {	VSTORE(char, var, SCM_CHAR(value)); }
}

static SOBJ get_ext_byte(SOBJ var, void *ptr) {
  return(SCM_MKINUM((ptr)?FETCH(char,ptr):VFETCH(char, var)));
}

static void set_ext_byte(SOBJ var, void *ptr, SOBJ value) {
  if (ptr) { STORE(char, ptr, scm_number2long(value)); }
  else {	VSTORE(char, var, scm_number2long(value)); }
}

static SOBJ get_ext_short(SOBJ var, void *ptr) {
  return(SCM_MKINUM((ptr)?FETCH(short,ptr):VFETCH(short, var)));
}

static void set_ext_short(SOBJ var, void *ptr, SOBJ value) {
  if (ptr) { STORE(short, ptr, scm_number2long(value)); }
  else {	VSTORE(short, var, scm_number2long(value)); }
}

static SOBJ get_ext_long(SOBJ var, void *ptr) {
  return(scm_int2num((ptr)?FETCH(long,ptr):VFETCH(long, var)));
}

static void set_ext_long(SOBJ var, void *ptr, SOBJ value) {
  if (ptr) { STORE(long, ptr, scm_number2long(value)); }
  else {	VSTORE(long, var, scm_number2long(value)); }
}

static SOBJ get_ext_float(SOBJ var, void *ptr) {
  return(scm_mkfnum((ptr)?FETCH(float,ptr):VFETCH(float, var)));
}

static void set_ext_float(SOBJ var, void *ptr, SOBJ value) {
  if (ptr) { STORE(float, ptr, scm_number2double(value)); }
  else {	VSTORE(float, var, scm_number2double(value)); }
}

static SOBJ get_ext_double(SOBJ var, void *ptr) {
  return(scm_mkfnum((ptr)?FETCH(double,ptr):VFETCH(double, var)));
}

static void set_ext_double(SOBJ var, void *ptr, SOBJ value) {
  if (ptr) { STORE(double, var, scm_number2double(value)); }
  else {	VSTORE(double, var, scm_number2double(value)); }
}

static SOBJ get_ext_str(SOBJ var, void *ptr) {
  return(scm_mkstring((ptr)?FETCH(char*,ptr):VFETCH(char*,var)));
}

static void set_ext_str(SOBJ var, void *ptr, SOBJ value) {

  if (!SCM_STRINGP(value)) 	SCM_ERR("bad string", value);

  if (ptr) {
	free(ptr);					/* suppose we have not allocted it */
	STORE(char*, ptr, strdup(SCM_STR_VALUE(value)));
  } else {
	if (VFETCH(char *, var) != NULL) free(VFETCH(char *, var));
	VSTORE(char *, var, strdup(SCM_STR_VALUE(value)));
  }
}

static SOBJ get_ext_strbuf(SOBJ var, void *ptr) {
  return(scm_mkstring((ptr)?ptr:SCM_VAR_ADDR(var)));
}

static void set_ext_strbuf(SOBJ var, void *ptr, SOBJ value) {
  if (!SCM_STRINGP(value)) 	SCM_ERR("bad string", value);
  if (ptr) {
	strcpy(ptr, SCM_STR_VALUE(value));
  } else {
	strcpy((char*)SCM_VAR_ADDR(var), SCM_STR_VALUE(value));
  }
}

static SOBJ get_cscheme(SOBJ var, void *ptr) {
  return((ptr)?FETCH(SOBJ,ptr):VFETCH(SOBJ, var));
}

static void set_cscheme(SOBJ var, void *ptr, SOBJ value) {
  if (ptr) { STORE(SOBJ, ptr, value); }
  else {	VSTORE(SOBJ, var, value); }
}

/****************************************************************
 * Default know type of variables. This table may be refered by
 * SCM_VAR_AUX field of variable.
 *
 * A type of variable is a way to handle the addr value
 *
 ****************************************************************/
SCM_VarAux scm_var_hook[] = {
  { "cscheme",		NULL,	-1,		sizeof(SOBJ),	SCM_ALIGNOF(SOBJ),
	get_cscheme, 	NULL, 	set_cscheme,  		NULL
  },
  { "char",			NULL,	-1,		sizeof(char),	SCM_ALIGNOF(char),
	get_ext_char, 	NULL, 	set_ext_char, 		NULL
  },
  { "byte",			NULL,	-1,		sizeof(char),	SCM_ALIGNOF(char),
	get_ext_byte,	NULL,	set_ext_byte,		NULL
  },
  { "short", 		NULL,	-1,		sizeof(short),	SCM_ALIGNOF(short),
	get_ext_short, 	NULL, 	set_ext_short, 		NULL
  },
  { "int", 			NULL,	-1,		sizeof(int),	SCM_ALIGNOF(int),
	get_ext_long, 	NULL, 	set_ext_long,  		NULL
  },
  { "long", 		NULL,	-1,		sizeof(long),	SCM_ALIGNOF(long),
	get_ext_long, 	NULL, 	set_ext_long,  		NULL	
  },
  { "float", 		NULL,	-1,		sizeof(float),	SCM_ALIGNOF(float),
	get_ext_float, 	NULL, 	set_ext_float,  	NULL
  },
  { "double",		NULL,	-1,		sizeof(double),	SCM_ALIGNOF(double),
	get_ext_double, NULL, 	set_ext_double,  	NULL
  },
  { "string",  		NULL,	-1,		sizeof(char *),	SCM_ALIGNOF(char*),
	get_ext_str, 	NULL,	set_ext_str,   		NULL
  },
  { "string-buffer",NULL,	-1,		sizeof(char *),	SCM_ALIGNOF(char*),
	get_ext_strbuf, NULL, 	set_ext_strbuf,  	NULL,
  },
  { NULL }
};


/*** Get a var to value: if no ptr is given the SCM_VAR_ADDR content
 * is supposed to point to address of the value to get. */
SOBJ scm_var_get(SOBJ var, void *ptr)
{
  SCM_VarAux *va = SCM_VAR_AUX(var);
  void *iv;

  if (va->type >= 0) {			/* possibly converted by  */
	iv = (ptr) ? FETCH(void*, ptr) : VFETCH(void*, var);

	/* if scheme type and no conversion routine, return the pointer */
	if (scm_type_hook[va->type].ext2obj == NULL)
	  return(scm_mkpointer(iv));
	
	/* otherwise convert to scheme */
	return( (*scm_type_hook[va->type].ext2obj)(va->type, iv) );
  }
  /* convert according to scm_var_hook[] */
  return( (*va->get)(var, ptr) );
}

/*** Set a var to value: if no ptr is given the SCM_VAR_ADDR content
 * is supposed to point to address to set. */
SOBJ scm_var_set(SOBJ var, void *ptr, SOBJ value)
{
  SCM_VarAux *va = SCM_VAR_AUX(var);
  if (va->type >= 0) {
	if (ptr == NULL) ptr = SCM_VAR_ADDR(var);
	if (scm_type_hook[va->type].obj2ext != NULL) {
	  STORE(void *, ptr, (*scm_type_hook[va->type].obj2ext)(value));
	} else {
	  STORE(void *, ptr, SCM_POINTER(value));
	}
  } else {
	(*va->set)(var, ptr, value);
  }
  return(scm_undefined);
}

SCM_VarAux *scm_var_type_lookup(SOBJ atom)
{
  SCM_VarAux *va = scm_var_hook;
  while(va->name) {
	if (va->atom == atom) return(va);
	va++;
  }
  return(NULL);
}

static void scm_var_type_init()
{
  SCM_VarAux *va = scm_var_hook;
  while(va->name) {
	va->atom = scm_mkatom(va->name);
	va++;
  }
}

void scm_var_mark(SOBJ var)
{
}

void scm_var_sweep(SOBJ var)
{
  SCM_VarAux *aux;

  aux = SCM_VAR_AUX(var);
  if (aux) {
	/* if aux does not refer to scm_var_hook array, free it */
	if (!((void*)aux > (void*)scm_var_hook &&
		  (void*)aux <= (((void*)scm_var_hook) + sizeof(scm_var_hook)))) {
	  scm_free(aux);
	}
  }
  SCM_VAR_AUX(var) = NULL;
}

void scm_var_print(SOBJ x, PORT *p)
{
  port_puts(p, "#<var ");
  port_puts(p, SCM_VAR_AUX(x)->name);
  port_puts(p, ">");
}

void scm_var_write(SOBJ x, PORT *p)
{
  port_puts(p, "#<var ");
  port_puts(p, SCM_VAR_AUX(x)->name);
  port_puts(p, ">");
}

SOBJ scm_mkvar(SOBJ type, void *addr)
{
  SOBJ var;
  SCM_VarAux *aux;
  SOBJ_TYPE_DESCR *t;

  aux = scm_var_type_lookup(type);
  /* if not a system type, try to find scheme type */
  if (aux == NULL) {	
	t = scm_lookup_type(SCM_ATOM_NAME(type));
	if (t == NULL)
	  SCM_ERR("mkvar: bad type", type);

	aux = scm_must_alloc(sizeof(SCM_VarAux));
	aux->name = t->name;
	aux->atom = scm_mkatom(t->name);
	aux->size = sizeof(SOBJ);
	aux->align = SCM_ALIGNOF(SOBJ);

	/* Got it. Test if getter / setter are defined */
	if (t->ext2obj == NULL || t->obj2ext == NULL)
	  SCM_ERR("mkvar: type has no set|get", type);

	scm_puts("mkvar: scheme object's type: "); scm_cprint(aux->atom);
	aux->type = t - scm_type_hook;
  } else {
	aux->type = -1;
  }
  var = scm_newcell(SOBJ_T_VAR);
  SCM_VAR_AUX(var) = aux;
  SCM_VAR_ADDR(var) = addr;
  return(var);
}

/*E* (external-variable? OBJ) => BOOL */
/*D* Returns #t if OBJ is an external variable, #f otherwise */
SOBJ scm_external_variablep(SOBJ x)
{
  return(SCM_MKBOOL(SCM_VARP(x)));
}

/*E* (make-extern-variable LIB TYPE NAME) => EXTVAR */
/*D* Declare an externaly defined variable. LIB is the name of the
  dynamic library where the NAME symbol is defined. TYPE is a keyword
  describing the type of variable and can take this values: :char,
  :short, :int, :long, :float, :double, :string, :string-buffer and
  :cscheme. */
SOBJ scm_make_extern_var(SOBJ lib, SOBJ type, SOBJ name)
{
  SOBJ var;
  void *addr;

  if (!SCM_STRINGP(lib))   SCM_ERR("make-extern-var: bad lib",  lib);
  if (!SCM_KEYWORDP(type)) SCM_ERR("make-extern-var: bad type", type);
  if (!SCM_STRINGP(name))  SCM_ERR("make-extern-var: bad name", name);
  
  addr = scm_find_extsym(SCM_STR_VALUE(lib), SCM_STR_VALUE(name), TRUE);
  var = scm_mkvar(SCM_KEYW_NAME(type), addr);
  return(var);
}

void scm_add_cvar(char *name, SOBJ *addr)
{
  SOBJ var = scm_mkvar(scm_mkatom("cscheme"), addr);
  scm_symadd(name, var);
  scm_gc_protect(addr);
}

/*E* atom-hash => HASH */
/*D* This hash contains the list of all atoms. */
/*E* symbol-hash => HASH */
/*D* Contains the list of all symbols */
/*E* keyword-hash => HASH */
/*D* Contains the list of all keywords. Keywords are stored without
  prefix or suffixes. */

/*E* stdin-port => PORT */
/*E* stdout-port => PORT */
/*E* stderr-port => PORT */

void scm_init_variable()
{
  scm_var_type_init();
  scm_add_cprim("make-extern-variable",	&scm_make_extern_var, 3);

  scm_add_cprim("extern-variable?",	scm_external_variablep, 1);

  /* init system variable. it helps for initialisation sequence */
  scm_add_cvar("atom-hash", 	&scm_atom_hash);
  scm_add_cvar("symbol-hash", 	&scm_symbol_hash);
  scm_add_cvar("keyword-hash", 	&scm_keyword_hash);

  scm_add_cvar("stdin-port",	&scm_in_port);
  scm_add_cvar("stdout-port",	&scm_out_port);
  scm_add_cvar("stderr-port",	&scm_err_port);
}
