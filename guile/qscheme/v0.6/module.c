/* -*- tab-width:4; -*- */
/*
 * Scheme modules
 */
#include "s.h"
#include "vm2.h"
#include "stack.h"

SOBJ scm_global_module;			/* the global module */
SOBJ scm_current_module;		/* the current module */


/*E* module-hash => HASH */
/*D* contains the reference to all defined modules */

SOBJ scm_module_hash;			/* contains ref to all modules */


/****************************************************************
 * Type driver functions 
 ****************************************************************/
void scm_module_mark(SOBJ obj)
{
  SCM_Module *m = SCM_MODULE(obj);
  if (m->name) { 		scm_gc_mark(m->name); }
  if (m->symbols) { 	scm_gc_mark(m->symbols); }
  if (m->exports) { 	scm_gc_mark(m->exports); }
  if (m->imports) { 	scm_gc_mark(m->imports); }
}

void scm_module_sweep(SOBJ obj)
{
  if (SCM_MODULE(obj)) {  scm_free(SCM_MODULE(obj)); }
  SCM_MODULE(obj) = NULL;
}


void scm_module_print(SOBJ obj, PORT *p)
{
  port_puts(p, "#<module ");
  scm_write_obj(SCM_MODULE(obj)->name, p, TRUE);
  port_puts(p, ">");
}

void scm_module_write(SOBJ obj, PORT *p)
{
  scm_module_print(obj,p);
}

SCM_STRBUF *scm_module2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  return(scm_strbuf_concat_sprintf(sb, "#<module %s>", SCM_MODULE(obj)->name));
}

/****************************************************************
 * helper functions
 ****************************************************************/

/* Search symbol in a module.
 *
 * Only search in the specified module (0 depth search). Returns the
 * symbol entry or null if not found */

SOBJ scm_module_symbol_search(SOBJ mod, SOBJ atom)
{
  SCM_Module *m;
  SOBJ sym;

  if (!SCM_MODULEP(mod)) 	SCM_ERR("module-search: bad module", mod);
  if (!SCM_ATOMP(atom)) 	SCM_ERR("module-search: bad key", atom);
  
  m = SCM_MODULE(mod);
  sym = scm_hash_search(m->symbols, atom);
  return( sym == scm_false ? NULL : sym );
}

/* Search symbol in a module.
 *
 * If not found, create a new symbol with unbound value
 */
SOBJ scm_module_symbol_lookup(SOBJ mod, SOBJ atom)
{
  SCM_Module *m;
  SOBJ sym;

  if ( (sym = scm_module_symbol_search(mod, atom)) != NULL) 
	return(sym);

  m = SCM_MODULE(mod);
  return(SCM_CAR(scm_hash_set(m->symbols, atom, scm_unbound)));
}

/* find symbol in export list
 *
 * Try to find symbol in export list of imported modules of the
 * module. Returns a pointer to the symbol or NULL if search failed.
 */

SOBJ scm_module_find_exported_symbol(SCM_Module *m, SOBJ atom)
{
  SOBJ list, sym, module, obj;

  /*-- scan each imported module */

  for (list = m->imports; list != NULL; list = SCM_CDR(list)) {
	module = SCM_CAR(list);

	/*-- search exported symbol list */

	for (sym = SCM_MODULE(module)->exports; sym != NULL; sym = SCM_CDR(sym)) {

	  if (SCM_SYM_NAME(SCM_CAR(sym)) == atom) {
		obj = scm_module_symbol_search(module, atom);
		if (obj != NULL) return(obj);
		
		scm_puts("OOPS: symbol "); scm_cdisplay(atom);
		scm_puts(" is defined in export list of module ");
		scm_cdisplay(module);
		scm_puts(" but cannot be found in symbol table\n");
		break;
	  }
	}
	/*-- not found in the export list of this module. Try parent module */
	
	obj = scm_module_find_exported_symbol(SCM_MODULE(module), atom);
	if (obj != NULL) 	return(obj);
	
  }
  return(NULL);
}

/* Find symbol in module.
 *
 * First search current module, then search in imported module and
 * finally search in the global module.
 *
 * If create_it is true, we create the symbol if it is not found in
 * current and in explicitly imported modules.
 */
SOBJ scm_module_find_symbol(SOBJ mod, SOBJ atom, int create_it)
{
  SCM_Module *m;
  SOBJ sym;
  
  m = SCM_MODULE(mod);

  /*-- search in the specified */
  if ((sym = scm_module_symbol_search(mod, atom)) != NULL)
	return(sym);


  /*-- search in imported module chain */
  sym = scm_module_find_exported_symbol(m, atom);
  if (sym != NULL)
	return(sym);

  /*-- last chance: search in the global module */
  if ((sym = scm_hash_search(scm_symbol_hash, atom)) != scm_false)
	return(sym);
  
  if (create_it) {
	sym = SCM_CAR(scm_hash_set(m->symbols, atom, scm_unbound));
	return(sym);
  }	

  return(NULL);
}

/****************************************************************
 * Scheme functions for modules
 ****************************************************************/
/*E* (make-module NAME) => MODULE */
/*D* Returns a new module with name NAME. */
SOBJ scm_make_module(SOBJ name)
{
  SCM_Module *m;
  SOBJ new;

  if (!SCM_ATOMP(name)) SCM_ERR("make-module: bad atom", name);

  new = scm_hash_ref(scm_module_hash, name);

  if (new == scm_false) {

	/* module does not exist. have to create it */
	new = scm_newcell(SOBJ_T_MODULE);
	m = scm_must_alloc(sizeof(SCM_Module));
	
	SCM_MODULE(new) = m;
	m->name = name;
	m->symbols = scm_mkhash(SCM_HASH_T_SYMBOL); 	/* hash for symbols */
	m->imports = NULL;				/* list of imported module */
	m->exports = NULL;				/* list of exported symbols */
	m->export_all = FALSE;			/* default to export list */
	scm_hash_set(scm_module_hash, name, new);
  }
  return(new);
}

/*E* (module? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a module, #f otherwise */
SOBJ scm_modulep(SOBJ obj)
{
  return(SCM_MKBOOL(SCM_MODULEP(obj)));
}

/*E* (current-module) => MODULE*/
/*D* Returns the current module. */
SOBJ scm_get_current_module()
{
  return(scm_current_module);
}

/*E* (set-current-module MODULE) => MODULE */
/*D* Set MODULE as current module for symbol search. */
SOBJ scm_set_current_module(SOBJ mod)
{
  if (SCM_STRINGP(mod)) {
	mod = scm_hash_ref(scm_module_hash, mod);
  }
  if (SCM_SYMBOLP(mod)) {
	mod = scm_hash_ref(scm_module_hash, scm_symbol_to_string(mod));
  }

  if (!SCM_MODULEP(mod))	SCM_ERR("set-current-module: bad module", mod);
  scm_current_module = mod;
  return(mod);
}

/*E* (import MODULE...) => LIST */
/*D* Import symbols from modules given as argument. */
SOBJ scm_import(int argc, SOBJ *argv)
{
  SCM_Module *m = SCM_MODULE(scm_current_module);
  SOBJ *l = argv+argc;
  SOBJ *p = argv;
  SOBJ n;

  while(p < l) {
	if (!SCM_MODULE(*p)) 	SCM_ERR("import: bad module", *p);
	n = m->imports;
	if (n == NULL) {
	  m->imports = scm_cons(*p, NULL);
	} else {
	  while(n != NULL) {
		if (SCM_CAR(n) == *p)	break;
		if (SCM_CDR(n) == NULL) {
		  SCM_CDR(n) = scm_cons(*p, NULL);
		  break;
		}
		n = SCM_CDR(n);
	  }
	}
	p++;
  }
  return(m->imports);
}

/*E* (export SYM ...) => LIST*/
/*D* NOT IMPLEMENTED YET. */
SOBJ scm_export(int argc, SOBJ *argv)
{
  SCM_Module *m = SCM_MODULE(scm_current_module);

  return(m->exports);
}

/*E* (module-exports MODULE) => LIST*/
/*D* Returns the list of exported symbols */
SOBJ scm_module_exports(SOBJ mod)
{
  if (!SCM_MODULEP(mod)) SCM_ERR("module-exports: bad module", mod);
  return(SCM_MODULE(mod)->exports);
}

/*E* (module-imports MODULE) => LIST*/
/*D* Returns the list of imported modules. */
SOBJ scm_module_imports(SOBJ mod)
{
  if (!SCM_MODULEP(mod)) SCM_ERR("module-imports: bad module", mod);
  return(SCM_MODULE(mod)->imports);
}

/*I* (module-name MODULE) => STRING */
/*D* Return the module name as string */
SOBJ scm_module_name(SOBJ mod)
{
  if (!SCM_MODULEP(mod)) SCM_ERR("module-name: bad module", mod);
  return(SCM_MODULE(mod)->name);
}

/*E* (module-symbols MODULE) => LIST */
/*D* Returns the list of modules symbols. */
SOBJ scm_module_symbols(SOBJ mod)
{
  if (!SCM_MODULEP(mod)) SCM_ERR("module-symbols: bad module", mod);
  return(SCM_MODULE(mod)->symbols);
}

/*E* (find-module NAME) => MODULE*/
/*D* Return the MODULE having this NAME. If MODULE is not found, returns #f. */
SOBJ scm_find_module(SOBJ name)
{
  SOBJ mod = NULL;

  if (SCM_ATOMP(name)) {
	mod = scm_hash_ref(scm_module_hash, name);
  } else if (SCM_STRINGP(name)) {
	mod = scm_hash_ref(scm_module_hash, scm_mkatom(SCM_STR_VALUE(name)));
  } else {
	SCM_ERR("bad module name", name);
  }
  return( SCM_MODULEP(mod) ? mod : scm_false );
}

static void refresh_mod_symbols(SOBJ expr)
{
  SOBJ obj;

  while(expr) {
	obj = SCM_CAR(expr);
	if (SCM_PAIRP(obj)) { refresh_mod_symbols(obj); }
	if (SCM_SYMBOLP(obj)) {
	  SCM_CAR(obj) = scm_sym_clone(SCM_CAR(obj));
	}
	expr = SCM_CDR(expr);
  }
}

/*F* (module NAME BODY) => OBJ */
/*D* Create a new module NAME or set NAME as current module and
  evaluates the BODY. All symbols defined in the body will be store in
  the module's symbol list and are only visible inside this module.
  Returns the value of last evaluated expression. */

SOBJ scm_syntax_module(SOBJ icode, SOBJ expr, SOBJ env)
{
  SOBJ name;
  SOBJ old_mod, new_mod, sym;

  name = SCM_CAR(expr);
  if (!SCM_ATOMP(name))		SCM_ERR("define-module: bad module name", name);
  
  expr = SCM_CDR(expr);

#ifdef DEBUG_MODULE  
  scm_puts("module: name=");
  scm_cdisplay(name);	scm_puts(" expr=");		scm_cprint(expr);
#endif

  old_mod = scm_current_module;
  new_mod = scm_make_module(name);
  /* SCM_SYM_VALUE(name) = new_mod; */

  if ((sym = scm_hash_search(scm_symbol_hash, name)) == scm_false) {
	scm_hash_set(scm_symbol_hash, name, new_mod);
  }

  scm_current_module = new_mod;

  /*-- we have to resymbol all the expr before compiling it. */
  /* refresh_mod_symbols(expr); */

  scm_compile_begin(icode, expr, env);
  scm_current_module = old_mod;
  return(icode);
}

/*F* (export SYMBOL ...) */
/*D* Add all symbols to the export list of the module */

SOBJ scm_syntax_export(SOBJ icode, SOBJ expr, SOBJ env)
{
  SCM_Module *m;
  SOBJ atom, p;
  int found;

  scm_puts("; export: expr="); scm_cprint(expr);
  scm_puts(" env="); scm_cprint(env);

  m = SCM_MODULE(scm_current_module);
  while(expr) {
	atom = SCM_CAR(expr);
	if (!SCM_ATOMP(atom)) SCM_ERR("export: bad atom", atom);

	found = FALSE;
	for (p = m->exports; p ; p = SCM_CDR(p)) {
	  if (SCM_CAR(p) == atom) found = TRUE;
	}
	if (!found) {
	  m->exports = scm_cons(atom, m->exports);
	  scm_module_symbol_lookup(scm_current_module, atom);
	}	  
	expr = SCM_CDR(expr);
  }
  return(icode);
}

/*F* (import MODULE ...) */
/*D* Add modules as arguement to the import list of current module */

SOBJ scm_syntax_import(SOBJ icode, SOBJ expr, SOBJ env)
{
  scm_puts("; import: expr="); scm_cprint(expr);
  scm_puts(" env="); scm_cprint(env);

  return(icode);
}

/*F* MODULE::SYMBOL => OBJ */
/*D* Syntaxic sugar to access symbol of a module. Equivalent to
  (module MODULE SYMBOL) */

/*-- reconizer */
int scm_module_wreconize(PORT *port, char *s)
{
  return( strstr(s, "::") != NULL );
}

SOBJ scm_module_wparse(PORT *port, char *str)
{
  SOBJ mod, sym;
  char *buf, *p;
  SOBJ module, symbol;

  sym = NULL;
  buf = scm_must_strdup(str);

  if ((p = strstr(buf, "::")) != NULL) {
	*p = 0;	 			/* terminate string and point to sym*/
	module = scm_mkatom(buf);
	symbol = scm_mkatom(p + 2);
	mod = scm_find_module(module);
	sym = scm_module_find_symbol(mod, symbol, FALSE);
  }
  scm_free(buf);
  return( (sym != NULL) ? sym : scm_undefined);
}


/****************************************************************
 * Init module system
 ****************************************************************/
void scm_init_module()
{
  scm_module_hash = scm_mkhash(SCM_HASH_T_SYMBOL);

  /* export module-hash */
  scm_add_cvar("module-hash",		&scm_module_hash);

  /* create the global module */
  scm_global_module = scm_make_module(scm_mkatom("global"));
  SCM_MODULE(scm_global_module)->symbols = scm_symbol_hash;
  SCM_MODULE(scm_global_module)->export_all = TRUE;
  
  /* set the current module to the global one */
  scm_current_module = scm_global_module;
  
  scm_add_cprim("make-module", 		scm_make_module,	1);
  scm_add_cprim("module?",			scm_modulep,		1);

  scm_add_cprim("current-module",	scm_get_current_module, 	0);
  scm_add_cprim("set-current-module", scm_set_current_module, 	1);
  scm_add_cprim("import",			scm_import,					-1);
  
  scm_add_cprim("module-exports",	scm_module_exports,			1);
  scm_add_cprim("module-imports",	scm_module_imports,			1);
  scm_add_cprim("module-symbols",	scm_module_symbols,			1);
  scm_add_cprim("module-name",		scm_module_name,			1);
  scm_add_cprim("find-module",		scm_find_module,			1);

  scm_add_syntax("module",			scm_syntax_module);
  scm_add_syntax("export",			scm_syntax_export);
  scm_add_syntax("import",			scm_syntax_import);
}
