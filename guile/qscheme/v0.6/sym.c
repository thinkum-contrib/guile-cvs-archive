/* -*- tab-width:4; -*- */
/*
 * Symbol and Keywords handling
 *
 *
 * Note: code to print and mark symbol is inlined in scm_gc_mark and scm_write
 *
 */

#include "s.h"

/*-- hashes */
SOBJ scm_symbol_hash;			/* hash for symbols */
SOBJ scm_keyword_hash;			/* hash for keywords */
int  scm_keyword_write_mode;	/* write mode for symbols */
char *scm_keyword_write_prefix;
char *scm_keyword_write_suffix;

/*-- make a symbol */

SOBJ scm_mksymbol2(SOBJ atom, SOBJ value)
{
  SOBJ new = scm_newcell(SOBJ_T_SYMBOL);
  SCM_SYM_NAME(new)  = atom;
  SCM_SYM_VALUE(new) = value;
  return(new);
}

SOBJ scm_mksymbol(char *str)
{
  return(scm_mksymbol2(scm_mkatom(str), scm_unbound));
}

SOBJ scm_mkkeyword2(SOBJ atom, SOBJ dummy)
{
  SOBJ new = scm_newcell(SOBJ_T_KEYWORD);
  SCM_KEYW_NAME(new) = atom;
  return(new);
}
	 
SOBJ scm_mkkeyword(char *str)
{
  return(scm_mkkeyword2(scm_mkatom(str), NULL));
}

void scm_symbol_mark(SOBJ obj)
{
  scm_gc_mark(SCM_SYM_NAME(obj));
  scm_gc_mark(SCM_SYM_VALUE(obj));
}

void scm_symbol_sweep(SOBJ obj)
{
}

/*-- search for a symbol */
SOBJ scm_symsearch(char *str)
{
  SOBJ obj = scm_hash_search(scm_symbol_hash, scm_mkatom(str));
  return( (obj == scm_false) ? NULL : obj );
}

/*-- add a new symbol: no duplicate checks are done */

SOBJ scm_atom_hash_complete(SOBJ hash, SOBJ atom, SOBJ (*create)())
{
  int i;
  SCM_Hash *h;
  SOBJ p, e;

  h = SCM_HASH(hash);
  i = scm_hash_code(h, atom);
  for (p = h->hash[i]; p; p = SCM_CDR(p)) {
	e = SCM_CAR(p);
	if (SCM_SYM_NAME(e) == atom) return(e);
  }
  e = (*create)(atom, scm_unbound);
  h->hash[i] = scm_cons(e, h->hash[i]);
  h->nkeys++;

  if (h->nkeys >= h->maxkeys) { scm_rebuild_hash(hash); }
  return(e);
}

SOBJ scm_symadd(char *str, SOBJ value)
{
  SOBJ node;
  SOBJ atom = scm_mkatom(str);
  node = scm_hash_set(scm_symbol_hash, atom, value);
  node = scm_hash_search(scm_symbol_hash, atom);
#ifdef DEBUG
  scm_puts("scm_symadd: ");  scm_cprint(node);
#endif
  return(node);
}

SOBJ scm_sym_clone(SOBJ sym)
{
  return(scm_atom_hash_complete(scm_symbol_hash,
								SCM_SYM_NAME(sym),
								scm_mksymbol2));
}

/****************************************************************
 * Keyword routines
 ****************************************************************/
SOBJ scm_keyword_add(char *str)
{
  return(scm_atom_hash_complete(scm_keyword_hash,
								scm_mkatom(str),
								scm_mkkeyword2));
}

SOBJ scm_keywordp(SOBJ obj)
{
  return(SCM_MKBOOL(SCM_OBJTYPE(obj) == SOBJ_T_KEYWORD));
}

SOBJ scm_keyword_to_string(SOBJ obj)
{
  if (!SCM_KEYWORDP(obj)) 	SCM_ERR("keyword->string: bad keyword", obj);
  return(scm_atom_to_string(SCM_KEYW_NAME(obj)));
}

SOBJ scm_string_to_keyword(SOBJ obj)
{
  if (!SCM_STRINGP(obj)) 	SCM_ERR("string->keyword: bad string", obj);
  return(scm_keyword_add(SCM_STR_VALUE(obj)));
}

SOBJ scm_get_keyword(SOBJ keyw, SOBJ list, SOBJ default_value)
{
  if (!SCM_KEYWORDP(keyw))	SCM_ERR("get-keyword: bad keyword", keyw);

  while(list) {
	if (!SCM_PAIRP(list))	goto bad_list;

	if (!SCM_KEYWORDP(SCM_CAR(list)))
		SCM_ERR("get-keyword: bad keyword", SCM_CAR(list));
	
	if (SCM_KEYW_NAME(SCM_CAR(list)) == SCM_KEYW_NAME(keyw)) {
	  list = SCM_CDR(list);
	  if (!SCM_PAIRP(list))	SCM_ERR("get-keyword: bad value", list);
	  return(SCM_CAR(list));
	}
	/* jump over value and get next potential keyword */
	list = SCM_CDR(list);  if (!SCM_PAIRP(list))  goto bad_list;
	list = SCM_CDR(list);
  }
  return(default_value);

 bad_list:
  SCM_ERR("get-keyword: bad list", list);
  return(NULL);
}

void scm_init_symbol_hash()
{
  scm_symbol_hash = scm_mkhash(SCM_HASH_T_SYMBOL);
  scm_gc_protect(&scm_symbol_hash);

  scm_keyword_hash= scm_mkhash(SCM_HASH_T_SYMBOL);
  scm_gc_protect(&scm_keyword_hash);
}

/*E* (keyword-display-type NUMBER) => NUMBER */
/*D* Changes the way of keyword display. When NUMBER is 0, the
  keywords are displayed prefixed by ':', when NUMBER is 1 they are
  prefixed with '#!' and when NUMBER is 2 they are suffixed with ':' */
SOBJ scm_keyword_display_type(SOBJ x)
{
  int mode = SCM_INUM(x);
  switch(mode) {
  case SCM_KEYW_WRITE_DEFLT:
	scm_keyword_write_prefix = ":";
	scm_keyword_write_suffix = "";
	break;
  case SCM_KEYW_WRITE_DSSL:
	scm_keyword_write_prefix = "#!";
	scm_keyword_write_suffix = "";
	break;
  case SCM_KEYW_WRITE_OTHER:
	scm_keyword_write_prefix = "";
	scm_keyword_write_suffix = ":";
	break;
  default:
	mode = SCM_KEYW_WRITE_DEFLT;
  }
  scm_keyword_write_mode = mode;
  return(x);
}

/*E* (gensym [PREFIX]) => SYMBOL */
/*D* Returns a new symbol which is guaranted to be unique (during this
  scheme session). */
SOBJ scm_gensym(int nargs, SOBJ *arg)
{
  char *prefix;
  char buf[128];
  static int count;
  SOBJ new;

  if (nargs != 0) {
	scm_puts("WARNING: gensym ignores prefix argument");
  }
  prefix = SCM_GENSYM_PREFIX;
  sprintf(buf, "%s%d", prefix, count++);
  new = scm_newcell(SOBJ_T_ATOM);
  SCM_ATOM_NAME(new) = scm_must_strdup(buf);
  SCM_ATOM_NEXT(new) = NULL;
  return(scm_mksymbol2(new, scm_unbound));
}

/*E* (make-symbol NAME) => SYMBOL */
/*D* Create an unbound symbol. */
SOBJ scm_make_symbol(SOBJ name)
{
  if (!SCM_ATOMP(name))	SCM_ERR("bad symbol", name);
  return(scm_mksymbol2(name, scm_unbound));
}

/*E* (symbol-name SYM) => NAME */
/*D* Returns the name associated to symbol SYM. NAME is an atom.*/
SOBJ scm_symbol_name(SOBJ x)
{
  if (!SCM_SYMBOLP(x)) 	SCM_ERR("bad symbol", x);
  return(SCM_SYM_NAME(x));
}

/*E* (symbol-value SYM) => VALUE */
/*D* Returns the value associated to symbol SYM. VALUE is any scheme
  object. */
SOBJ scm_symbol_value(SOBJ x)
{
  if (!SCM_SYMBOLP(x)) 	SCM_ERR("bad symbol", x);
  return(SCM_SYM_VALUE(x));
}

void scm_init_symbol()
{
  scm_keyword_write_mode = SCM_KEYW_WRITE_DEFLT;
  scm_keyword_write_prefix = ":";
  scm_keyword_write_suffix = "";

  scm_add_cprim("keyword-display-type", scm_keyword_display_type, 1);
  scm_add_cprim("gensym",				scm_gensym,			-1);
  scm_add_cprim("make-symbol",			scm_make_symbol,	1);
  scm_add_cprim("symbol-name",			scm_symbol_name,	1);
  scm_add_cprim("symbol-value",			scm_symbol_value,	1);
}
