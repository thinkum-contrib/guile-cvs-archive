/* -*- tab-width:4; -*- */
/*
 * Atom
 */
#include "s.h"

SOBJ scm_atom_hash;				/* the hash holding the atoms */

/*** Type related function */
void scm_atom_mark(SOBJ obj)
{
  if (SCM_ATOM_NEXT(obj))	scm_gc_mark(SCM_ATOM_NEXT(obj));
}

void scm_atom_sweep(SOBJ obj)
{
  if (SCM_ATOM_NAME(obj))	scm_free(SCM_ATOM_NAME(obj));
  SCM_ATOM_NAME(obj) = NULL;
}

void scm_atom_print(SOBJ obj, PORT *p)
{
  if (SCM_ATOM_NAME(obj)) {
	port_puts(p, SCM_ATOM_NAME(obj));
  } else {
	port_puts(p, "#<atom ");
	port_putx(p, obj);
	port_putc(p, '>');
  }
}

SCM_STRBUF *scm_atom2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  return(scm_strbuf_concat_sprintf(sb, "#<atom %s>", SCM_ATOM_NAME(obj)));
}

/*** functions */
static void scm_rebuild_atom_hash()
{
  SOBJ *ref, l;
  int   nr, i, hc;
  SCM_Hash *h;

  h = SCM_HASH(scm_atom_hash);

  /* collect all atoms to ref[] */
  ref = scm_must_alloc(sizeof(SOBJ) * h->nkeys);
  nr = 0;
  for (i = 0; i < h->hsize; i++) {
	for (l = h->hash[i]; l; l = SCM_ATOM_NEXT(l)) {
	  ref[nr++] = l;
	}
  }

  /* resize the hash */
  scm_free(h->hash);
  h->hsize = h->hsize*4;
  h->maxkeys = h->hsize * SCM_MAX_HASH_DEPTH;
  h->hash = scm_must_alloc(sizeof(SOBJ) * h->hsize);
  
  /* initialize to nil */
  for (i = 0; i < h->hsize; i++) 	h->hash[i] = NULL;

  /* inject ref[] to the new hash */
  for (i = 0; i < nr; i++) {
	hc = scm_hash_string(SCM_ATOM_NAME(ref[i])) % h->hsize;

	SCM_ATOM_NEXT(ref[i]) = h->hash[hc];
	h->hash[hc] = ref[i];
  }	
  scm_free(ref);
}


static SOBJ scm_find_atom(char *str, int add)
{
  SCM_Hash *h;
  SOBJ l, new;
  int i;

  h = SCM_HASH(scm_atom_hash);
  i = scm_hash_string(str) % h->hsize;
  for (l = h->hash[i]; l; l = SCM_ATOM_NEXT(l)) {
	if (streq(SCM_ATOM_NAME(l), str))  return(l);
  }
  if (add) {
	new = scm_newcell(SOBJ_T_ATOM);
	SCM_ATOM_NAME(new) = scm_must_strdup(str);
	SCM_ATOM_NEXT(new) = h->hash[i];
	h->hash[i] = new;
	h->nkeys++;
	if (h->nkeys >= h->maxkeys) { scm_rebuild_atom_hash(); }
	return(new);
  }
  return(NULL);
}

SOBJ scm_mkatom(char *str)
{
  return(scm_find_atom(str, TRUE));
}

/*E* (string->atom STRING) => ATOM */
/*D* Returns the atom representing the string */
SOBJ scm_string_to_atom(SOBJ obj)
{
  if (!SCM_STRINGP(obj))	SCM_ERR("string->atom: bad string", obj);
  return(scm_mkatom(SCM_STR_VALUE(obj)));
}

/*E* (atom->string ATOM) => STRING */
/*D* Returns the STRING representing the ATOM. */

SOBJ scm_atom_to_string(SOBJ obj)
{
  if (!SCM_ATOMP(obj))		SCM_ERR("atom->string: bad atom", obj);
  return(scm_mkstring(SCM_ATOM_NAME(obj)));
}

SOBJ scm_atom_get_hash()
{
  return(scm_atom_hash);
}

/*** initialization */

void scm_init_atom()
{
  scm_atom_hash = scm_mkhash(SCM_HASH_T_ATOM);
  scm_gc_protect(&scm_atom_hash);

  scm_add_cprim("string->atom", scm_string_to_atom, 1);
  scm_add_cprim("atom->string", scm_atom_to_string, 1);
}
