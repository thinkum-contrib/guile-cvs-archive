/* -*- tab-width:4; -*- */
/*
 * Hash tables
 *
 *
 *
 * We have 3 hash types
 *
 * SCM_HASH_T_GEN, the general one, SCM_HASH_T_SYMBOL and SCM_HASH_T_ATOM.
 *
 *
 * thread of SCM_HASH_T_GEN is: ( (key . value) ... )
 *
 * thread -><pair>[ . | *-]--->...
 *                  |
 *                  v
 *			<pair>[ <any>key | <any>value ]
 *
 *
 * Thread of SCM_HASH_T_SYMBOL is: ( sym ... )
 *
 * thread ->  <pair> [ . | *-]-->
 *                     |
 *                     v
 *            <sym>  [ <atom>name | <any>value ]
 *
 *
 * Thread of SCM_HASH_T_ATOM is: 
 *
 * thread -> <atom> [ <cstr>name | *-]---> ...
 *
 */

#include "s.h"

/****************************************************************
 * Type functions
 ****************************************************************/

void scm_hash_mark(SOBJ obj)
{
  int i;
  SOBJ l;
  SCM_Hash *h = SCM_HASH(obj);

  for (i = 0; i < h->hsize; i++) {
	l = h->hash[i];
	if (l != NULL) scm_gc_mark(l);
  }
}

void scm_hash_sweep(SOBJ obj)
{
  if (SCM_HASH(obj)) scm_free(SCM_HASH(obj));
  SCM_HASH(obj) = NULL;
}

/*E* #{ ... } => HASH */
/*D* generate a hash literal */
int scm_hash_reconize(PORT *port, int c)
{
  int next = port_peekc(port);
  return (c == '#' && next == '{');
}

SOBJ scm_hash_parse(PORT *port, int c)
{
  static SOBJ rbrace;
  SOBJ obj, l, *p;
  port_getc(port);				/* ignore leading '{' */

  if (rbrace == NULL)   rbrace = scm_mkatom("}");
  
  l = NULL;
  p = &l;
  while( (obj = scm_read_port(port)) != scm_eof) {
	if (SCM_ATOMP(obj) && obj == rbrace)	break;
	*p = scm_cons(obj, NULL);
	p = &(SCM_CDR(*p));
  }
  return(scm_list_to_hash(l));
}

static void scm_hash_dump(SOBJ obj, PORT *p, int flag)
{
  int i;
  SCM_Hash *h = SCM_HASH(obj);
  SOBJ l;

  switch(h->type) {
  case SCM_HASH_T_GEN:		port_puts(p, "#{");   				break;
  case SCM_HASH_T_SYMBOL:	port_puts(p, "#{<symbol-hash> ");  	break;
  case SCM_HASH_T_ATOM:		port_puts(p, "#{<atom-hash> "); 	break;
  default:					port_puts(p, "#{<???> ");
  }
  
  for (i = 0; i < h->hsize; i++) {
	l = h->hash[i];
	while(l) {
	  switch(h->type) {
	  case SCM_HASH_T_ATOM:
		scm_write_obj(l,p,flag);
		l = SCM_ATOM_NEXT(l);
		break;
	  case SCM_HASH_T_SYMBOL:
	  case SCM_HASH_T_GEN:
		scm_write_obj(SCM_CAR(l), p, flag);
		l = SCM_CDR(l);
		break;
	  }
	  port_putc(p, ' ');
	}
	/* This works for debugging hash threads...
	  if (h->hash[i]) scm_write_obj(h->hash[i], p, flag);
	*/
  }
  port_putc(p, '}');
}

/****************************************************************
 * Helper functions
 ****************************************************************/
Uint scm_hash_string(char *str)
{
  int c;
  unsigned int r = 0;
  while((c = *str++) != 0) {
	r = (r*7) ^ c;
	/*	r += (r << 3) + c; */
  }
  return(r);
}

Uint scm_hash_string1(char *str)
{
  unsigned int r = 0, c, x;

  while( (c = *str++) != 0) {
	x = (r << 4) + c;
	if (x) 	r = (r ^ (x % ((~0U >> 4U) + 1))) & (~0U >> 4U);
  }
  return(r);
}

Uint scm_hash_pointer(void *ptr)
{
  long n = (long) ptr;
  return( (n>>1) * 1103515245 );
}

Uint scm_hash_code(SCM_Hash *h, SOBJ key)
{
  int hc;
  
  switch(SCM_OBJTYPE(key)) {
  case SOBJ_T_ATOM:
	hc = scm_hash_string(SCM_ATOM_NAME(key));
	break;
	
  case SOBJ_T_STRING:
	hc = scm_hash_string(SCM_STR_VALUE(key));
 	break;
	
  case SOBJ_T_SYMBOL:
	hc = scm_hash_string(SCM_ATOM_NAME(SCM_SYM_NAME(key)));
	break;

  case SOBJ_T_POINTER:
	hc = scm_hash_pointer(SCM_POINTER(key));
	break;

  default:
	hc = scm_hash_pointer(key);
  }
#ifdef DEBUG
  scm_puts("; hash code for "); scm_cdisplay(key);
  scm_puts("="); scm_putn(hc);  scm_puts("\n");
#endif
  return(hc % h->hsize);
}

void scm_hash_print(SOBJ obj, PORT *p)
{
  scm_hash_dump(obj, p, TRUE);
}

void scm_hash_write(SOBJ obj, PORT *p)
{
  scm_hash_dump(obj, p, FALSE);
}

SCM_STRBUF *scm_hash2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  SCM_Hash *h = SCM_HASH(obj);
  char *spacer;
  SOBJ l;
  int i;

  spacer =
	(h->type == SCM_HASH_T_ATOM) ? "#{atom:" : 
	(h->type == SCM_HASH_T_SYMBOL) ? "#{symbol:" : "#{";

  for (i = 0; i < h->hsize; i++) {
	l = h->hash[i];
	while(l) {
	  sb = scm_strbuf_concat_str(sb, spacer); 	  spacer = " ";

	  switch(h->type) {
	  case SCM_HASH_T_ATOM:
		sb = scm_strbuf_concat_str(sb, SCM_ATOM_NAME(l));
		l = SCM_ATOM_NEXT(l);
		break;

	  case SCM_HASH_T_GEN:
		sb = scm_iobj2str(sb, SCM_CAR(l), 1);
		l = SCM_CDR(l);
		break;
	  case SCM_HASH_T_SYMBOL:
		{
		  SOBJ sym = SCM_CAR(l);
		  sb = scm_strbuf_concat_sprintf(sb, "(%s . ",
										 SCM_ATOM_NAME(SCM_SYM_NAME(sym)));
		  sb = scm_iobj2str(sb, SCM_SYM_VALUE(sym), 1);
		  sb = scm_strbuf_concat_str(sb, ")");
		  l = SCM_CDR(l);
		  break;
		}
	  }
	}
  }
  sb = scm_strbuf_concat_str(sb, "}");
  return(sb);
}

/****************************************************************
 * Scheme functions
 ****************************************************************/
SOBJ scm_mkhash(int type) 
{
  SOBJ new;
  SCM_Hash *h;
  int i;
  
  new = scm_newcell(SOBJ_T_HASH);
  h   = scm_must_alloc( sizeof(SCM_Hash) );

  h->hsize = 4;
  h->type  = type;
  h->hash  = scm_must_alloc(sizeof(SOBJ) * h->hsize);
  h->nkeys = 0;
  h->maxkeys = h->hsize * SCM_MAX_HASH_DEPTH;
  for (i = 0; i < h->hsize; i++) {
	h->hash[i] = NULL;
  }
  SCM_HASH(new) = h;
  return(new);
}

/*E* (make-hash [TYPE] ) => HASH */
/*D* Create a new hash of type TYPE. TYPE is 0 for generic, 1 for
 * symbol and 2 for atom hashes. If no type is given, creates a
 * general hash. */
SOBJ scm_make_hash(int nargs, SOBJ *arg)
{
  SOBJ type;
  int n;
  if (nargs == 0) 	return(scm_mkhash(SCM_HASH_T_GEN));

  if (nargs != 1)
	SCM_ERR("make-hash: bad number of arguments",SCM_MKINUM(nargs));

  type = arg[0];

  if (!SCM_INUMP(type)) 	SCM_ERR("make-hash: bad hash type", type);
  n = SCM_INUM(type);
  if (n != SCM_HASH_T_GEN &&
	  n != SCM_HASH_T_SYMBOL &&
	  n != SCM_HASH_T_ATOM) {
	SCM_ERR("make-hash: bad hash-type", type);
  }
  return(scm_mkhash(n));
}

/*E* (make-generic-hash) => HASH */
/*D* Create a generic hash */
SOBJ scm_make_generic_hash()
{
  return(scm_mkhash(SCM_HASH_T_GEN));
}

/*E* (make-symbol-hash) => SYMBOL-HASH */
/*D* Create a symbol hash */
SOBJ scm_make_symbol_hash()
{
  return(scm_mkhash(SCM_HASH_T_SYMBOL));
}

/*E* (make-atom-hash) => ATOM-HASH */
/*D* Create a atom hash. Atom hash cannot have any value associated to
 * the key */
SOBJ scm_make_atom_hash()
{
  return(scm_mkhash(SCM_HASH_T_ATOM));
}

/*-- principle is:
 * - build an array containing ref to all nodes of hash
 * - alloc a new hash[]
 * - fill the new hash[]
 * - free the ref array
 */
void scm_rebuild_hash(SOBJ hash)
{
  SOBJ *ref, l;
  int   nr, i, hc;
  SCM_Hash *h;

  h = SCM_HASH(hash);

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

  switch(h->type) {
  case SCM_HASH_T_GEN:
	for (i = 0; i < nr; i++) {
	  hc = scm_hash_code(h, SCM_CAAR(ref[i]));
	  SCM_CDR(ref[i]) = h->hash[hc];
	  h->hash[hc] = ref[i];
	}
	break;

  case SCM_HASH_T_SYMBOL:
	for (i = 0; i < nr; i++) {
	  hc = scm_hash_code(h, SCM_CAAR(ref[i]));
	  SCM_CDR(ref[i]) = h->hash[hc];
	  h->hash[hc] = ref[i];
	}
	break;

  case SCM_HASH_T_ATOM:
	for (i = 0; i < nr; i++) {
	  hc = scm_hash_string(SCM_ATOM_NAME(ref[i])) % h->hsize;
	  SCM_ATOM_NEXT(ref[i]) = h->hash[hc];
	  h->hash[hc] = ref[i];
	}
  }
  scm_free(ref);
}

/* KKK implement type dependant hash */

/* search a hash for the key.
 * returns a pointer to the toplevel node or NULL if not found. Toplevel
 * node is:
 * - the atom itself in case of atom hash
 * - the pointer to a pair which car is a symbol in case of symbol hash
 * - the pointer to a pair pointing to a key/value pair otherwise
 */
SOBJ scm_hash_search_key(SOBJ hash, SOBJ key, int *hcode, char **key_str)
{
  SCM_Hash *h;
  int i;
  SOBJ l;
  char *p;
  
  h = SCM_HASH(hash);
  p = NULL;
  
  switch(h->type) {

  case SCM_HASH_T_ATOM:
	
	switch(SCM_OBJTYPE(key)) {
	case SOBJ_T_ATOM:	p = SCM_ATOM_NAME(key); 				break;
	case SOBJ_T_STRING:	p = SCM_STR_VALUE(key);					break;
	case SOBJ_T_SYMBOL: p = SCM_ATOM_NAME(SCM_SYM_NAME(key));	break;
	default:
	  SCM_ERR("hash-search: atom-hash key must be atom|string|symbol",key);
	}
	i = scm_hash_string(p) % h->hsize;
	*hcode = i;  *key_str = p;
	for (l = h->hash[i]; l; l = SCM_ATOM_NEXT(l)) {
	  if (strcmp(p, SCM_ATOM_NAME(l)) == 0)	return(l);
	}
	break;
	
  case SCM_HASH_T_SYMBOL:
	
	switch(SCM_OBJTYPE(key)) {
	case SOBJ_T_ATOM:	p = SCM_ATOM_NAME(key);					break;
	case SOBJ_T_STRING:	p = SCM_STR_VALUE(key);					break;
	case SOBJ_T_SYMBOL:	p = SCM_ATOM_NAME(SCM_SYM_NAME(key));	break;
	default:
	  SCM_ERR("hash-search: symbol-hash key must be atom|string|symbol",key);
	}
	i = scm_hash_string(p) % h->hsize;
	*hcode = i;  *key_str = p;
	for (l = h->hash[i]; l; l = SCM_CDR(l)) {
	  if (streq(p, SCM_ATOM_NAME(SCM_SYM_NAME(SCM_CAR(l)))))	return(l);
	}
	break;
	
  case SCM_HASH_T_GEN:
	
	i = scm_hash_code(h, key);
	*hcode = i;
	*key_str = NULL;
	for (l = h->hash[i]; l; l = SCM_CDR(l)) {
	  if (scm_equal(key, SCM_CAAR(l)) != scm_false) return(l);
	}
	break;

  default:
	SCM_ERR("hash-search: illegal hash-type", hash);
  }
  return(NULL);
}

/*E* (hash-set! HASH KEY VALUE) => ASSOC */
/*D* Create or change an the entry matching KEY in HASH to VALUE. */
SOBJ scm_hash_set(SOBJ hash, SOBJ key, SOBJ value)
{
  SOBJ node;
  SCM_Hash *h;
  int hc;						/* the hash code */
  char *keystr;					/* the key string used to search */

  if (SCM_OBJTYPE(hash) != SOBJ_T_HASH) SCM_ERR("hash-set!: bad hash", hash);

  h = SCM_HASH(hash);
  node = scm_hash_search_key(hash, key, &hc, &keystr);

  if (node) {					/* found */
	switch(h->type) {
	case SCM_HASH_T_ATOM:	/* atom hash have no value */			break;
	case SCM_HASH_T_SYMBOL:	SCM_SYM_VALUE(SCM_CAR(node)) = value; 	break;
	case SCM_HASH_T_GEN:	SCM_CDR(SCM_CAR(node)) = value;			break;
	}
	return(node);
  }

  /* not found, create the node */
  
  switch(h->type) {
  case SCM_HASH_T_ATOM:
	{
	  SOBJ new = scm_newcell(SOBJ_T_ATOM);
	  SCM_ATOM_NAME(new) = scm_must_strdup(keystr);
	  SCM_ATOM_NEXT(new) = h->hash[hc];
	  h->hash[hc] = new;
	  node = new;
	  break;
	}
  case SCM_HASH_T_SYMBOL:
	{
	  SOBJ sym = scm_newcell(SOBJ_T_SYMBOL);
	  SCM_SYM_NAME(sym) = scm_mkatom(keystr);
	  SCM_SYM_VALUE(sym) = value;
	  h->hash[hc] = scm_cons(sym, h->hash[hc]);
	  node = h->hash[hc];
	  break;
	}
  case SCM_HASH_T_GEN:
	{
	  SOBJ assoc = scm_cons(key, value);
	  h->hash[hc] = scm_cons(assoc, h->hash[hc]);
	  node = h->hash[hc];
	  break;
	}
  default:
	SCM_ERR("hash-set!: illegal hash type", hash);
  }
  
  h->nkeys++;
  if (h->nkeys >= h->maxkeys) { scm_rebuild_hash(hash); }
  return(node);
}

/*E* (hash-ref HASH KEY) => OBJ | #f */
/*D* Return the value OBJ matching KEY in hash HASH. If KEY is not
  found, #f is returned */
SOBJ scm_hash_ref(SOBJ hash, SOBJ key)
{
  SOBJ node;
  SCM_Hash *h;
  int hc;						/* the hash code */
  char *keystr;					/* the key string used to search */

  if (SCM_OBJTYPE(hash) != SOBJ_T_HASH) SCM_ERR("hash-ref: bad hash", hash);

  h = SCM_HASH(hash);
  node = scm_hash_search_key(hash, key, &hc, &keystr);
  if (node) {
	switch(h->type) {
	case SCM_HASH_T_SYMBOL:	return(SCM_SYM_VALUE(SCM_CAR(node)));
	case SCM_HASH_T_GEN:	return(SCM_CDR(SCM_CAR(node)));
	}
	return(node);
  }
  return(scm_false);
}

/* just like hash-ref, but returns the assoc node, not the value */
/*E* (hash-search HASH KEY) => ASSOC | #f */
/*D* Search for KEY in HASH. If found, the matching ASSOC is returned,
  otherwise #f is returned. */
SOBJ scm_hash_search(SOBJ hash, SOBJ key)
{
  SOBJ node;
  SCM_Hash *h;
  int hc;						/* the hash code */
  char *keystr;					/* the key string used to search */

  if (SCM_OBJTYPE(hash) != SOBJ_T_HASH) SCM_ERR("hash-search: bad hash", hash);

  h = SCM_HASH(hash);
  node = scm_hash_search_key(hash, key, &hc, &keystr);
  if (node) {
	switch(h->type) {
	case SCM_HASH_T_SYMBOL:
	case SCM_HASH_T_GEN:	return(SCM_CAR(node));
	}
	return(node);
  }
  return(scm_false);
}

/*E* (hash-remove! HASH KEY) -> HASH */
/*D* Remove KEY from HASH. */
SOBJ scm_hash_remove(SOBJ hash, SOBJ key)
{
  SOBJ node, l, last;
  SCM_Hash *h;
  int hc;						/* the hash code */
  char *keystr;					/* the key string used to search */

  if (SCM_OBJTYPE(hash) != SOBJ_T_HASH) SCM_ERR("hash-ref: bad hash", hash);

  h = SCM_HASH(hash);
  node = scm_hash_search_key(hash, key, &hc, &keystr);
  if (node) {					/* found a node */
	last = NULL;
	for (last = NULL, l = h->hash[hc]; l; last = l, l = SCM_CDR(l)) {
	  if (SCM_CAR(l) == SCM_CAR(node)) {
		if (last) {
		  SCM_CDR(last) = SCM_CDR(node);
		} else {
		  h->hash[hc] = SCM_CDR(node);
		}
		SCM_CDR(node) = NULL;
		SCM_CAR(node) = NULL;
		break;
	  }
	}
  }
  return(hash);
}

/*E* (hash->list HASH) => ALIST */
/*D* Returned the content of the HASH as association list ALIST */
SOBJ scm_hash_to_list(SOBJ hash)
{
  SOBJ l, p;
  int i;
  SCM_Hash *h;
  
  if (!SCM_HASHP(hash)) SCM_ERR("hash->list: bad hash", hash);

  l = NULL;
  h = SCM_HASH(hash);
  for (i = 0; i < h->hsize; i++) {
	switch(h->type) {
	case SCM_HASH_T_GEN:
	  for (p = h->hash[i]; p; p = SCM_CDR(p))
		l = scm_cons(SCM_CAR(p), l);
	  break;
	case SCM_HASH_T_SYMBOL:
	  for (p = h->hash[i]; p; p = SCM_CDR(p))
		l = scm_cons(scm_cons(SCM_SYM_NAME(SCM_CAR(p)),
							  SCM_SYM_VALUE(SCM_CAR(p))), l);
	  break;
	case SCM_HASH_T_ATOM:
	  for (p = h->hash[i]; p; p = SCM_CDR(p))
		l = scm_cons(scm_cons(p, NULL), l);
	}
  }
  return(l);
}

/*E* (list->hash ALIST) => HASH */
/*D* Return a new HASH filled with the assiotiation found in ALIST. */
SOBJ scm_list_to_hash(SOBJ l)
{
  SOBJ hash;
  SOBJ e, k, v;
  
  if (l != NULL && !SCM_PAIRP(l))	SCM_ERR("list->hash: bad list", l);
  
  k = NULL;  v = NULL;
  hash =  scm_make_hash(0, NULL);
  while(l && SCM_PAIRP(l)) {
	e = SCM_CAR(l);
	l = SCM_CDR(l);
	switch(SCM_OBJTYPE(e)) {
	case SOBJ_T_PAIR:	k = SCM_CAR(e);   v = SCM_CDR(e);   break;
	case SOBJ_T_ATOM:
	  k = e;
	  if (l == NULL || !SCM_PAIRP(l)) SCM_ERR("list->hash: bad value", SCM_CDR(l));
	  v = SCM_CAR(l);
	  l = SCM_CDR(l);
	  break;
	default:
	  SCM_ERR("list->hash: bad list", l);
	}
	scm_hash_set(hash, k, v);
  }
  return(hash);
  
  while(l && SCM_PAIRP(l)) {
	e = SCM_CAR(l);
	if (!SCM_PAIRP(e)) 	SCM_ERR("list->hash: bad entry", e);
	scm_hash_set(hash, SCM_CAR(e), SCM_CDR(e));
	l = SCM_CDR(l);
  }
  return(hash);
}

/*E* (hash-stat HASH) => #undefined */
/*D* Display the statistics for the HASH */
SOBJ scm_hash_stat(SOBJ hash)
{
  SCM_Hash *h;
  SOBJ l;
  int i, n, max;
  int depth[10];
  char pbuf[128];

  if (!SCM_HASHP(hash)) SCM_ERR("hash-stat: bad hash", hash);
	
  h = SCM_HASH(hash);
  scm_puts("hash statistics:\n");
  scm_puts("hsize: ");  	scm_putn(h->hsize);  	scm_putc('\n');
  scm_puts("nkeys: ");  	scm_putn(h->nkeys);  	scm_putc('\n');
  scm_puts("maxkeys: "); 	scm_putn(h->maxkeys);	scm_putc('\n');
  
  scm_puts("distribution: ");
  max = 0;
  for (i = 0; i < 10; i++) { depth[i] = 0; }
  
  for (i = 0; i < h->hsize; i++) {
	n = 0;
	for (l = h->hash[i]; l; l = SCM_CDR(l)) n++;
	if (n > max) max = n;
	depth[ (n < 10) ? n : 9 ]++;
	scm_putn(n); scm_putc(' ');
  }
  scm_putc('\n');
  scm_puts("# keys in # thread\n");
  for (i = 0; i < 10; i++) {
	sprintf(pbuf, "%2d %4d (%02.2f%%)\n", i, depth[i],
			(h->hsize != 0) ? depth[i] * 100.0 / h->hsize : 0.0);
	scm_puts(pbuf);
  }
  scm_puts("worst case: "); scm_putn(max);	scm_putc('\n');

  return(scm_undefined);
}

/*E* (hash? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a hash, #f otherwise */
SOBJ scm_hashp(SOBJ x)
{
  return(SCM_MKBOOL(SCM_OBJTYPE(x) == SOBJ_T_HASH));
}

/*E* (atom-hash? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is an atom hash, #f otherwise */
SOBJ scm_atom_hashp(SOBJ x)
{
  return(SCM_MKBOOL( (SCM_OBJTYPE(x) == SOBJ_T_HASH) && (SCM_HASH(x)->type == SCM_HASH_T_ATOM)));
}

/*E* (symbol-hash? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a symbol hash, #f otherwise */
SOBJ scm_symbol_hashp(SOBJ x)
{
  return(SCM_MKBOOL( (SCM_OBJTYPE(x) == SOBJ_T_HASH) && (SCM_HASH(x)->type == SCM_HASH_T_SYMBOL)));
}

/*E* (normal-hash? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a generic (normal) hash, #f otherwise */

/*E* (generic-hash? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a generic (normal) hash, #f otherwise */
SOBJ scm_gen_hashp(SOBJ x)
{
  return(SCM_MKBOOL( (SCM_OBJTYPE(x) == SOBJ_T_HASH) && (SCM_HASH(x)->type == SCM_HASH_T_GEN)));
}

void scm_init_hash()
{
  scm_add_cprim("make-hash", 		scm_make_hash,			-1);
  scm_add_cprim("make-symbol-hash", scm_make_symbol_hash,	0);
  scm_add_cprim("make-atom-hash", 	scm_make_atom_hash,		0);
  
  scm_add_cprim("hash-set!",		scm_hash_set,		3);
  scm_add_cprim("hash-ref",			scm_hash_ref,		2);
  scm_add_cprim("hash-search",		scm_hash_search, 	2);
  scm_add_cprim("hash-remove!",		scm_hash_remove,	2);
  scm_add_cprim("hash->list",		scm_hash_to_list,	1);
  scm_add_cprim("list->hash",		scm_list_to_hash,	1);
  scm_add_cprim("hash-stat",		scm_hash_stat,		1);
  scm_add_cprim("hash?",			scm_hashp,			1);
  scm_add_cprim("atom-hash?",		scm_atom_hashp,		1);
  scm_add_cprim("symbol-hash?",		scm_symbol_hashp,	1);
  scm_add_cprim("generic-hash?",	scm_gen_hashp,		1);
  scm_add_cprim("normal-hash?",		scm_gen_hashp,		1);
}
