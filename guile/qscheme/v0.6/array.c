/* -*- tab-width:4; -*- */
/*
 * Implementation of arrays
 */
#include "s.h"
#include "heap.h"


void scm_array_mark(SOBJ arr)
{
  int i;
  SOBJ *slot;

  slot = SCM_ARRAY(arr);
  if (slot == NULL) 	return;
  for (i = 0; i < SCM_ASIZE(arr); i++) {
	if (!SCM_INUMP(slot[i]) && scm_is_pointer_to_heap(slot[i])) {
	  scm_gc_mark(slot[i]);
	}	
  }
}

void scm_array_sweep(SOBJ arr)
{
  SCM_Array *p = SCM_ADESCR(arr);

#ifdef DEBUG
  scm_puts("; removing array #");  port_putx(SCM_OUTP, arr);
  scm_puts("\n");
#endif

  if (p != NULL) {
	scm_free(p);
  }
  SCM_ADESCR(arr) = NULL;
}

/*-- array reconizer and parser */
int scm_array_reconize(PORT *port, int c)
{
  int next = port_peekc(port);
  return (c == '#' && next == '(');
}

SOBJ scm_array_parse(PORT *port, int start_char)
{
  port_getc(port);			/* ignore the leading '(' */
  return(scm_list_to_vector(scm_read_list(port)));
}

SOBJ scm_mkarray(int size, SOBJ fill)
{
  SOBJ arr, *p;
  int i;

  arr = scm_newcell(SOBJ_T_ARRAY);
  SCM_ADESCR(arr) =
	scm_must_alloc(sizeof(SCM_Array) + (size * sizeof(SOBJ)));
  
  SCM_ADESCR(arr)->size = size;
  SCM_ADESCR(arr)->alloced = size + 1;

  p = SCM_ARRAY(arr);
  for (i = 0; i < size; i++)  *p++ = fill;
  return(arr);
}

/************************************************************************
 * Vector type definition
 ************************************************************************/

void scm_vector_print(SOBJ arr, PORT *p)
{
  int i;
  port_puts(p, "#(");
  for (i = 0; i < SCM_ASIZE(arr); i++) {
	scm_write_obj(SCM_AREF(arr, i), p, 1);
	if (i < (SCM_ASIZE(arr) -1)) port_putc(p, ' ');
  }
  port_putc(p, ')');
}

void scm_vector_write(SOBJ arr, PORT *p)
{
  int i;
  port_puts(p, "#(");
  for (i = 0; i < SCM_ASIZE(arr); i++) {
	scm_write_obj(SCM_AREF(arr, i), p, 0);
	if (i < (SCM_ASIZE(arr) -1)) port_putc(p, ' ');
  }
  port_putc(p, ')');
}

SCM_STRBUF *scm_array2str(SCM_STRBUF *sb, SOBJ arr, int raw)
{
  int i;
  char *delim;
  
  delim = "#(";
  for (i = 0; i < SCM_ASIZE(arr); i++) {
	sb = scm_strbuf_concat_str(sb, delim);  delim = " ";
	sb = scm_iobj2str(sb, SCM_AREF(arr, i), 1);
  }
  return(scm_strbuf_concat_str(sb, ")"));
}

SOBJ scm_array_compare(SOBJ a1, SOBJ a2)
{
  int i;

  if (SCM_ASIZE(a1) != SCM_ASIZE(a2)) return(scm_false);
  for (i = 0; i < SCM_ASIZE(a1); i++) {
	if (scm_equal(SCM_AREF(a1,i), SCM_AREF(a2,i)) == scm_false)
	  return(scm_false);
  }
  return(scm_true);
}

/****************************************************************
 * R5RS procedures
 ****************************************************************/
/*S* (vector? OBJ) => BOOLEAN */
/*D* Returns #t if obj is a vector, otherwise returns #f. */
SOBJ scm_vectorp(SOBJ obj)
{
  return( SCM_ARRAYP(obj) ? scm_true : scm_false );
}

/*S* (make-vector K [FILL]) => VECTOR */
/*D* Returns a newly allocated vector of K elements.  If a second
  argument is given, then each element is initialized to fill.
  Otherwise the initial contents of each element is unspecified. */
SOBJ scm_make_vector(int n, SOBJ *p)
{
  SOBJ fill;
  if (n < 1 || !SCM_INUMP(p[0])) 	SCM_ERR("make-vector: bad count", p[0]);
  fill = (n >=2) ? p[1] : scm_unbound;
  return(scm_mkarray(SCM_INUM(p[0]), fill));
}

/*S* (vector obj ...) => vector */
/*D* Returns a newly allocated vector whose elements contain the given
  arguments. */
/*X* (vector 'a 'b 'c) =>  #(a b c) */
SOBJ scm_vector(int n, SOBJ *p)
{
  int i;
  SOBJ new = scm_mkarray(n, NULL);
  for (i = 0; i < n; i++) {
	SCM_AREF(new, i) = p[i];
  }
  return(new);
}

/*S* (vector-length vector) => number */
/*D* Returns the number of elements in vector as an exact integer. */
SOBJ scm_vector_length(SOBJ obj)
{
  return(SCM_MKINUM(SCM_ASIZE(obj)));
}

/*S* (vref VECTOR K) => OBJ */
/*D* Alias for vector-ref */

/*S* (vector-ref vector k) => obj */
/*D* k must be a valid index of vector.  Vector-ref returns the contents of
  element k of vector. */
/*X* (vector-ref #(1 1 2 3 5 8 13 21) 5) => 8 */
SOBJ scm_vector_ref(SOBJ vector, SOBJ index)
{
  int i;

  if (!SCM_ARRAYP(vector))	SCM_ERR("vector-ref: bad vector", vector);
  if (!SCM_INUMP(index))	SCM_ERR("vector-ref: bad index", index);
  i = SCM_INUM(index);
  if (i < 0 || i >= SCM_ASIZE(vector))
	SCM_ERR("vector-ref: out of range", index);

  return(SCM_AREF(vector, i));
}

/*E* (vset! VECTOR K OBJ) => #undefined */
/*D* Alias for vector-set! */

/*S* (vector-set! vector k obj) => #undefined */
/*D* Stores obj in element k of vector. The value returned by
  vector-set! is unspecified. */
SOBJ scm_vector_set(SOBJ vector, SOBJ index, SOBJ value)
{
  int i;

  if (!SCM_ARRAYP(vector))	SCM_ERR("vector-set!: bad vector", vector);
  if (!SCM_INUMP(index))	SCM_ERR("vector-set!: bad index", index);
  i = SCM_INUM(index);
  if (i < 0 || i >= SCM_ASIZE(vector))
	SCM_ERR("vector-set!: out of range", index);

  SCM_AREF(vector, i) = value;
  return(scm_undefined);
}

/*S* (vector->list VECTOR) => LIST */
/*D* Returns a newly allocated list of the objects contained in
  the elements of VECTOR. */
SOBJ scm_vector_to_list(SOBJ vector)
{
  int i;
  SOBJ list = NULL;

  if (!SCM_ARRAYP(vector)) 	SCM_ERR("vector->list: bad vector", vector);
  i = SCM_ASIZE(vector);
  while(--i >= 0) {
	list = scm_cons(SCM_AREF(vector, i), list);
  }
  return(list);
}

/*S* (list->vector LIST) => VECTOR */
/*D* Returns a newly created VECTOR initialized to the elements of the
  list LIST. */
SOBJ scm_list_to_vector(SOBJ list)
{
  int i, len = scm_list_length(list);
  SOBJ v;
  
  if (len < 0)	SCM_ERR("list->vector: bad list", list);
  v = scm_mkarray(len, scm_unbound);
  i = 0;
  while(list) {
	SCM_AREF(v, i) = SCM_CAR(list);
	list = SCM_CDR(list);
	i++;
  }
  return(v);
}

/*S* (vector-fill! VECTOR FILL) => #undefined */
/*D* Stores FILL in every element of VECTOR. */
SOBJ scm_vector_fill(SOBJ vector, SOBJ fill)
{
  int i;
  if (!SCM_ARRAYP(vector))	SCM_ERR("vector-fill! bad vector", vector);
  
  for (i = 0; i < SCM_ASIZE(vector); i++) {
	SCM_AREF(vector, i) = fill;
  }
  return(scm_undefined);
}

/****************************************************************
 * Extensions
 ****************************************************************/
/*E* (vector-resize VECTOR NEWSIZE) => VECTOR */
/*D* Change the size of VECTOR to NEWSIZE. */
SOBJ scm_vector_resize(SOBJ vector, SOBJ size)
{
  int newsize;
  
  if (!SCM_ARRAYP(vector))	SCM_ERR("vector-resize: bad vector", vector);
  if (!SCM_INUMP(size))		SCM_ERR("vector-resize: bad size",   size);

  newsize = SCM_INUM(size);
  if (newsize > SCM_ADESCR(vector)->alloced) {
	SCM_Array *p;
	int i;

	p = scm_must_realloc(SCM_ADESCR(vector),
						 sizeof(SCM_Array) + (newsize * sizeof(SOBJ)));

	for (i = p->size; i < newsize; i++) {
	  p->item[i] = scm_unbound;
	}
	SCM_ADESCR(vector) = p;
	p->size = newsize;
	p->alloced = newsize + 1;
  } else {
	SCM_ASIZE(vector) = newsize;
  }
  return(vector);
}

/*E* (vector-append VECTOR OBJ) => VECTOR */
/*D* Append object OBJ to vector VECTOR, incrementing it's size */
SOBJ scm_vector_append(SOBJ vector, SOBJ item)
{
  int i;

  if (!SCM_ARRAYP(vector))		SCM_ERR("bad vector", vector);
  i = SCM_ASIZE(vector);
  scm_vector_resize(vector, SCM_MKINUM(i+1));
  SCM_AREF(vector, i) = item;
  return(vector);
}

/*E* (vector-sort! VECTOR COMP) => #undefined */
/*D* Sort vector. COMP is a simple function with 2 arguments testing
 * comparing 2 elements. */
SOBJ scm_vector_sort(SOBJ vector, SOBJ comp)
{
  SOBJ *a, tmp;
  int i,j,incr, size;
  
  if (!SCM_ARRAYP(vector))	SCM_ERR("bad vector", vector);

  a = SCM_ARRAY(vector);
  size = SCM_ASIZE(vector);

  for (incr = size / 2; incr; incr /= 2) {
    for (i = incr; i < size; i++) {
      for (j = i-incr; j >= 0; j -= incr) {
		if (scm_cmpnum(a[j], a[j+incr]) < 0)	break;
#ifdef OLD
		if (scm_apply2(comp, a[j], a[j+incr]) != scm_true)  break;
#endif
		tmp = a[j+incr];  a[j+incr] = a[j];  a[j] = tmp;
      }
    }
  }
  return(vector);
}

static void vqsort(SOBJ *a, int l, int r)
{
  int i;
  SOBJ v, t, *ai, *aj;

  if (l < r) {
	v = a[r];
	ai = a + (l - 1);
	aj = a + r;
	for (;;) {
	  while(ai <= aj && scm_cmpnum(*(++ai), v) < 0) ;
	  while(ai <= aj && scm_cmpnum(*(--aj), v) > 0) ;
	  if (ai >= aj)	break;
	  t = *ai;  *ai = *aj;  *aj = t;
	}
	t = *ai;  *ai = v;   a[r] = t;
	i = (ai - a);
	vqsort(a, l, i-1);	
	vqsort(a, i+1, r);
  }
}


/*E* (vector-qsort! VECTOR) => VECTOR */
/*D* Destructively sort a vector using the qsort algorythm. The vector
 * is supposed to contain number only. */
SOBJ scm_vector_qsort(SOBJ vector)
{
  if (!SCM_ARRAYP(vector))	SCM_ERR("bad vector", vector);
  vqsort(SCM_ARRAY(vector), 0, SCM_ASIZE(vector) - 1);
  return(vector);
}

/*E* (vector-remove! VECTOR ELT) => #undef */
/*D* Remove item at index ELT from VECTOR */
SOBJ scm_vector_remove(SOBJ vector, SOBJ elt)
{
  int i, len;
  SOBJ *a, *l;
  if (!SCM_ARRAYP(vector))	SCM_ERR("bad vector", vector);
  if (!SCM_INUMP(elt))		SCM_ERR("bad index",  elt);

  i = SCM_INUM(elt);
  len = SCM_ASIZE(vector);
  if (len >= 1 && i >= 0 && i < (len - 1)) {
	SCM_ASIZE(vector) = len - 1;
	a = SCM_ARRAY(vector) + i;
	l = SCM_ARRAY(vector) + len - 1;
	while(a < l) {
	  a[0] = a[1];
	  a++;
	}
  }
  return(vector);
}


/****************************************************************
 * Initialisation
 ****************************************************************/

void scm_init_array()
{
  /* R5RS */
  scm_add_cprim("vector?",			scm_vectorp,		1);
  scm_add_cprim("make-vector",		scm_make_vector,	-1);
  scm_add_cprim("vector",			scm_vector,			-1);
  scm_add_cprim("vector-length",	scm_vector_length,	1);
  scm_add_cprim("vector-ref",		scm_vector_ref,		2);
  scm_add_cprim("vref",				scm_vector_ref,		2);
  scm_add_cprim("vector-set!",		scm_vector_set,		3);
  scm_add_cprim("vset!",			scm_vector_set,		3);
  scm_add_cprim("vector->list",		scm_vector_to_list,	1);
  scm_add_cprim("list->vector",		scm_list_to_vector,	1);
  scm_add_cprim("vector-fill!",		scm_vector_fill,	2);

  /* extensions */
  scm_add_cprim("vector-resize",	scm_vector_resize,  2);
  scm_add_cprim("vector-append",	scm_vector_append,  2);
  scm_add_cprim("vector-sort!",		scm_vector_sort,	2);
  scm_add_cprim("vector-qsort!",	scm_vector_qsort,	1);
  scm_add_cprim("vector-remove!",	scm_vector_remove,	2);

  
}
