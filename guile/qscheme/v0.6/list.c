/* -*- tab-width:4; -*- */
/*
 * List related functions
 */
#include "s.h"
#include "vm2.h"
#include "stack.h"

static void wrong_arg_type(char *func, SOBJ obj)
{
  char erbuf[128];
  sprintf(erbuf, "%s: wrong arg type", func);
  SCM_ERR(erbuf, obj);
}

/*S* (pair? OBJ) => BOOLEAN */
/*D* Returns #t if obj is a pair, and otherwise returns #f. */
SOBJ scm_pairp(SOBJ x)
{
  return (SCM_PAIRP(x) ? scm_true : scm_false);
}

/*S* (cons OBJ1 OBJ2) => PAIR */
/*D* Returns a newly allocated pair whose car is OBJ1 and whose cdr is
OBJ2.  The pair is guaranteed to be different (in the sense of eqv?)
from every existing object. */

SOBJ scm_cons(SOBJ car, SOBJ cdr)
{
  SOBJ new = scm_newcell(SOBJ_T_PAIR);
  SCM_CAR(new) = car;
  SCM_CDR(new) = cdr;
  return (new);
}

/*E* (cons2 OBJ1 OBJ2 OBJ3) => PAIR */
/*D* Return a new pair construct whose car is OBJ1 and cdr is a cons
  of OBJ2 and OBJ3. Example: (cons2 1 2 3) => (1 2 . 3) */
SOBJ scm_cons2(SOBJ car, SOBJ cdr, SOBJ cddr)
{
  SOBJ new = scm_newcell(SOBJ_T_PAIR);
  SCM_CAR(new) = car;
  SCM_CDR(new) = scm_newcell(SOBJ_T_PAIR);
  SCM_CADR(new) = cdr;
  SCM_CDDR(new) = cddr;
  return (new);
}

/*S* (car PAIR) => OBJ*/
/*D* Returns the contents of the car field of pair. Note that it is an
  error to take the car of the empty list.*/
SOBJ scm_car(SOBJ obj)
{
  if (!SCM_PAIRP(obj))
    wrong_arg_type("car", obj);
  return (SCM_CAR(obj));
}

/*S* (cdr PAIR) => OBJ */
/*D* Returns the contents of the cdr field of pair.  Note that it is
  an error to take the cdr of the empty list. */
SOBJ scm_cdr(SOBJ obj)
{
  if (!SCM_PAIRP(obj))
    wrong_arg_type("cdr", obj);
  return (SCM_CDR(obj));
}

/*S* (set-car! PAIR OBJ) => #undefined */
/*D* Stores OBJ in the car field of PAIR. */
SOBJ scm_setcar(SOBJ obj, SOBJ val)
{
  if (!SCM_PAIRP(obj))
    wrong_arg_type("set-car!", obj);
  SCM_CAR(obj) = val;
  return (scm_undefined);
}

/*S* (set-cdr! PAIR OBJ) => #undefined */
/*D* Stores OBJ in the cdr field of PAIR. */
SOBJ scm_setcdr(SOBJ obj, SOBJ val)
{
  if (!SCM_PAIRP(obj))
    wrong_arg_type("set-cdr!", obj);
  SCM_CDR(obj) = val;
  return (scm_undefined);
}

static SOBJ internal_cxr(SOBJ l, char *fct)
{
  register SOBJ tmp = l;
  register char *p;

  for (p = fct + strlen(fct) - 1; *p != 'X'; p--) {
    if (tmp == NULL || !SCM_PAIRP(tmp)) {
      char name[50];
      sprintf(name, "c%sr: bad list", fct + 1);
      SCM_ERR(name, l);
    }
    tmp = (*p == 'a') ? SCM_CAR(tmp) : SCM_CDR(tmp);
  }
  return tmp;
}

SOBJ scm_caar(SOBJ l) {  	return(internal_cxr(l, "Xaa"));		}
SOBJ scm_cdar(SOBJ l) {  	return(internal_cxr(l, "Xda"));		}
SOBJ scm_cadr(SOBJ l) {  	return(internal_cxr(l, "Xad"));		}
SOBJ scm_cddr(SOBJ l) {  	return(internal_cxr(l, "Xdd"));		}
SOBJ scm_caaar(SOBJ l) { 	return(internal_cxr(l, "Xaaa"));	}
SOBJ scm_cdaar(SOBJ l) {	return(internal_cxr(l, "Xdaa"));	}
SOBJ scm_cadar(SOBJ l) {	return(internal_cxr(l, "Xada"));	}
SOBJ scm_cddar(SOBJ l) {	return(internal_cxr(l, "Xdda"));	}
SOBJ scm_caadr(SOBJ l) {  	return(internal_cxr(l, "Xaad"));	}
SOBJ scm_cdadr(SOBJ l) { 	return(internal_cxr(l, "Xdad"));	}
SOBJ scm_caddr(SOBJ l) {  	return(internal_cxr(l, "Xadd"));	}
SOBJ scm_cdddr(SOBJ l) {  	return(internal_cxr(l, "Xddd"));	}
SOBJ scm_caaaar(SOBJ l) {  	return(internal_cxr(l, "Xaaaa"));	}
SOBJ scm_cdaaar(SOBJ l) {  	return(internal_cxr(l, "Xdaaa"));	}
SOBJ scm_cadaar(SOBJ l) {  	return(internal_cxr(l, "Xadaa"));	}
SOBJ scm_cddaar(SOBJ l) {  	return(internal_cxr(l, "Xddaa"));	}
SOBJ scm_caadar(SOBJ l) {  	return(internal_cxr(l, "Xaada"));	}
SOBJ scm_cdadar(SOBJ l) {   return(internal_cxr(l, "Xdada"));	}
SOBJ scm_caddar(SOBJ l) {   return(internal_cxr(l, "Xadda"));	}
SOBJ scm_cdddar(SOBJ l) {   return(internal_cxr(l, "Xddda"));	}
SOBJ scm_caaadr(SOBJ l) {   return(internal_cxr(l, "Xaaad"));	}
SOBJ scm_cdaadr(SOBJ l) {   return(internal_cxr(l, "Xdaad"));	}
SOBJ scm_cadadr(SOBJ l) {   return(internal_cxr(l, "Xadad"));	}
SOBJ scm_cddadr(SOBJ l) {   return(internal_cxr(l, "Xddad"));	}
SOBJ scm_caaddr(SOBJ l) {   return(internal_cxr(l, "Xaadd"));	}
SOBJ scm_cdaddr(SOBJ l) {   return(internal_cxr(l, "Xdadd"));	}
SOBJ scm_cadddr(SOBJ l) {   return(internal_cxr(l, "Xaddd"));	}
SOBJ scm_cddddr(SOBJ l) {   return(internal_cxr(l, "Xdddd"));	}

/*S* (null? OBJ) => BOOLEAN */
/*D* Returns #t if obj is the empty list, otherwise returns #f. */
SOBJ scm_nullp(SOBJ obj)
{
  return ((obj == NULL) ? scm_true : scm_false);
}

/* Return the length of SX, or -1 if it's not a proper list.
   This uses the "tortoise and hare" algorithm to detect "infinitely
   long" lists (i.e. lists with cycles in their cdrs), and returns -1
   if it does find one.  */
long scm_list_length(SOBJ sx)
{
  long i = 0;
  SOBJ tortoise = sx;
  SOBJ hare = sx;

  do {
	if (SCM_NULLP(hare))	return i;
	if (!SCM_PAIRP(hare)) 	return -1;
    hare = SCM_CDR(hare);
    i++;
	if (SCM_NULLP(hare))	return i;
	if (!SCM_PAIRP(hare)) 	return -1;
    hare = SCM_CDR(hare);
    i++;
    /* For every two steps the hare takes, the tortoise takes one.  */
    tortoise = SCM_CDR(tortoise);
  }
  while (hare != tortoise);

  /* If the tortoise ever catches the hare, then the list must contain
     a cycle.  */
  return -1;
}

/*S* (list? OBJ) => BOOLEAN */
/*D* Returns #t if obj is a list, otherwise returns #f. */
SOBJ scm_listp(SOBJ obj)
{
  return (scm_list_length(obj) >= 0 ? scm_true : scm_false);
}

/*S* (list OBJ ...) => LIST */
/*D* Returns a newly allocated list of its arguments.*/
SOBJ scm_list(int n, SOBJ *obja)
{
  SOBJ *p, list = NULL;
  p = obja + n;
  while (--p >= obja) {
    list = scm_cons(*p, list);
  }
  return (list);
}

/*S* (length LIST) => NUMBER */
/*D* Returns the length of LIST. */
SOBJ scm_length(SOBJ obj)
{
  int len;
  len = scm_list_length(obj);
  if (len < 0)
    SCM_ERR("length: not calculable", obj);
  return (SCM_MKINUM(len));
}

/*S* (append LIST ...) => LIST */
/*D* Returns a list consisting of the elements of the first list
  followed by the elements of the other lists. */
SOBJ scm_append(int len, SOBJ *l)
{
  SOBJ res,arg = NULL;
  SOBJ *last = &res, *limit;
 
  if (len == 0)		return(NULL);
  for (limit = l + len; l < limit; l++) {
	for (arg = *l; arg; arg = SCM_CDR(arg)) {
	  if (!SCM_PAIRP(arg)) {
		if (l < (limit - 1)) SCM_ERR("append: bad list", *l);
		break;
	  }
	  *last = scm_cons(SCM_CAR(arg), NULL);
	  last = &SCM_CDR(*last);
	}
  }
  *last = arg;
  return res;
}

/*E* (append2 LIST1 LIST2) => LIST */
/*D* Returns a list consisting of the elements of the LIST1 followed
  by the elements of the LIST2 */
SOBJ scm_append2(SOBJ l1, SOBJ l2)
{
  SOBJ arg[2];
  arg[0] = l1;  arg[1] = l2;
  return(scm_append(2, arg));
}

/*S* (reverse LIST) => LIST */
/*D* Returns a newly allocated list consisting of the elements of list in
  reverse order. */
SOBJ scm_reverse(SOBJ l)
{
  SOBJ n = NULL;
  while (l) {
    if (!SCM_PAIRP(l))
      SCM_ERR("reverse: bad list", l);
    n = scm_cons(SCM_CAR(l), n);
    l = SCM_CDR(l);
  }
  return (n);
}

/*S* (list-tail LIST K) => LIST */
/*D* Returns the sublist of LIST obtained by omitting the first K
  elements. It is an error if LIST has fewer than K elements. */
SOBJ scm_list_tail(SOBJ list, SOBJ k)
{
  SOBJ l;
  long x;

  if (!SCM_PAIRP(list))
    SCM_ERR("list-tail: bad list", list);
  if (!SCM_INUMP(k))
    SCM_ERR("list-tail: bad index", k);
  x = SCM_INUM(k);
  if (x < 0)
    SCM_ERR("list-tail: index must be exact positive integer", k);
  l = list;

  for (l = list; x > 0; x--) {
    if (l == NULL || !SCM_PAIRP(l))
      SCM_ERR("list-tail: list too short", list);
    l = SCM_CDR(l);
  }
  return l;
}

/*S* (list-ref LIST K) => OBJ */
/*D* Returns the Kth element of LIST. (This is the same as the car of
  (list-tail LIST K).)  It is an error if LIST has fewer than K
  elements. */
SOBJ scm_list_ref(SOBJ list, SOBJ k)
{
  SOBJ l;
  long x;

  if (!SCM_PAIRP(list))
    SCM_ERR("list-ref: Bad list", list);
  if (!SCM_INUMP(k))
    SCM_ERR("list-tail: bad index", k);
  x = SCM_INUM(k);
  if (x < 0)
    SCM_ERR("list-ref: index must be exact positive integer", k);
  l = list;

  for (; x > 0; x--) {
    if (l == NULL || !SCM_PAIRP(l))
      break;
    l = SCM_CDR(l);
  }
  if (l == NULL || !SCM_PAIRP(l))
    SCM_ERR("list-ref: list too short", list);

  return(SCM_CAR(l));
}

static SOBJ lmember(SOBJ obj, SOBJ list, SOBJ (*predicate) (SOBJ, SOBJ))
{
  register SOBJ ptr;

  if (!SCM_PAIRP(list) && !SCM_NULLP(list))
    SCM_ERR("member: bad list", list);
  for (ptr = list; !SCM_NULLP(ptr);) {
    if (SCM_PAIRP(ptr)) {
      if ((*predicate) (SCM_CAR(ptr), obj) == scm_true)
	return(ptr);
    } else
      /* end of a dotted list */
      return ((*predicate) (ptr, obj) == scm_true) ? ptr : scm_false;
    if ((ptr = SCM_CDR(ptr)) == list)
      SCM_ERR("member: circular list", NULL);
  }
  return(scm_false);
}

/*S* (memq OBJ LIST) => LIST | #f */
/*D* Return the first sublist of LIST whose car is OBJ. If OBJ does
  not occur in LIST, then #f is returned. Memq uses eq? to compare OBJ
  with the elements of LIST. */
SOBJ scm_memq(SOBJ obj, SOBJ list)
{
  return(lmember(obj, list, scm_eq));
}

/*S* (memv OBJ LIST) => LIST | #f */
/*D* Return the first sublist of LIST whose car is OBJ. If OBJ does
  not occur in LIST, then #f is returned. Memq uses eqv? to compare OBJ
  with the elements of LIST. */
SOBJ scm_memv(SOBJ obj, SOBJ list)
{
  return(lmember(obj, list, scm_eqv));
}

/*S* (member OBJ LIST) => LIST | #f */
/*D* Return the first sublist of LIST whose car is OBJ. If OBJ does
  not occur in LIST, then #f is returned. Memq uses equal? to compare OBJ
  with the elements of LIST. */
SOBJ scm_member(SOBJ obj, SOBJ list)
{
  return(lmember(obj, list, scm_equal));
}

static SOBJ lassoc(SOBJ obj, SOBJ alist, SOBJ (*predicate) (SOBJ, SOBJ))
{
  register SOBJ l, tmp;

  for (l = alist; SCM_PAIRP(l);) {
    tmp = SCM_CAR(l);
    if (SCM_PAIRP(tmp) && (*predicate) (SCM_CAR(tmp), obj) == scm_true)
      return tmp;

    if ((l = SCM_CDR(l)) == alist)
      SCM_ERR("assoc function: cirular list", alist);
  }
  if (!SCM_NULLP(l))
    SCM_ERR("assoc function: improper list", alist);

  return (scm_false);
}

/*S* (assq OBJ ALIST) => PAIR | #f */
/*D* Return the first PAIR in ALIST whose car field eq? OBJ. If no
  pair in ALIST has OBJ as its car, then #f is returned.*/
SOBJ scm_assq(SOBJ obj, SOBJ alist)
{
  return(lassoc(obj, alist, scm_eq));
}

/*S* (assv OBJ ALIST) => PAIR | #f */
/*D* Return the first PAIR in ALIST whose car field eqv? OBJ. If no
  pair in ALIST has OBJ as its car, then #f is returned.*/
SOBJ scm_assv(SOBJ obj, SOBJ alist)
{
  return(lassoc(obj, alist, scm_eqv));
}

/*S* (assoc OBJ ALIST) => PAIR | #f */
/*D* Return the first PAIR in ALIST whose car field equal? OBJ. If no
  pair in ALIST has OBJ as its car, then #f is returned.*/
SOBJ scm_assoc(SOBJ obj, SOBJ alist)
{
  return(lassoc(obj, alist, scm_equal));
}


SOBJ scm_map2(SOBJ func, int argc, SOBJ *argv, int map)
{
  SOBJ *code, res, *tmp, list;
  int i, j;

  code = alloca((argc + 6) * sizeof(SOBJ));
  code[0] = SCM_OPCODE(SCM_OP_MARK);
  code[1] = SCM_OPCODE(SCM_OP_PUSHN);
  code[2] = SCM_MKINUM(argc + 1);

  code[argc + 3] = func;
  code[argc + 4] = SCM_OPCODE(SCM_OP_CALL);
  code[argc + 5] = SCM_OPCODE(SCM_OP_END);

  list = NULL;  tmp = &list;
  while(argv[0]) {
	i = argc - 1;
	j = 3;
	while(i >= 0) {
	  if (argv[i] == NULL) 	SCM_ERR("list too short", argv[i]);
	  code[j] = SCM_CAR(argv[i]);
	  argv[i] = SCM_CDR(argv[i]);
	  i--;
	  j++;
	}
	res = scm_run_engine(code);
	if (map) { *tmp = scm_cons(res, NULL); tmp = &SCM_CDR(*tmp); }
  }
  return((map) ? list : scm_undefined);
}

/*S* (map PROC LIST1 LIST2 ...) => LIST */
/*D* The lists must be lists, and proc must be a procedure taking as
  many arguments as there are lists and returning a single value.  If
  more than one list is given, then they must all be the same
  length. Map applies PROC element-wise to the elements of the lists
  and returns a list of the results, in order. */
/*X* (map cadr '((a b) (d e) (g h)))   =>  (b e h) */

SOBJ scm_map(int argc, SOBJ *argv)
{
  SOBJ func;
  if (argc < 2)	return(NULL);
  func = argv[0];
  scm_sp = argv;
  /* need to save scm_sp, because scm_map2 restarts a new vm */
  return(scm_map2(func, argc-1, argv+1, TRUE));
}

/*S* (for-each proc list1 list2 ...) => #undefined */
/*D* The arguments to for-each are like the arguments to map, but
  for-each calls proc for its side effects rather than for its values.
  Unlike map, for-each is guaranteed to call proc on the elements of
  the lists in order from the first element(s) to the last, and the
  value returned by for-each is unspecified. */

SOBJ scm_foreach(int argc, SOBJ *argv)
{
  SOBJ func;
  if (argc < 2)	return(NULL);
  func = argv[0];
  scm_sp = argv;
  return(scm_map2(func, argc-1, argv+1, FALSE));
}

/*E* (nth K LIST) => OBJ */
/*D* Returns the Kth element of the LIST. */
SOBJ scm_nth(SOBJ n, SOBJ l)
{
  int i, limit;
  if (!SCM_INUMP(n)) 	SCM_ERR("bad index", n);
  limit = SCM_INUM(n);
  for (i = 0; (i < limit) && SCM_PAIRP(l); i++) {
	l = SCM_CDR(l);
  }
  if (!SCM_PAIRP(l)) SCM_ERR("bad list or index out of range", scm_cons(n,l));
  return(SCM_CAR(l));
}

/*E* (list-remove! LIST OBJ) => LIST */
/*D* Remove the pair containing OBJ from LIST. The original LIST is
  modified by this function. eqv? is used to locate the OBJ */
/*X* (list-remove! '(a b c d) 'c) => (a b d) */
SOBJ scm_list_remove(SOBJ list, SOBJ n)
{
  SOBJ l, last;

  for (last = NULL, l = list; SCM_PAIRP(l); last = l, l = SCM_CDR(l)) {
	if (scm_eqv(SCM_CAR(l), n) != scm_false) {
	  if (last) {
		SCM_CDR(last) = SCM_CDR(l);
	  } else {					/* first node */
		list = SCM_CDR(list);
	  }
	  SCM_CDR(l) = NULL;
	  break;
	}
  }
  return(list);
}

/*E* (list-replace! LIST OBJ NEW) => LIST */
/*D* Replace OBJ in LIST with the NEW object. The original LIST is
  modified by this function. eqv? is used to locate the OBJ */
/*X* (list-replace! '(a b c d) 'c 'k) => (a b k d) */
SOBJ scm_list_replace(SOBJ list, SOBJ item, SOBJ new)
{
  SOBJ l;
  
  for (l = list; SCM_PAIRP(l); l = SCM_CDR(l)) {
	if (scm_eqv(SCM_CAR(l), item) != scm_false) {
	  SCM_CAR(l) = new;
	  break;
	}
  }
  return(list);
}




void scm_init_list()
{
  scm_add_cprim("pair?"     , scm_pairp     , 1);
  scm_add_cprim("cons"      , scm_cons      , 2);
  scm_add_cprim("cons2"		, scm_cons2		, 3);
  scm_add_cprim("car"       , scm_car       , 1);
  scm_add_cprim("cdr"       , scm_cdr       , 1);
  scm_add_cprim("set-car!"  , scm_setcar    , 2);
  scm_add_cprim("set-cdr!"  , scm_setcdr    , 2);
  scm_add_cprim("caar"      , scm_caar      , 1);
  scm_add_cprim("cdar"      , scm_cdar      , 1);
  scm_add_cprim("cadr"      , scm_cadr      , 1);
  scm_add_cprim("cddr"      , scm_cddr      , 1);
  scm_add_cprim("caaar"     , scm_caaar     , 1);
  scm_add_cprim("cdaar"     , scm_cdaar     , 1);
  scm_add_cprim("cadar"     , scm_cadar     , 1);
  scm_add_cprim("cddar"     , scm_cddar     , 1);
  scm_add_cprim("caadr"     , scm_caadr     , 1);
  scm_add_cprim("cdadr"     , scm_cdadr     , 1);
  scm_add_cprim("caddr"     , scm_caddr     , 1);
  scm_add_cprim("cdddr"     , scm_cdddr     , 1);
  scm_add_cprim("caaaar"    , scm_caaaar    , 1);
  scm_add_cprim("cdaaar"    , scm_cdaaar    , 1);
  scm_add_cprim("cadaar"    , scm_cadaar    , 1);
  scm_add_cprim("cddaar"    , scm_cddaar    , 1);
  scm_add_cprim("caadar"    , scm_caadar    , 1);
  scm_add_cprim("cdadar"    , scm_cdadar    , 1);
  scm_add_cprim("caddar"    , scm_caddar    , 1);
  scm_add_cprim("cdddar"    , scm_cdddar    , 1);
  scm_add_cprim("caaadr"    , scm_caaadr    , 1);
  scm_add_cprim("cdaadr"    , scm_cdaadr    , 1);
  scm_add_cprim("cadadr"    , scm_cadadr    , 1);
  scm_add_cprim("cddadr"    , scm_cddadr    , 1);
  scm_add_cprim("caaddr"    , scm_caaddr    , 1);
  scm_add_cprim("cdaddr"    , scm_cdaddr    , 1);
  scm_add_cprim("cadddr"    , scm_cadddr    , 1);
  scm_add_cprim("cddddr"    , scm_cddddr    , 1);
  scm_add_cprim("null?"     , scm_nullp     , 1);
  scm_add_cprim("list?"     , scm_listp     , 1);
  scm_add_cprim("list*"     , scm_list      , -1);
  scm_add_cprim("length"    , scm_length    , 1);
  scm_add_cprim("append"    , scm_append    , -1);
  scm_add_cprim("append2"   , scm_append2    , 2);
  scm_add_cprim("reverse"   , scm_reverse   , 1);
  scm_add_cprim("list-tail" , scm_list_tail , 2);
  scm_add_cprim("list-ref"  , scm_list_ref  , 2);
  scm_add_cprim("memq"      , scm_memq      , 2);
  scm_add_cprim("memv"      , scm_memv      , 2);
  scm_add_cprim("member"    , scm_member    , 2);
  scm_add_cprim("assq"      , scm_assq      , 2);
  scm_add_cprim("assv"      , scm_assv      , 2);
  scm_add_cprim("assoc"     , scm_assoc     , 2);
  scm_add_cprim("map"		, scm_map		, -1);
  scm_add_cprim("for-each"	, scm_foreach	, -1);
  scm_add_cprim("nth"		, scm_nth		, 2);
  scm_add_cprim("list-remove!"	, scm_list_remove	, 2);
  scm_add_cprim("list-replace!"	, scm_list_replace	, 3);
}
