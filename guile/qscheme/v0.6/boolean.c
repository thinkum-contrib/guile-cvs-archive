/* -*- tab-width:4; -*- */
/*
 * The booleans
 */

#include "s.h"

/*S* (not OBJ) => BOOLEAN */
/*D* Returns #t if obj is false, and returns #f otherwise. */
SOBJ scm_not(SOBJ x)
{
  return SCM_EQ(x, scm_false) ? scm_true : scm_false;
}

/*S* (boolean? OBJ) => BOOLEAN */
/*D* Returns #t if obj is either #t or #f and returns #f otherwise. */
SOBJ scm_booleanp(SOBJ x)
{
  return SCM_BOOLEANP(x) ? scm_true: scm_false;
}

/*S* (eq? OBJ1 OBJ2) => BOOLEAN */
/*D* Returns #t if OBJ1 and OBJ2 represents the same cell. */
SOBJ scm_eq(SOBJ x, SOBJ y)
{
  return( (x == y) ? scm_true : scm_false );
}
/*S* (eqv? OBJ1 OBJ2) => BOOLEAN */
/*D* Returns #t if OBJ1 and OBJ2 should normally be regarded as the
  same object, #f otherwise. */
SOBJ scm_eqv(SOBJ x, SOBJ y)
{
  if (x == y)  return(scm_true);

  if ((SCM_SYMBOLP(x) || SCM_ATOMP(x)) && (SCM_SYMBOLP(y) || SCM_ATOMP(y))) {
	if (SCM_SYMBOLP(x)) x = SCM_SYM_NAME(x);
	if (SCM_SYMBOLP(y)) y = SCM_SYM_NAME(y);
	return(SCM_MKBOOL(x == y));
  }
  if (SCM_FNUMP(x) && SCM_FNUMP(y)) {
	return(SCM_MKBOOL(SCM_FNUM(x) == SCM_FNUM(y)));
  }
  if (SCM_EXACTP(x) && SCM_EXACTP(y)) {
	return(SCM_MKBOOL(scm_cmpnum(x,y) == 0));
  }
  return(scm_false);
}

/*S* (equal? obj1 obj2) => BOOLEAN */
/*D* Equal? recursively compares the contents of pairs, vectors, and
  strings, applying eqv? on other objects such as numbers and symbols.
  A rule of thumb is that objects are generally equal? if they print
  the same.  Equal? may fail to terminate if its arguments are
  circular data structures. */
SOBJ scm_equal(SOBJ x, SOBJ y)
{
  int t;
Top:
  if (scm_eqv(x, y) == scm_true) return scm_true;
  
  switch (SCM_OBJTYPE(x)) {
  case SOBJ_T_PAIR:
	if (SCM_PAIRP(y)) {
	  if (scm_equal(SCM_CAR(x), SCM_CAR(y)) == scm_false) return scm_false;
	  x = SCM_CDR(x); y = SCM_CDR(y);
	  goto Top;
	}
	break;
  default:
	if ((t = SCM_OBJTYPE(x)) == SCM_OBJTYPE(y)) {
	  t = SCM_OBJTYPE(x);
	  if (scm_type_hook[t].compare) {
		return((*scm_type_hook[t].compare) (x,y));
	  }
	}
  }
  return scm_false;
}

void scm_init_boolean()
{
  /* this are inlined 
  scm_add_cprim("not",			scm_not, 		1);
  scm_add_cprim("boolean?",		scm_booleanp, 	1);
  scm_add_cprim("eq?",			scm_eq,			2);
  */
  scm_add_cprim("eqv?",			scm_eqv,		2);
  scm_add_cprim("equal?",		scm_equal,		2);
}
