/* -*- tab-width:4; -*- */
/*
 * Numbers
 */
#include "s.h"

#define SMALLNUMP(x)	(fabs(x) <= SOBJ_INUM_MAX)
#define ISINT(x)		(floor(x) == (x))

SOBJ scm_newbnum()
{
  SOBJ z = scm_newcell(SOBJ_T_BNUM);
  SCM_BNUM(z) = scm_must_alloc(sizeof(MP_INT));
  mpz_init(SCM_BNUM(z));
  return z;
}

void scm_bnum_sweep(SOBJ obj)
{
  fprintf(stderr, "freeing bignum at %p\n", obj);
  mpz_clear(SCM_BNUM(obj));
  scm_free(SCM_BNUM(obj));
}


SOBJ scm_mkinum(long x)
{
  return(SCM_MKINUM(x));
}

SOBJ scm_mkfnum(double x)
{
  SOBJ z = scm_newcell(SOBJ_T_FNUM); 
  SCM_FNUM(z) = x;
  return z;
}

/*-- conversion : The idea is to convert numbers if they are not of
 * the same class to an apropriate class suitable for future
 * operations */

/*-- convert to big */
SOBJ scm_int2bnum(long n)
{
  SOBJ z = scm_newcell(SOBJ_T_BNUM);
  SCM_BNUM(z) = scm_must_alloc(sizeof(MP_INT));
  mpz_init_set_si(SCM_BNUM(z), n);
  return z;
}

SOBJ scm_uint2bnum(unsigned long n)
{
  SOBJ z = scm_newcell(SOBJ_T_BNUM);
  SCM_BNUM(z) = scm_must_alloc(sizeof(MP_INT));
  mpz_init_set_ui(SCM_BNUM(z), n);
  return z;
}

static SOBJ flt2bnum(double n)
{
  SOBJ z = scm_newcell(SOBJ_T_BNUM);
  SCM_BNUM(z) = scm_must_alloc(sizeof(MP_INT));
  mpz_init_set_d(SCM_BNUM(z), n);
  return(z);
}

#ifdef NOT_USED
static SOBJ cstr2bnum(char *s)
{
  SOBJ z = scm_newcell(SOBJ_T_BNUM); 
  SCM_BNUM(z) = scm_must_alloc(sizeof(MP_INT));
  mpz_init_set_str(SCM_BNUM(z), s, 10);
  return z;
}
#endif

/*-- convert to flt */
static SOBJ int2fnum(long n)
{
  SOBJ z = scm_newcell(SOBJ_T_FNUM);
  SCM_FNUM(z) = (double) n;
  return(z);
}

static SOBJ big2fnum(MP_INT *n)
{
  SOBJ z = scm_newcell(SOBJ_T_FNUM);
  SCM_FNUM(z) = mpz_get_d(n);
  return(z);
}

SOBJ scm_flt2num(double n)
{	
  if (SCM_INUM_RANGE(n)) 
	return(SCM_MKINUM((long) n));

  return(flt2bnum(n));
}

SOBJ scm_int2num(long n)
{
  if (SCM_INUM_RANGE(n))
	return(SCM_MKINUM(n));
  return(scm_int2bnum(n));
}

SOBJ scm_uint2num(unsigned long n)
{
  if (n <= SOBJ_INUM_MAX)
	return(SCM_MKINUM(n));

  return(scm_uint2bnum(n));
}

double scm_number2double(SOBJ x)
{
  if (!SCM_NUMBERP(x)) 	SCM_ERR("scm_number2double: bad number", x);
  switch(SCM_OBJTYPE(x)) {
  case SOBJ_T_INUM:		return( (double) SCM_INUM(x) );
  case SOBJ_T_FNUM:		return( SCM_FNUM(x) );
  case SOBJ_T_BNUM:		return( mpz_get_d(SCM_BNUM(x)) );
  }
  return(-1);
}

long scm_number2long(SOBJ x)
{
  if (!SCM_NUMBERP(x)) 	SCM_ERR("scm_number2long: bad number", x);
  switch(SCM_OBJTYPE(x)) {
  case SOBJ_T_INUM:		return( SCM_INUM(x) );
  case SOBJ_T_FNUM:		return( (long)SCM_FNUM(x) );
  case SOBJ_T_BNUM:		return( mpz_get_ui(SCM_BNUM(x)) );
  }
  return(-1);
}

/*-- convert to same type : return the type */
static int scm_convert(SOBJ *px, SOBJ *py)
{
  SOBJ x = *px;
  SOBJ y = *py;

  if (SCM_OBJTYPE(x) == SCM_OBJTYPE(y))	return(SCM_OBJTYPE(x));

  if (SCM_OBJTYPE(x) == SOBJ_T_FNUM && ISINT(SCM_FNUM(x))) {
	*px = x = scm_flt2num(SCM_FNUM(x));
  }
  if (SCM_OBJTYPE(y) == SOBJ_T_FNUM && ISINT(SCM_FNUM(y))) {
	*py = y = scm_flt2num(SCM_FNUM(y));
  }

  if (SCM_OBJTYPE(x) == SCM_OBJTYPE(y))	return(SCM_OBJTYPE(x));

  switch(SCM_OBJTYPE(x)) {
  case SOBJ_T_INUM:
	switch(SCM_OBJTYPE(y)) {
	case SOBJ_T_BNUM: *px = scm_int2bnum(SCM_INUM(x));	return(SOBJ_T_BNUM);
	case SOBJ_T_FNUM: *px = int2fnum(SCM_INUM(x));		return(SOBJ_T_FNUM);
	}
  case SOBJ_T_BNUM:
	switch(SCM_OBJTYPE(y)) {
	case SOBJ_T_INUM: *py = scm_int2bnum(SCM_INUM(y));	return(SOBJ_T_BNUM);
	case SOBJ_T_FNUM: *px = big2fnum(SCM_BNUM(x));		return(SOBJ_T_FNUM);
	}
  case SOBJ_T_FNUM:
	switch(SCM_OBJTYPE(y)) {
	case SOBJ_T_INUM:	*py = int2fnum(SCM_INUM(y));	return(SOBJ_T_FNUM);
	case SOBJ_T_BNUM:	*py = big2fnum(SCM_BNUM(y));	return(SOBJ_T_FNUM);
	}
  }
  return(SOBJ_T_UNDEFINED);
}

/*-- choose the most compact representation of the number */
SOBJ scm_compact_number(SOBJ n)
{
  if (SCM_INUMP(n))	return(n);
  if (SCM_FNUMP(n) && ISINT(SCM_FNUM(n)))
	n = scm_flt2num(SCM_FNUM(n));

  if (SCM_INUMP(n))	return(n);
  switch(n->type) {
  case SOBJ_T_BNUM:
	if (mpz_cmp_si(SCM_BNUM(n), SOBJ_INUM_MIN) >= 0 &&
		mpz_cmp_si(SCM_BNUM(n), SOBJ_INUM_MAX) <= 0) {
	  n = SCM_MKINUM(mpz_get_si(SCM_BNUM(n)));
	}
	break;
  case SOBJ_T_FNUM:
	if (ISINT(SCM_FNUM(n))) 	n = scm_flt2num(SCM_FNUM(n));
	break;
  }
  return(n);
}

/****************************************************************
 * Arithmetic
 ****************************************************************/

/*-- add 2 number */
SOBJ scm_add2(SOBJ x, SOBJ y)
{
  SOBJ r;
  switch(scm_convert(&x, &y)) {
  case SOBJ_T_INUM: return(scm_int2num(SCM_INUM(x) + SCM_INUM(y)));
  case SOBJ_T_FNUM:	r = scm_mkfnum(SCM_FNUM(x) + SCM_FNUM(y)); 	break;
  case SOBJ_T_BNUM:
	  r = scm_newbnum();
	  mpz_add(SCM_BNUM(r), SCM_BNUM(x), SCM_BNUM(y));
	  break;

  default:
	return(scm_undefined);
  }
  return(scm_compact_number(r));
}

/*-- mul 2 number */
SOBJ scm_mul2(SOBJ x, SOBJ y)
{
  SOBJ r;
  switch(scm_convert(&x, &y)) {
  case SOBJ_T_FNUM:	r = scm_mkfnum(SCM_FNUM(x) * SCM_FNUM(y)); 	break;
  case SOBJ_T_INUM:
	{
	  long n;
	  
	  if (SCM_INUM(x) == 0 || SCM_INUM(y) == 0)	return(SCM_MKINUM(0));
	  n = SCM_INUM(x) * SCM_INUM(y);
	  if (SCM_INUM(x) == (n / SCM_INUM(y))) return(scm_int2num(n));

	  r = scm_mkfnum((double)SCM_INUM(x) * (double)SCM_INUM(y));
	  break;
	}
  case SOBJ_T_BNUM:
	  r = scm_newbnum();
	  mpz_mul(SCM_BNUM(r), SCM_BNUM(x), SCM_BNUM(y));
	  break;

  default:
	return(scm_undefined);
  }
  return(scm_compact_number(r));
}

/*-- sub 2 number */
SOBJ scm_sub2(SOBJ x, SOBJ y)
{
  SOBJ r;
  switch(scm_convert(&x, &y)) {
  case SOBJ_T_INUM: return(scm_int2num(SCM_INUM(x) - SCM_INUM(y)));
  case SOBJ_T_FNUM:	r = scm_mkfnum(SCM_FNUM(x) - SCM_FNUM(y)); 	break;
  case SOBJ_T_BNUM:
	  r = scm_newbnum();
	  mpz_sub(SCM_BNUM(r), SCM_BNUM(x), SCM_BNUM(y));
	  break;

  default:
	return(scm_undefined);
  }
  return(scm_compact_number(r));
}

/*-- div 2 numbers */
SOBJ scm_div2(SOBJ x, SOBJ y)
{
  SOBJ r;
  switch(scm_convert(&x, &y)) {
  case SOBJ_T_FNUM:	r = scm_mkfnum(SCM_FNUM(x) / SCM_FNUM(y)); 	break;
  case SOBJ_T_INUM:
	{
	  double tmp = (double)SCM_INUM(x) / (double)SCM_INUM(y);
	  if (ISINT(tmp)) {
		if (SCM_INUM_RANGE(tmp))	return(SCM_MKINUM((long)tmp));
		r = scm_newbnum();
		mpz_set_d(SCM_BNUM(r), tmp);
	  } else {
		r = scm_mkfnum(tmp);
	  }
	  break;
	}
  case SOBJ_T_BNUM:
	  r = scm_newbnum();
	  mpz_div(SCM_BNUM(r), SCM_BNUM(x), SCM_BNUM(y));
	  break;

  default:
	return(scm_undefined);
  }
  return(scm_compact_number(r));
}

/*-- compare 2 numbers */
int scm_cmpnum(SOBJ x, SOBJ y)
{
  int t;

  if (x == y) 	return(0);		/* quick out when eq */

  t = scm_convert(&x, &y);
  if (x == y) 	return(0);
  switch(t) {
  case SOBJ_T_INUM: 
	return(((long)x > (long)y) ? 1 : -1 );
  case SOBJ_T_FNUM:
	if (SCM_FNUM(x) == SCM_FNUM(y))	return(0);
	return((SCM_FNUM(x) > SCM_FNUM(y)) ? 1 : -1);
	
  case SOBJ_T_BNUM:
	return(mpz_cmp(SCM_BNUM(x), SCM_BNUM(y)));
  }
  return(SOBJ_INUM_MAX);
}

/*-- cmpops */
SOBJ scm_lt2(SOBJ x, SOBJ y) { return(SCM_MKBOOL(scm_cmpnum(x,y) <  0)); }
SOBJ scm_le2(SOBJ x, SOBJ y) { return(SCM_MKBOOL(scm_cmpnum(x,y) <= 0)); }
SOBJ scm_ge2(SOBJ x, SOBJ y) { return(SCM_MKBOOL(scm_cmpnum(x,y) >= 0)); }
SOBJ scm_gt2(SOBJ x, SOBJ y) { return(SCM_MKBOOL(scm_cmpnum(x,y) >  0)); }
SOBJ scm_eq2(SOBJ x, SOBJ y) { return(SCM_MKBOOL(scm_cmpnum(x,y) == 0)); }

SOBJ scm_zerop(SOBJ x)
{
  return(SCM_MKBOOL(scm_cmpnum(x, SCM_MKINUM(0)) == 0));
}

SOBJ scm_positivep(SOBJ x)
{
  return(SCM_MKBOOL(scm_cmpnum(x, SCM_MKINUM(0)) > 0));
}

SOBJ scm_negativep(SOBJ x)
{
  return(SCM_MKBOOL(scm_cmpnum(x, SCM_MKINUM(0)) < 0));
}

SOBJ scm_oddp(SOBJ x)
{
  if (!SCM_NUMBERP(x)) SCM_ERR("odd?: bad number", x);
  switch(SCM_OBJTYPE(x)) {
  case SOBJ_T_INUM:		return( SCM_MKBOOL( (SCM_INUM(x) & 1) == 1 ));
  case SOBJ_T_FNUM:		return( SCM_MKBOOL( fmod(SCM_FNUM(x),2.0) == 1.0 ));
  case SOBJ_T_BNUM:
	{
	  MP_INT q, r;
	  SOBJ res;
	  mpz_init(&q);  mpz_init(&r);
	  mpz_divmod_ui(&q, &r, SCM_BNUM(x), 2);
	  res = SCM_MKBOOL( mpz_cmp_ui(&r, 0) != 0);
	  mpz_clear(&q); mpz_clear(&r);
	  return(res);
	}
  }
  SCM_ERR("odd?: bad number", x);
  return(NULL);
}

SOBJ scm_evenp(SOBJ x)
{
  if (!SCM_NUMBERP(x)) SCM_ERR("even?: bad number", x);
  switch(SCM_OBJTYPE(x)) {
  case SOBJ_T_INUM:		return( SCM_MKBOOL( (SCM_INUM(x) & 1) == 0 ));
  case SOBJ_T_FNUM:		return( SCM_MKBOOL( fmod(SCM_FNUM(x),2.0) == 0.0 ));
  case SOBJ_T_BNUM:
	{
	  MP_INT q, r;
	  SOBJ res;
	  mpz_init(&q);  mpz_init(&r);
	  mpz_divmod_ui(&q, &r, SCM_BNUM(x), 2);
	  res = SCM_MKBOOL( mpz_cmp_ui(&r, 0) == 0);
	  mpz_clear(&q); mpz_clear(&r);
	  return(res);
	}
  }
  SCM_ERR("even?: bad number", x);
  return(NULL);
}

SOBJ scm_abs(SOBJ x)
{
  if (!SCM_NUMBERP(x)) SCM_ERR("abs: bad number", x);
  switch(SCM_OBJTYPE(x)) {
  case SOBJ_T_INUM:
	return( SCM_INUM(x) < 0 ? SCM_MKINUM(-(SCM_INUM(x))) : x);
  case SOBJ_T_FNUM:
	return( SCM_FNUM(x) < 0.0? scm_mkfnum( -(SCM_FNUM(x)) ) : x);
  case SOBJ_T_BNUM:
	if (mpz_cmp_ui(SCM_BNUM(x), 0) < 0) {
	  SOBJ new = scm_newbnum();
	  mpz_neg(SCM_BNUM(new), SCM_BNUM(x));
	  return(new);
	}
	return(x);
  }
  return(NULL);
}

SOBJ scm_do_int_div(SOBJ x, SOBJ y, int quotient)
{
  MP_INT q, r;
  SOBJ res;
  int exact = TRUE;

  if (SCM_FNUMP(x)) {
	if (!ISINT(SCM_FNUM(x)))	SCM_ERR("bad integer", x);
	x = scm_flt2num(SCM_FNUM(x));  exact = FALSE;
  }
  if (SCM_FNUMP(y)) {
	if (!ISINT(SCM_FNUM(y)))	SCM_ERR("bad integer", y);
	y = scm_flt2num(SCM_FNUM(y));  exact = FALSE;
  }
  
  if (SCM_INUMP(x) && SCM_INUMP(y)) {
	long r = quotient ?
	  SCM_INUM(x) / SCM_INUM(y) :
	  SCM_INUM(x) % SCM_INUM(y) ;

	return( exact ? SCM_MKINUM(r) : scm_mkfnum(r));
  }
  /* at least one of the 2 number is big */
  if (SCM_INUMP(x))  x = scm_int2bnum(SCM_INUM(x));
  if (SCM_INUMP(y))  y = scm_int2bnum(SCM_INUM(y));

  res = scm_newbnum();
  mpz_init(&q); mpz_init(&r);
  /*  mpz_divmod(&q, &r, SCM_BNUM(x), SCM_BNUM(y)); */
  mpz_tdiv_qr(&q, &r, SCM_BNUM(x), SCM_BNUM(y));
  mpz_set(SCM_BNUM(res), quotient ? &q : &r);
  mpz_clear(&q); mpz_clear(&r);
  return( exact ? res : big2fnum(SCM_BNUM(res)));
}

SOBJ scm_quotient(SOBJ x, SOBJ y)
{
  if (!SCM_NUMBERP(x) && !SCM_NUMBERP(y))
	SCM_ERR("quotient: bad arguments", scm_cons(x, y));

  if (scm_zerop(y) == scm_true) SCM_ERR("quotient: division by 0", NULL);

  return(scm_do_int_div(x, y, TRUE));
}

SOBJ scm_remainder(SOBJ x, SOBJ y)
{
  if (!SCM_NUMBERP(x) && !SCM_NUMBERP(y))
	SCM_ERR("remainder: bad arguments", scm_cons(x, y));

  if (scm_zerop(y) == scm_true) SCM_ERR("remainder: division by 0", NULL);
  return(scm_do_int_div(x, y, FALSE));
}

SOBJ scm_modulo(SOBJ x, SOBJ y)
{
  SOBJ z;

  if (!SCM_NUMBERP(x) && !SCM_NUMBERP(y))
	SCM_ERR("modulo: bad arguments", scm_cons(x, y));

  if (scm_zerop(y) == scm_true) SCM_ERR("modulo: division by 0", NULL);

  z = scm_do_int_div(x, y, FALSE);
  if (scm_negativep(x) != scm_negativep(y) && scm_zerop(z) != scm_true) {
	z = scm_add2(z, y);
  }
  return(z);
}

static SOBJ scm_gcd2(SOBJ x, SOBJ y)
{
  return( scm_zerop(y) == scm_true ? x : scm_gcd2(y, scm_modulo(x,y)));
}

SOBJ scm_gcd(int nargs, SOBJ *obj)
{
  int i=0;
  SOBJ res = SCM_MKINUM(0);
  while(i < nargs) {
	if (!SCM_NUMBERP(obj[i])) 	SCM_ERR("gcd: bad number", obj[i]);
	res = (i == 0) ?
	  scm_abs(obj[i++]) : 
	  scm_abs(scm_gcd2(res, obj[i++]));
  }
  return(res);
}

SOBJ scm_lcm(int nargs, SOBJ *obj)
{
  int i=0;
  SOBJ res = SCM_MKINUM(1);
  while(i < nargs) {
	if (!SCM_NUMBERP(obj[i])) 	SCM_ERR("lcm: bad number", obj[i]);
	if (i == 0) {
	  res = scm_abs(obj[i]);
	} else {
	  if (scm_zerop(obj[i]) == scm_true) 	return(obj[i]);
	  res = scm_abs(scm_div2(scm_mul2(res, obj[i]), scm_gcd2(res, obj[i])));
	}
	i++;
  }
  return(res);
}

/*-- float specific */

SOBJ scm_floor(SOBJ x)
{
  if (!SCM_NUMBERP(x))	SCM_ERR("floor: bad number", x);
  if (SCM_FNUMP(x))		return(scm_mkfnum(floor(SCM_FNUM(x))));
  return(x);
}

SOBJ scm_ceil(SOBJ x)
{
  if (!SCM_NUMBERP(x))	SCM_ERR("ceil: bad number", x);
  if (SCM_FNUMP(x))		return(scm_mkfnum(ceil(SCM_FNUM(x))));
  return(x);
}

SOBJ scm_truncate(SOBJ x)
{
  if (!SCM_NUMBERP(x))	SCM_ERR("truncate: bad number", x);
  if (SCM_FNUMP(x)) {
	double d = SCM_FNUM(x);
	return(scm_mkfnum( d < 0.0 ? ceil(d) : floor(d) ));
  }
  return(x);
}

SOBJ scm_round(SOBJ x)
{
  if (!SCM_NUMBERP(x))	SCM_ERR("truncate: bad number", x);
  if (SCM_FNUMP(x)) {
	double v 	      = SCM_FNUM(x);
    double v_plus_0_5 = v + 0.5;
    double res        = floor(v_plus_0_5);

    return(scm_mkfnum(
      (v_plus_0_5==res && v_plus_0_5/2 != floor(v_plus_0_5/2)) ? res-1: res));
  }
  return(x);
}

static double exact2inexact(SOBJ x)
{
  if (!SCM_NUMBERP(x)) 	SCM_ERR("exact->inexact: bad number", x);
  switch(SCM_OBJTYPE(x)) {
  case SOBJ_T_INUM:		return( (double) SCM_INUM(x));
  case SOBJ_T_FNUM:		return( SCM_FNUM(x));
  case SOBJ_T_BNUM:		return( mpz_get_d(SCM_BNUM(x)));
  }
  return(0);
}

#define TRANS(fn) \
SOBJ scm_##fn(SOBJ x) { \
  if (SCM_NUMBERP(x)) return(scm_mkfnum(fn(exact2inexact(x)))); \
  SCM_ERR("trans: bad number", x); return(NULL);\
}

TRANS(exp);
TRANS(log);
TRANS(log10);
TRANS(sin);
TRANS(cos);
TRANS(tan);
TRANS(asin);
TRANS(acos);

SOBJ scm_atan(SOBJ x, SOBJ y)
{
  double dx, dy;
  
  if (!SCM_NUMBERP(x))	SCM_ERR("atan: bad number", x);
  if (!SCM_NUMBERP(y))	return(scm_mkfnum(atan(exact2inexact(y))));
  
  dx = exact2inexact(x);		dy = exact2inexact(y);
  return(scm_mkfnum( (dx == 0 && dy == 0) ? 0 : atan2(dx, dy)));
}

SOBJ scm_sqrt(SOBJ x)
{
  double d;

  if (!SCM_NUMBERP(x)) 	SCM_ERR("sqrt: bad number", x);
  switch(SCM_OBJTYPE(x)) {
  case SOBJ_T_INUM:
	if (SCM_INUM(x) < 0)	goto errneg;
	d = sqrt(SCM_INUM(x));
	return( ISINT(d) ? SCM_MKINUM( (long)d) : scm_mkfnum(d) );

  case SOBJ_T_FNUM:
	if (SCM_FNUM(x) < 0.0)	goto errneg;
	return(scm_mkfnum(sqrt(SCM_FNUM(x))));
	
  case SOBJ_T_BNUM: {
	MP_INT root, rem;
	SOBJ res = NULL;
	
	if (mpz_cmp_si(SCM_BNUM(x), 0) < 0)	goto errneg;
	mpz_init(&root); mpz_init(&rem);
	mpz_sqrtrem(&root, &rem, SCM_BNUM(x));
	if (mpz_cmp_si(&rem, 0) == 0) {
	  res = scm_newbnum();
	  mpz_set(SCM_BNUM(res),&root);
	} else {
	  res = big2fnum(&root);
	}
	mpz_clear(&root);	  mpz_clear(&rem);
	return(res);
  }
  }
 errneg:
  SCM_ERR("sqrt: negative number", x);
  return(NULL);
}

SOBJ scm_exact_expt(SOBJ x, SOBJ y)
{
  MP_INT mod;
  SOBJ new = scm_newbnum();
  
  mpz_init_set_ui(&mod, 1);
  mpz_powm(SCM_BNUM(new), SCM_BNUM(x), SCM_BNUM(y), &mod);
  mpz_clear(&mod);
  return(new);
}

SOBJ scm_expt(SOBJ x, SOBJ y)
{
  if (!SCM_NUMBERP(x))	SCM_ERR("expt: bad number", x);
  if (!SCM_NUMBERP(y))	SCM_ERR("expt: bad number", y);
 
  switch(scm_convert(&x, &y)) {
  case SOBJ_T_INUM: {
	SOBJ new = scm_newbnum();
	mpz_ui_pow_ui(SCM_BNUM(new), SCM_INUM(x), SCM_INUM(y));
	return(scm_compact_number(new));
  }
  case SOBJ_T_FNUM:
	return(scm_mkfnum(pow(SCM_FNUM(x), SCM_FNUM(y))));
	
  case SOBJ_T_BNUM: {
	SOBJ new = scm_newbnum();
	MP_INT mod;
	mpz_init_set_ui(&mod, 1);
	mpz_powm(SCM_BNUM(new), SCM_BNUM(x), SCM_BNUM(y), &mod);
	mpz_clear(&mod);
	return(new);
  }
  }
  return(NULL);
}

SOBJ scm_exact_to_inexact(SOBJ x)
{
  return(scm_mkfnum(exact2inexact(x)));
}

SOBJ scm_inexact_to_exact(SOBJ x)
{
  if (!SCM_NUMBERP(x)) 	SCM_ERR("inexact->exact: bad number", x);
  if (SCM_FNUMP(x))	return(scm_flt2num(SCM_FNUM(x)));
  return(x);
}

/*-- return mallocated string */
SCM_STRBUF *scm_number2cstr(SOBJ n, long base)
{
  SCM_STRBUF *sb;
  char buf[128];
  int len;

  switch(SCM_OBJTYPE(n)) {
  case SOBJ_T_FNUM:
	if (base != 10) SCM_ERR("base must be 10 for this number", n);
	sb = scm_strbuf_sprintf(scm_strbuf_new(8), "%.15g", SCM_FNUM(n));
	return(sb);

  case SOBJ_T_INUM:
	{
	  char *p;
	  long value;
	  int  digit;
	  
	  if (base < 1 || base > 36) SCM_ERR("bad base (valid range 1..36)", n);

	  p = buf + sizeof(buf);	  
	  *(--p) = 0;
	  value = SCM_INUM(n);
	  if (value < 0) value = -value;
	  do {
		digit = value % base;
		*(--p) = digit + ((digit < 10) ? '0' : 'a'-10);
		value = value / base;
	  } while(value != 0);
	  if (SCM_INUM(n) < 0) {
		*(--p) = '-';
	  }
	  len = strlen(p);
	  sb = scm_strbuf_new(len);
	  strcpy(sb->str, p);
	  return(sb);
	}
  case SOBJ_T_BNUM:
	{
	  char *s;
	
	  if (base < 1 || base > 36) SCM_ERR("bad base (valid range 1..36)", n);
	  s = mpz_get_str(NULL, base, SCM_BNUM(n));
	  len = strlen(s);
	  sb = scm_strbuf_new(len);
	  strcpy(sb->str, s);
	  free(s);
	  return(sb);
	}
  }
  return NULL; /* never reached */
}

SOBJ scm_cstr2number(char *s, int base)
{
  int exact, isint;
  char *p;
  
  exact = 0;
  if (*s == '#') {				/* got specs */
	s++;
	while(*s) {
	  switch(tolower(*s)) {
	  case 'e':	exact = 1;		s++;  continue;
	  case 'i':	exact = -1;		s++;  continue;
	  case 'b': base = 2;		s++;  continue;
	  case 'o': base = 8;		s++;  continue;
	  case 'd': base = 10;		s++;  continue;
	  case 'x': base = 16;		s++;  continue;
	  }
	  break;
	}
  }
  if (*s == 0) 	return(scm_false);

  if (exact == -1 && base != 10)  SCM_ERR("inexact number, only base 10", NULL);

  if (*s == '+') s++;

  isint = 1; p = s;
  while(*p) {
	if ( *p == '-' || *p == '+') {
	  p++;
	  continue;
	}
	if ( (base==2 && (*p == '0' || *p == '1')) ||
		 (base==8  && (strchr("01234567", *p) != NULL)) ||
		 (base==10 && isdigit( *p)) ||
		 (base==16 && isxdigit(*p))) {
	  p++;
	  continue;
	}
	if (strchr("#.Ee", *p)) { /* float */
	  if (exact == 1) SCM_ERR("illegal . in exact number", NULL);
	  isint = 0;
	  if (*p == '#') *p = '0';
	  p++;
	  continue;
	}
	if (strchr("sSfFdDlL", *p)) { /* precision marker ignored */
	  *p = 0; break;
	}
	scm_puts("strange char '"); scm_putc(*p);  scm_puts("' in number\n");
	return(scm_false);
  }
  if (isint && exact != -1) {
	MP_INT n;
	SOBJ   z;
	if (mpz_init_set_str(&n, s, base) < 0) {
	  mpz_clear(&n);
	  return(scm_false);
	}
	if (mpz_cmp_si(&n, SOBJ_INUM_MIN) >= 0 && 
		mpz_cmp_si(&n, SOBJ_INUM_MAX) <= 0) {
	  long num = mpz_get_si(&n);
	  mpz_clear(&n);
	  return(SCM_MKINUM(num));
	}
	z = scm_newbnum();
	mpz_set(SCM_BNUM(z), &n);
	return z;
  } else {
	return(scm_mkfnum(atof(s)));
  }
}

SOBJ scm_number_to_string(SOBJ x, SOBJ y)
{
  int base;
  SOBJ new;
  
  if (!SCM_NUMBERP(x)) 	SCM_ERR("number->string: bad number", x);
  if (y == NULL) {
	base = 10;
  } else {
	if (!SCM_INUMP(y)) 	SCM_ERR("number->string: bad base", y);
	base = SCM_INUM(y);
  }

  new = scm_newcell(SOBJ_T_STRING);
  SCM_STR(new) = scm_number2cstr(x, base);
  return(new);
}

SOBJ scm_string_to_number(SOBJ x, SOBJ y)
{
  int base;
  
  if (!SCM_STRINGP(x)) SCM_ERR("string->number: bad string", x);
  if (y == NULL) {
	base = 10;
  } else {
	if (!SCM_INUMP(y)) SCM_ERR("string->number: bad base", y);
	base = SCM_INUM(y);
  }
  return(scm_cstr2number(SCM_STR_VALUE(x), base));
}

/****************************************************************
 * Bit operations
 ****************************************************************/
/*E* (ashift NUMBER POS) => NUMBER */
/*D* Return NUMBER shifted by POS bits. If POS is positive, shift to
  the left, else shift to the right. Equivalent to NUMBER * expt(2,
  POS) */
SOBJ scm_ashift(SOBJ val, SOBJ n)
{
  long m, k;
  m = scm_number2long(val);
  k = scm_number2long(n);
  if (k > 0)	m = m << k;
  else			m = m >> (-k);
  return(scm_int2num(m));
}

/*E* (bit-and N ...) => NUMBER */
/*D* Returns the logical and of it's argument. */

SOBJ scm_bit_and(int narg, SOBJ arg[])
{
  int i;
  long r;
  
  if (narg < 1)	return(scm_undefined);
  r = scm_number2long(arg[0]);
  for (i = 1; i < narg; i++) r &= scm_number2long(arg[i]);
  return(scm_int2num(r));
}

/*E* (bit-or N ...) => NUMBER */
/*D* Returns the logical or of it's argument. */

SOBJ scm_bit_or(int narg, SOBJ arg[])
{
  int i;
  long r;
  
  if (narg < 1)	return(scm_undefined);
  r = scm_number2long(arg[0]);
  for (i = 1; i < narg; i++) r |= scm_number2long(arg[i]);
  return(scm_int2num(r));
}

/*E* (bit-xor N ...) => NUMBER */
/*D* Returns the logical exclusive or of it's argument. */

SOBJ scm_bit_xor(int narg, SOBJ arg[])
{
  int i;
  long r;
  
  if (narg < 1)	return(scm_undefined);
  r = scm_number2long(arg[0]);
  for (i = 1; i < narg; i++) r ^= scm_number2long(arg[i]);
  return(scm_int2num(r));
}

/*E* (bit-not N) => NUMBER */
/*D* Returns the logical not of N. */

SOBJ scm_bit_not(SOBJ n)
{
  return(scm_int2num(~(scm_number2long(n))));
}

/****************************************************************
 * scheme interface
 ****************************************************************/

void scm_init_number()
{
  scm_add_cprim("ashift", 	scm_ashift,		2);
  scm_add_cprim("bit-and", 	scm_bit_and,	-1);
  scm_add_cprim("bit-or", 	scm_bit_or,		-1);
  scm_add_cprim("bit-xor", 	scm_bit_xor,	-1);
  scm_add_cprim("bit-not", 	scm_bit_not,	1);
}
