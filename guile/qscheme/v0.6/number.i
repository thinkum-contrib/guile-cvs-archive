/* -*- c -*- */
/****************************************************************
 * number
 ****************************************************************/
/*S* (number? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a number, #f otherwise */
Prim(numberp, "number?", 1)
{
  RETURN( SCM_MKBOOL(SCM_NUMBERP(TOS)) );
}
/*S* (integer? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is an integer number, #f otherwise */
Prim(integerp, "integer?", 1)
{
  if (SCM_INUMP(TOS) || SCM_BNUMP(TOS)) { RETURN(scm_true); }
  if (SCM_FNUMP(TOS)) {
	double x = SCM_FNUM(TOS);
	RETURN(SCM_MKBOOL( floor(x) == x ));
  }
  RETURN(scm_false);
}

/*S* (real? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is an real number, #f otherwise.*/
Prim(realp, "real?", 1)
{
  RETURN(SCM_MKBOOL(SCM_NUMBERP(TOS)));
}

/*S* (complex? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is an complex number, #f otherwise.*/
Prim(complexp, "complex?", 1)
{
  RETURN(SCM_MKBOOL(SCM_NUMBERP(TOS)));
}

/*S* (rational? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is an rational number, #f otherwise.*/
Prim(rationalp, "rational?", 1)
{
  RETURN(SCM_MKBOOL(SCM_NUMBERP(TOS)));
}

/*S* (exact? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is an exact number, #f otherwise.*/
Prim(exactp, "exact?", 1)
{
  RETURN(SCM_MKBOOL( SCM_INUMP(TOS) || SCM_BNUMP(TOS) ));
}

/*S* (inexact? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is an inexact number, #f otherwise.*/
Prim(inexactp, "inexact?", 1)
{
  RETURN(SCM_MKBOOL( SCM_FNUMP(TOS) ));
}

#ifdef INUM_OPTIMIZATION
#define OPTIM_LOGOP(op) \
if (SCM_INUMP((long)n1 & (long)TOS)) { \
  if ((long)n1 op (long)TOS) { TOS=scm_true; NEXT; } TOS=scm_false; NEXT; }
#else
#define OPTIM_LOGOP(op)
#endif

Prim(lt2, "*i-n2<*", 2)			/* n2 n1 -- flag */
{
  SOBJ n1;  spop(n1);  OPTIM_LOGOP(<);   TOS = scm_lt2(n1, TOS);  NEXT;
}

Prim(le2, "*i-n2<=*", 2)		/* n2 n1 -- flag */
{
  SOBJ n1;  spop(n1);  OPTIM_LOGOP(<=);  TOS = scm_le2(n1, TOS);  NEXT;
}

Prim(ge2, "*i-n2>=*", 2)		/* n2 n1 -- flag */
{
  SOBJ n1;  spop(n1);  OPTIM_LOGOP(>=);  TOS = scm_ge2(n1, TOS);  NEXT;
}

Prim(gt2, "*i-n2>*", 2)			/* n2 n1 -- flag */
{
  SOBJ n1;  spop(n1);  OPTIM_LOGOP(>);  TOS = scm_gt2(n1, TOS);  NEXT;
}

Prim(eq2, "*i-n2=*", 2)
{
  SOBJ n1;  spop(n1);  OPTIM_LOGOP(==);  TOS = scm_eq2(n1, TOS);  NEXT;
}

#define GEN_LOGOP(op) \
{ while((void*)(&sp[1]) < (void*)cont) { \
	if (!(scm_cmpnum(TOS,sp[1]) op 0)) { VRETURN(scm_false); } \
	sdrop(); \
  } \
  VRETURN(scm_true); \
}

PrimVarargs(ltv, "*i-nv<*")
{
  GEN_LOGOP(<);
}
PrimVarargs(lev, "*i-nv<=*")
{
  GEN_LOGOP(<=);
}
PrimVarargs(gev, "*i-nv>=*")
{
  GEN_LOGOP(>=);
}
PrimVarargs(gtv, "*i-nv>*")
{
  GEN_LOGOP(>);
}
PrimVarargs(eqv, "*i-nv=*")
{
  GEN_LOGOP(==);
}

/*S* (zero? OBJ) => BOOLEAN */
/*D* Return #t if OBJ is zero, #f otherwise */
Prim(zerop, "zero?", 1)
{
  if (SCM_INUMP(TOS)) {	RETURN(SCM_MKBOOL(SCM_INUM(TOS) == 0)); }
  RETURN(scm_zerop(TOS));
}

/*S* (positive? OBJ) => BOOLEAN */
/*D* Return #t if OBJ is positive, #f otherwise */
Prim(positivep, "positive?", 1)
{
  if (SCM_INUMP(TOS)) {	RETURN(SCM_MKBOOL(SCM_INUM(TOS) > 0)); }
  RETURN(scm_positivep(TOS));
}

/*S* (negative? OBJ) => BOOLEAN */
/*D* Return #t if OBJ is negative, #f otherwise */
Prim(negativep, "negative?", 1)
{
  if (SCM_INUMP(TOS)) {	RETURN(SCM_MKBOOL(SCM_INUM(TOS) < 0)); }
  RETURN(scm_negativep(TOS));
}

/*S* (odd? OBJ) => BOOLEAN */
/*D* Return #t if OBJ is odd, #f otherwise */
Prim(oddp, "odd?", 1)
{
  if (SCM_INUMP(TOS)) {	RETURN(SCM_MKBOOL( (SCM_INUM(TOS) & 1) == 1)); }
  RETURN(scm_oddp(TOS));
}

/*S* (even? OBJ) => BOOLEAN */
/*D* Return #t if OBJ is even, #f otherwise */
Prim(evenp, "even?", 1)
{
  if (SCM_INUMP(TOS)) {	RETURN(SCM_MKBOOL( (SCM_INUM(TOS) & 1) == 0)); }
  RETURN(scm_evenp(TOS));
}

/*S* (min X1 X2 ...) => NUMBER */
/*D* Return the minimum of its arguments */
PrimVarargs(min, "min")
{
  if (NARGS < 1) SCM_ERR("max: wrong number of args", NULL);
  sp++;
  if (SCM_INUMP(TOS)) {
	while( ((void *)sp < (void*)cont) && SCM_INUMP(*sp)) {
	  if (SCM_INUM(TOS) > SCM_INUM(*sp)) TOS = *sp;
	  sp++;
	}
  }
  while((void *)sp < (void*)cont) {
	if (scm_cmpnum(TOS, *sp) > 0) { TOS = *sp; }
	sp++;
  }
  VRETURN(TOS);
}

/*S* (max X1 X2 ...) => NUMBER */
/*D* Return the maximum of its arguments */
PrimVarargs(max, "max")
{
  if (NARGS < 1) SCM_ERR("max: wrong number of args", NULL);
  sp++;
  if (SCM_INUMP(TOS)) {
	while( ((void *)sp < (void*)cont) && SCM_INUMP(*sp)) {
	  if (SCM_INUM(TOS) < SCM_INUM(*sp)) TOS = *sp;
	  sp++;
	}
  }
  while((void *)sp < (void*)cont) {
	if (scm_cmpnum(TOS, *sp) < 0) { TOS = *sp; }
	sp++;
  }
  VRETURN(TOS);
}

Prim(add2, "add2", 2)				/* n2 n1 -- n1+n2 */
{
  SOBJ n1;
  spop(n1);

#ifdef INUM_OPTIMIZATION
  if (SCM_INUMP((long)n1 & (long)TOS)) {
	long r = SCM_INUM(n1) + SCM_INUM(TOS);
	if (SCM_INUM_RANGE(r)) {  TOS = SCM_MKINUM(r);  NEXT; }
	TOS = scm_int2bnum(r);  NEXT;
  }
#endif
  TOS = scm_add2(n1, TOS);
  NEXT;
}

/*S* (+ N1 ...) => NUMBER */
/*D* return the sum of its arguments */
PrimVarargs(addv, "addv")
{
  if ((void*)sp >= (void*)cont) { VRETURN(SCM_MKINUM(0)); }
  sp++;
  if (SCM_INUMP(TOS)) {
	long sum = SCM_INUM(TOS);
	long r = 0;
	while((void*)sp < (void*)cont && SCM_INUMP(*sp)) {
	  r = sum + SCM_INUM(*sp);
	  if (!SCM_INUM_RANGE(r)) 	break;
	  sum = r;
	  sp++;
	}
	TOS = SCM_MKINUM(sum);
  }
  while((void*)sp < (void*)cont) {
	TOS = scm_add2(TOS, *sp++);
  }
  sp--;
  VRETURN(TOS);
}

Prim(mul2, "mul2", 2)			/* n2 n1 -- n1*n2 */
{
  SOBJ n1;

  spop(n1);

#ifdef INUM_OPTIMIZATION
  if (SCM_INUMP(n1) && SCM_INUMP(TOS)) {
	long r, x, y;
	if ( (x = SCM_INUM(n1)) == 0 || (y = SCM_INUM(TOS)) == 0) {
	  TOS = SCM_MKINUM(0);
	  NEXT;
	}
	r = x * y;
	if (y == (r / x)) {
	  TOS = SCM_MKINUM(r);
	  NEXT;
	}
  }
#endif
  TOS = scm_mul2(n1, TOS);
  NEXT;
}

/*S* (* N1 ...) => NUMBER */
/*D* Return the product of its arguments */
PrimVarargs(mulv, "mulv")
{
  if ((void*)sp >= (void*)cont) { VRETURN(SCM_MKINUM(1)); }
  sp++;
  if (SCM_INUMP(TOS)) {
	long sum = SCM_INUM(TOS);
	long r = 0;
	long n;
	while((void*)sp < (void*)cont && SCM_INUMP(*sp)) {
	  if ((n = SCM_INUM(*sp)) == 0) {
		sum = 0;  break;
	  }
	  r = sum * SCM_INUM(*sp);
	  if (sum != r / n)	break;
	  sum = r;
	  sp++;
	}
	TOS = SCM_MKINUM(sum);
  }
  while(SCM_INUM(TOS) != 0 && (void*)sp < (void*)cont) {
	TOS = scm_mul2(TOS, *sp++);
  }
  sp--;
  VRETURN(TOS);
}

Prim(sub2, "sub2", 2)		/* n2 n1 -- n1-n2 */
{
  SOBJ n1;

  spop(n1);
#ifdef INUM_OPTIMIZATION
  if (SCM_INUMP(n1) && SCM_INUMP(TOS)) {
	long r;
	r = SCM_INUM(n1) - SCM_INUM(TOS);
	if (SCM_INUM_RANGE(r)) {  TOS = SCM_MKINUM(r);  NEXT; }
	TOS = scm_int2bnum(r); NEXT;
  }
#endif
  TOS = scm_sub2(n1, TOS);
  NEXT;
}

/*S* (- N1 ...) => NUMBER */
/*D* Returns the difference of it's arguments. With one argument,
  return the additive inverse of the argument */
PrimVarargs(subv, "subv")
{
  if ((void*)sp >= (void*)cont) { VRETURN(SCM_MKINUM(0)); }

  if (NARGS == 1) {	spush(SCM_MKINUM(0)); }
  sp++;
  if (SCM_INUMP(TOS)) {
	long sum = SCM_INUM(TOS);
	long r = 0;
	while((void*)sp < (void*)cont && SCM_INUMP(*sp)) {
	  r = sum - SCM_INUM(*sp);
	  if (!SCM_INUM_RANGE(r)) 	break;
	  sum = r;
	  sp++;
	}
	TOS = SCM_MKINUM(sum);
  }
  while((void*)sp < (void*)cont) {
	TOS = scm_sub2(TOS, *sp++);
  }
  VRETURN(TOS);
}

Prim(div2, "div2", 2)			/* n2 n1 -- n1/n2 */
{
  SOBJ n1;
  spop(n1);
  TOS = scm_div2(n1, TOS);
  NEXT;
}

/*S* (/ N1 ...) => NUMBER */
/*D* Return the quotient of it's argument. With one argument, return
  the inverse of it's argument */
PrimVarargs(divv, "divv")
{
  if (NARGS < 1) 	SCM_ERR("/: bad number of args", NULL);
  if (NARGS == 1)	spush(SCM_MKINUM(1));
  sp++;
  while((void*)sp < (void*)cont) {
	TOS = scm_div2(TOS, *sp++);
  }
  VRETURN(TOS);
}

/*S* (abs X) => NUMBER */
/*D* Returns the absolute value of its argument. */
Prim(abs, "abs", 1)
{
  if (SCM_INUMP(TOS)) {
	if (SCM_INUM(TOS) < 0) { TOS=SCM_MKINUM( -(SCM_INUM(TOS))); NEXT; }
  }
  RETURN(scm_abs(TOS));
}

/*S* (quotient N1 N2) => INTEGER */
/*D* Returns the quotient of N1/N2 rounded toward zero. */
Prim(quotient, "quotient", 2)
{
  SOBJ x;
  spop(x);
  RETURN(scm_quotient(x, TOS));
}

/*S* (remainder N1 N2) => INTEGER */
/*D* Returns the quotient of N1/N2. */
Prim(remainder, "remainder", 2)
{
  SOBJ x;
  spop(x);
  RETURN(scm_remainder(x, TOS));
}

/*S* (modulo N1 N2) => NUMBER */
/*D* Returns the modulo of N1/N2. */
Prim(modulo, "modulo", 2)
{
  SOBJ x;
  spop(x);
  RETURN(scm_modulo(x, TOS));
}

/*S* (gcd N1 ...) => NUMBER */
/*D* Return the greatest common divisor of it's arguments. */
PrimVarargs(gcd, "gcd")
{
  supdate();  VRETURN(scm_gcd(NARGS, sp));
}

/*S* (lcm N1 ...) => NUMBER */
/*D* Return the least common multiple of it's arguments */
PrimVarargs(lcm, "lcm")
{
  supdate();  VRETURN(scm_lcm(NARGS, sp));
}

/*S* (floor N) => INTEGER */
/*D* Returns the largest integer not larger than N. */
Prim(floor, 	"floor", 1)
{
  RETURN(scm_floor(TOS));
}

/*S* (ceil N) => INTEGER */
/*D* Returns the smallest integer not smaller than N. */
Prim(ceil, 		"ceiling", 1) 	
{
  RETURN(scm_ceil(TOS)); 
}

/*S* (truncate N) => INTEGER */
/*D* Returns the integer closest to N whose absolute value is not
  larger than the absolute value of N. */
Prim(truncate, 	"truncate", 1) 	
{
  RETURN(scm_truncate(TOS)); 
}
/*S* (round N) => INTEGER */
/*D* Returns the closest integer to N, rounding to even when N is
  halfway between two integers. */
Prim(round, "round", 1)
{
  RETURN(scm_round(TOS));
}

/*S* (exp X) => NUMBER */
/*D* Returns the value of e (the base of natural logarithms) raised to
  the power of X. */
Prim(exp,		"exp",	1) 		
{
  RETURN(scm_exp(TOS)); 
}

/*S* (log X) => NUMBER */
/*D* Returns the natural logarithm of X. */
Prim(log,		"log",	1) 		
{
  RETURN(scm_log(TOS)); 
}

/*E* (log10 X) => NUMBER */
/*D* Returns the base-10 logarithm of X. */
Prim(log10,		"log10",	1) 		
{
  RETURN(scm_log10(TOS)); 
}

/*S* (sin X) => NUMBER */
/*D* Returns the sine of X, where X is given in radians. */
Prim(sin,		"sin",	1) 		
{
  RETURN(scm_sin(TOS)); 
}

/*S* (cos X) => NUMBER */
/*D* Returns the cosine of X, where X is given in radians. */
Prim(cos,		"cos",	1) 		
{
  RETURN(scm_cos(TOS)); 
}

/*S* (tan X) => NUMBER */
/*D* Returns the tangent of X, where X is given in radians. */
Prim(tan,		"tan",	1) 		
{
  RETURN(scm_tan(TOS)); 
}

/*S* (asin X) => NUMBER */
/*D* Returns the arc sine of X; that is the value whose sine is X. */
Prim(asin,		"asin",	1) 		
{
  RETURN(scm_asin(TOS)); 
}

/*S* (acos X) => NUMBER */
/*D* Returns the arc cosine of X; that is the value whose sine is X. */
Prim(acos,		"acos",	1) 		
{
  RETURN(scm_acos(TOS)); 
}

/*S* (atan X) => NUMBER */
/*D* Returns the arc tangent of X in radians. */
/*S* (atan Y X)  => NUMBER */
/*D* calculates the arc tangent of the two variables X and Y.  It is
  similar to calculating the arc tangent of Y / X, except that the
  signs of both arguments are used to determine the quadrant of the
  result. */
Prim(atan,		"atan",	2)
{
  SOBJ x;  spop(x);  RETURN(scm_atan(x, TOS));
}

/*S* (sqrt X) => NUMBER */
/*D* Returns the principal square root of X. */
Prim(sqrt,		"sqrt",	1)
{
  RETURN(scm_sqrt(TOS));
}

/*S* (expt X Y) => NUMBER */
/*D* Returns X raised to the power Y. */
Prim(expt,		"expt",	2)
{
  SOBJ x;  spop(x);  RETURN(scm_expt(x, TOS));
}

/*E* (random) => FLOAT */
/*D* Returns a random number in range 0-1.0. */
Prim(random,	"random", 	0)
{
  spush(scm_mkfnum(drand48()));
  NEXT;
}

/*S* (exact->inexact Z) => NUMBER */
/*D* Returns an inexact representation of Z. The value returned is the
  inexact number that is numerically closest to the argument. */
Prim(exact2inexact, "exact->inexact", 1)
{
  RETURN(scm_exact_to_inexact(TOS));
}

/*S* (inexact->exact z) => NUMBER */
/*D* Returns an exact representation of Z.  The value returned is the
  exact number that is numerically closest to the argument. */
Prim(inexact2exact, "inexact->exact", 1)
{
  RETURN(scm_inexact_to_exact(TOS));
}

/*S* (number->string Z [RADIX]) => STRING */
/*D* Returns as a string an external representation of the given number
  in the given radix */
PrimVarargs(number2string, "number->string")
{
  if (NARGS < 1) SCM_ERR("number->string: bad number of args", NULL);
  VRETURN(scm_number_to_string(TOS, (NARGS > 1) ? sp[1] : NULL));
}

/*S* (string->number STRING RADIX) => NUMBER */
/*D* Returns a number of the maximally precise representation expressed by
  the given string. */
PrimVarargs(string2number, "string->number")
{
  if (NARGS < 1) SCM_ERR("string->number: bad number of args", NULL);
  VRETURN(scm_string_to_number(TOS, (NARGS > 1) ? sp[1] : NULL));
}

/*E* (1+ X) => NUMBER*/
/*D* Returns X + 1. */
Prim(plus1, "1+", 1)
{
  if (SCM_INUMP(TOS) && (SCM_INUM(TOS) < SOBJ_INUM_MAX)) {
	(long)TOS += (1 << SOBJ_INUM_SHIFT);
	NEXT;
  }
  TOS = scm_add2(SCM_MKINUM(1), TOS);
  NEXT;
}

/*E* (2+ X) => NUMBER*/
/*D* Returns X + 2. */
Prim(plus2, "2+", 1)
{
  if (SCM_INUMP(TOS) && (SCM_INUM(TOS) < SOBJ_INUM_MAX)) {
	(long)TOS += (2 << SOBJ_INUM_SHIFT);
	NEXT;
  }
  TOS = scm_add2(SCM_MKINUM(2), TOS);
  NEXT;
}

/*E* (1- X) => NUMBER*/
/*D* Returns X - 1. */
Prim(minus1, "1-", 1)
{
  if (SCM_INUMP(TOS) && (SCM_INUM(TOS) > SOBJ_INUM_MIN)) {
	(long)TOS += (-1 << SOBJ_INUM_SHIFT);
	NEXT;
  }
  TOS = scm_sub2(TOS, SCM_MKINUM(1));
  NEXT;
}

/*E* (2- X) => NUMBER*/
/*D* Returns X - 1. */
Prim(minus2, "2-", 1)
{
  if (SCM_INUMP(TOS) && (SCM_INUM(TOS) > SOBJ_INUM_MIN)) {
	(long)TOS += (-2 << SOBJ_INUM_SHIFT);
	NEXT;
  }
  TOS = scm_sub2(TOS, SCM_MKINUM(2));
  NEXT;
}

