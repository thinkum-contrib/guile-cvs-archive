/*      Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


/* data initialization and C<->Scheme data conversion */

#include <stdio.h>

#include <gh.h>
#include <environments.h>

/* data conversion C->scheme */
SCM 
gh_int2scmb (int x)		/* this is being phased out */
{
  return (x ? SCM_BOOL_T : SCM_BOOL_F);
}
SCM 
gh_bool2scm (int x)
{
  return (x ? SCM_BOOL_T : SCM_BOOL_F);
}
SCM 
gh_int2scm (int x)
{
  return scm_long2num ((long) x);
}
SCM 
gh_ulong2scm (unsigned long x)
{
  return scm_ulong2num (x);
}
SCM 
gh_long2scm (long x)
{
  return scm_long2num (x);
}
SCM 
gh_double2scm (double x)
{
  return scm_makdbl (x, 0.0);
}
SCM 
gh_char2scm (char c)
{
 return SCM_MAKICHR (c);
}
SCM 
gh_str2scm (char *s, int len)
{
  return scm_makfromstr (s, len, 0);
}
SCM 
gh_str02scm (char *s)
{
  return scm_makfrom0str (s);
}
/* Copy LEN characters at SRC into the *existing* Scheme string DST,
   starting at START.  START is an index into DST; zero means the
   beginning of the string.

   If START + LEN is off the end of DST, signal an out-of-range
   error.  */
void 
gh_set_substr (char *src, SCM dst, int start, int len)
{
  char *dst_ptr;
  unsigned long dst_len;
  unsigned long effective_length;

  SCM_ASSERT (SCM_NIMP (dst) && SCM_STRINGP (dst), dst, SCM_ARG3,
	      "gh_set_substr");

  dst_ptr = SCM_CHARS (dst);
  dst_len = SCM_LENGTH (dst);
  SCM_ASSERT (len >= 0 && (unsigned) len <= dst_len,
	      dst, SCM_ARG4, "gh_set_substr");
  
  scm_protect_object (dst);
  effective_length = ((unsigned) len < dst_len) ? len : dst_len;
  memmove (dst_ptr + start, src, effective_length);
  scm_unprotect_object (dst);
}

/* Return the symbol named SYMBOL_STR.  */
SCM 
gh_symbol2scm (char *symbol_str)
{
  return SCM_CAR (scm_intern (symbol_str));
}

static SCM
makvect (char* m, int len, int type)
{
  SCM ans;
  SCM_NEWCELL (ans);
  SCM_DEFER_INTS;
  SCM_SETCHARS (ans, m);
  SCM_SETLENGTH (ans, len, type);
  SCM_ALLOW_INTS;
  return ans;
}

SCM
gh_ints2scm (int *d, int n)
{
  SCM *m;
  int i;
  m = (SCM*) scm_must_malloc (n * sizeof (SCM), "vector");
  for (i = 0; i < n; ++i)
    m[i] = (d[i] >= SCM_MOST_NEGATIVE_FIXNUM
	    && d[i] <= SCM_MOST_POSITIVE_FIXNUM
	    ? SCM_MAKINUM (d[i])
	    : scm_long2big (d[i]));
  return makvect ((char *) m, n, scm_tc7_vector);
}

SCM
gh_doubles2scm (double *d, int n)
{
  SCM *m = (SCM*) scm_must_malloc (n * sizeof (SCM), "vector");
  int i;
  for (i = 0; i < n; ++i)
    m[i] = scm_makdbl (d[i], 0.0);
  return makvect ((char *) m, n, scm_tc7_vector);
}

SCM
gh_chars2byvect (char *d, int n)
{
  char *m = scm_must_malloc (n * sizeof (char), "vector");
  memcpy (m, d, n * sizeof (char));
  return makvect (m, n, scm_tc7_byvect);
}

SCM
gh_shorts2svect (short *d, int n)
{
  char *m = scm_must_malloc (n * sizeof (short), "vector");
  memcpy (m, d, n * sizeof (short));
  return makvect (m, n, scm_tc7_svect);
}

SCM
gh_longs2ivect (long *d, int n)
{
  char *m = scm_must_malloc (n * sizeof (long), "vector");
  memcpy (m, d, n * sizeof (long));
  return makvect (m, n, scm_tc7_ivect);
}

SCM
gh_ulongs2uvect (unsigned long *d, int n)
{
  char *m = scm_must_malloc (n * sizeof (unsigned long), "vector");
  memcpy (m, d, n * sizeof (unsigned long));
  return makvect (m, n, scm_tc7_uvect);
}

#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
SCM
gh_floats2fvect (float *d, int n)
{
  char *m = scm_must_malloc (n * sizeof (float), "vector");
  memcpy (m, d, n * sizeof (float));
  return makvect (m, n, scm_tc7_fvect);
}
#endif

SCM
gh_doubles2dvect (double *d, int n)
{
  char *m = scm_must_malloc (n * sizeof (double), "vector");
  memcpy (m, d, n * sizeof (double));
  return makvect (m, n, scm_tc7_dvect);
}
#endif

/* data conversion scheme->C */
int 
gh_scm2bool (SCM obj)
{
  return ((obj) == SCM_BOOL_F) ? 0 : 1;
}
unsigned long 
gh_scm2ulong (SCM obj)
{
  return scm_num2ulong (obj, (char *) SCM_ARG1, "gh_scm2ulong");
}
long 
gh_scm2long (SCM obj)
{
  return scm_num2long (obj, (char *) SCM_ARG1, "gh_scm2long");
}
int 
gh_scm2int (SCM obj)
{
  /* NOTE: possible loss of precision here */
  return (int) scm_num2long (obj, (char *) SCM_ARG1, "gh_scm2int");
}
double 
gh_scm2double (SCM obj)
{
  return scm_num2dbl (obj, "gh_scm2double");
}
char 
gh_scm2char (SCM obj)
{
  return SCM_ICHR (obj);
}

/* Convert a vector, weak vector, string, substring or uniform vector
   into an array of chars.  If result array in arg 2 is NULL, malloc a
   new one. */
char *
gh_scm2chars (SCM obj, char *m)
{
  int i, n;
  long v;
  SCM val;
  if (!SCM_NIMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (SCM_INUMP (val))
	    {
	      v = SCM_INUM (val);
	      if (v < -128 || v > 255)
		scm_out_of_range (0, obj);
	    }
	  else
	    scm_wrong_type_arg (0, 0, obj);
	}
      if (m == 0)
	m = (char *) malloc (n * sizeof (char));
      for (i = 0; i < n; ++i)
	m[i] = SCM_INUM (SCM_VELTS (obj)[i]);
      break;
    case scm_tc7_byvect:
    case scm_tc7_string:
    case scm_tc7_substring:
      n = SCM_LENGTH (obj);
      if (m == 0)
	m = (char *) malloc (n * sizeof (char));
      memcpy (m, SCM_VELTS (obj), n * sizeof (char));
      break;
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   shorts.  If result array in arg 2 is NULL, malloc a new one. */
short *
gh_scm2shorts (SCM obj, short *m)
{
  int i, n;
  long v;
  SCM val;
  if (!SCM_NIMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (SCM_INUMP (val))
	    {
	      v = SCM_INUM (val);
	      if (v < -32768 || v > 65535)
		scm_out_of_range (0, obj);
	    }
	  else
	    scm_wrong_type_arg (0, 0, obj);
	}
      if (m == 0)
	m = (short *) malloc (n * sizeof (short));
      for (i = 0; i < n; ++i)
	m[i] = SCM_INUM (SCM_VELTS (obj)[i]);
      break;
    case scm_tc7_svect:
      n = SCM_LENGTH (obj);
      if (m == 0)
	m = (short *) malloc (n * sizeof (short));
      memcpy (m, SCM_VELTS (obj), n * sizeof (short));
      break;
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   longs.  If result array in arg 2 is NULL, malloc a new one. */
long *
gh_scm2longs (SCM obj, long *m)
{
  int i, n;
  SCM val;
  if (!SCM_NIMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (!SCM_INUMP (val) && !(SCM_NIMP (val) && SCM_BIGP (val)))
	    scm_wrong_type_arg (0, 0, obj);
	}
      if (m == 0)
	m = (long *) malloc (n * sizeof (long));
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  m[i] = SCM_INUMP (val) ? SCM_INUM (val) : scm_num2long (val, 0, 0);
	}
      break;
    case scm_tc7_ivect:
    case scm_tc7_uvect:
      n = SCM_LENGTH (obj);
      if (m == 0)
	m = (long *) malloc (n * sizeof (long));
      memcpy (m, SCM_VELTS (obj), n * sizeof (long));
      break;
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   floats.  If result array in arg 2 is NULL, malloc a new one. */
float *
gh_scm2floats (SCM obj, float *m)
{
  int i, n;
  SCM val;
  if (!SCM_NIMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (!SCM_INUMP (val)
	      && !(SCM_NIMP (val) && (SCM_BIGP (val) || SCM_REALP (val))))
	    scm_wrong_type_arg (0, 0, val);
	}
      if (m == 0)
	m = (float *) malloc (n * sizeof (float));
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (SCM_INUMP (val))
	    m[i] = SCM_INUM (val);
	  else if (SCM_BIGP (val))
	    m[i] = scm_num2long (val, 0, 0);
	  else
	    m[i] = SCM_REALPART (val);
	}
      break;
#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      n = SCM_LENGTH (obj);
      if (m == 0)
	m = (float *) malloc (n * sizeof (float));
      memcpy (m, (float *) SCM_VELTS (obj), n * sizeof (float));
      break;
#endif
    case scm_tc7_dvect:
      n = SCM_LENGTH (obj);
      if (m == 0)
	m = (float*) malloc (n * sizeof (float));
      for (i = 0; i < n; ++i)
	m[i] = ((double *) SCM_VELTS (obj))[i];
      break;
#endif
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   doubles.  If result array in arg 2 is NULL, malloc a new one. */
double *
gh_scm2doubles (SCM obj, double *m)
{
  int i, n;
  SCM val;
  if (!SCM_NIMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (!SCM_INUMP (val)
	      && !(SCM_NIMP (val) && (SCM_BIGP (val) || SCM_REALP (val))))
	    scm_wrong_type_arg (0, 0, val);
	}
      if (m == 0)
	m = (double *) malloc (n * sizeof (double));
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (SCM_INUMP (val))
	    m[i] = SCM_INUM (val);
	  else if (SCM_BIGP (val))
	    m[i] = scm_num2long (val, 0, 0);
	  else
	    m[i] = SCM_REALPART (val);
	}
      break;
#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      n = SCM_LENGTH (obj);
      if (m == 0)
	m = (double *) malloc (n * sizeof (double));
      for (i = 0; i < n; ++i)
	m[i] = ((float *) SCM_VELTS (obj))[i];
      break;
#endif
    case scm_tc7_dvect:
      n = SCM_LENGTH (obj);
      if (m == 0)
	m = (double*) malloc (n * sizeof (double));
      memcpy (m, SCM_VELTS (obj), n * sizeof (double));
      break;
#endif
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* string conversions between C and Scheme */

/* gh_scm2newstr() -- Given a Scheme string STR, return a pointer to a
   new copy of its contents, followed by a null byte.  If lenp is
   non-null, set *lenp to the string's length.

   This function uses malloc to obtain storage for the copy; the
   caller is responsible for freeing it.

   Note that Scheme strings may contain arbitrary data, including null
   characters.  This means that null termination is not a reliable way
   to determine the length of the returned value.  However, the
   function always copies the complete contents of STR, and sets
   *LEN_P to the true length of the string (when LEN_P is non-null).  */
char *
gh_scm2newstr (SCM str, int *lenp)
{
  char *ret_str;
  int len;

  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG3,
	      "gh_scm2newstr");

  /* protect str from GC while we copy off its data */
  scm_protect_object (str);

  len = SCM_LENGTH (str);

  ret_str = (char *) scm_must_malloc ((len + 1) * sizeof (char),
				      "gh_scm2newstr");
  /* so we copy tmp_str to ret_str, which is what we will allocate */
  memcpy (ret_str, SCM_ROCHARS (str), len);	/* test ROCHARS here -twp */
  /* now make sure we null-terminate it */
  ret_str[len] = '\0';

  scm_unprotect_object (str);

  if (lenp != NULL)
    {
      *lenp = len;
    }

  return ret_str;
}


/* Copy LEN characters at START from the Scheme string SRC to memory
   at DST.  START is an index into SRC; zero means the beginning of
   the string.  DST has already been allocated by the caller.

   If START + LEN is off the end of SRC, silently truncate the source
   region to fit the string.  If truncation occurs, the corresponding
   area of DST is left unchanged.  */
void 
gh_get_substr (SCM src, char *dst, int start, int len)
{
  int src_len, effective_length;
  SCM_ASSERT (SCM_NIMP (src) && SCM_ROSTRINGP (src), src, SCM_ARG3,
	      "gh_get_substr");

  scm_protect_object (src);
  src_len = SCM_LENGTH (src);
  effective_length = (len < src_len) ? len : src_len;
  memcpy (dst + start, SCM_ROCHARS (src), effective_length * sizeof (char));
  /* FIXME: must signal an error if len > src_len */
  scm_unprotect_object (src);
}


/* gh_scm2newsymbol() -- Given a Scheme symbol 'identifier, return a
   pointer to a string with the symbol characters "identifier",
   followed by a null byte.  If lenp is non-null, set *lenp to the
   string's length.

   This function uses malloc to obtain storage for the copy; the
   caller is responsible for freeing it. */
char *
gh_symbol2newstr (SCM sym, int *lenp)
{
  char *ret_str;
  int len;

  SCM_ASSERT (SCM_NIMP (sym) && SCM_SYMBOLP (sym), sym, SCM_ARG3,
	      "gh_scm2newsymbol");

  /* protect str from GC while we copy off its data */
  scm_protect_object (sym);

  len = SCM_LENGTH (sym);

  ret_str = (char *) scm_must_malloc ((len + 1) * sizeof (char),
				      "gh_symbol2newstr");
  /* so we copy tmp_str to ret_str, which is what we will allocate */
  memcpy (ret_str, SCM_CHARS (sym), len);
  /* now make sure we null-terminate it */
  ret_str[len] = '\0';

  scm_unprotect_object (sym);

  if (lenp != NULL)
    {
      *lenp = len;
    }

  return ret_str;
}


/* create a new vector of the given length, all initialized to the
   given value */
SCM
gh_make_vector (SCM len, SCM fill)
{
  return scm_make_vector (len, fill);
}

/* set the given element of the given vector to the given value */
SCM 
gh_vector_set_x (SCM vec, SCM pos, SCM val)
{
  return scm_vector_set_x (vec, pos, val);
}

/* retrieve the given element of the given vector */
SCM 
gh_vector_ref (SCM vec, SCM pos)
{
  return scm_vector_ref (vec, pos);
}

/* returns the length of the given vector */
unsigned long 
gh_vector_length (SCM v)
{
  return gh_scm2ulong (scm_vector_length (v));
}


/* uniform vector support */

/* returns the length as a C unsigned long integer */
unsigned long
gh_uniform_vector_length (SCM v)
{
  return gh_scm2ulong (scm_uniform_vector_length (v));
}

/* gets the given element from a uniform vector; ilist is a list (or
   possibly a single integer) of indices, and its length is the
   dimension of the uniform vector */
SCM
gh_uniform_vector_ref (SCM v, SCM ilist)
{
  return scm_uniform_vector_ref (v, ilist);
}

/* sets an individual element in a uniform vector */
/* SCM */
/* gh_list_to_uniform_array ( */


/* Data lookups between C and Scheme

   Look up a symbol with a given name, and return the object to which
   it is bound.  gh_lookup examines the Guile top level, and
   gh_module_lookup checks the module namespace specified by the
   `vec' argument.

   The return value is the Scheme object to which SNAME is bound, or
   SCM_UNDEFINED if SNAME is not bound in the given context.  */

SCM
gh_lookup (char *sname)
{
  SCM sym = gh_symbol2scm (sname);
  return scm_c_environment_ref(scm_interaction_environment, sym);
}

SCM
gh_module_lookup (SCM vec, char *sname)
{
  SCM sym = gh_symbol2scm (sname);
  return scm_c_environment_ref(vec, sym);
}
