/* This file contains definitions for discouraged features.  When you
   discourage something, move it here when that is feasible.
*/

/* Copyright (C) 2003, 2004 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "libguile.h"

#if (SCM_ENABLE_DISCOURAGED == 1)

#define DEFFROM(t,f1,f2) SCM f1(t x) { return f2 (x); }
#define DEFTO(t,f1,f2)   t f1(SCM x, unsigned long pos, const char *s_caller) \
                           { return f2 (x); }

DEFFROM (short, scm_short2num, scm_from_short);
DEFFROM (unsigned short, scm_ushort2num, scm_from_ushort);
DEFFROM (int, scm_int2num, scm_from_int);
DEFFROM (unsigned int, scm_uint2num, scm_from_uint);
DEFFROM (long, scm_long2num, scm_from_long);
DEFFROM (unsigned long, scm_ulong2num, scm_from_ulong);
DEFFROM (size_t, scm_size2num, scm_from_size_t);
DEFFROM (ptrdiff_t, scm_ptrdiff2num, scm_from_ssize_t);

DEFTO (short, scm_num2short, scm_to_short);
DEFTO (unsigned short, scm_num2ushort, scm_to_ushort);
DEFTO (int, scm_num2int, scm_to_int);
DEFTO (unsigned int, scm_num2uint, scm_to_uint);
DEFTO (long, scm_num2long, scm_to_long);
DEFTO (unsigned long, scm_num2ulong, scm_to_ulong);
DEFTO (size_t, scm_num2size, scm_to_size_t);
DEFTO (ptrdiff_t, scm_num2ptrdiff, scm_to_ssize_t);

#if SCM_SIZEOF_LONG_LONG != 0
DEFFROM (long long, scm_long_long2num, scm_from_long_long);
DEFFROM (unsigned long long, scm_ulong_long2num, scm_from_ulong_long);
DEFTO   (long long, scm_num2long_long, scm_to_long_long);
DEFTO   (unsigned long long, scm_num2ulong_long, scm_to_ulong_long);
#endif

SCM
scm_make_real (double x)
{
  return scm_from_double (x);
}

double
scm_num2dbl (SCM a, const char *why)
{
  return scm_to_double (a);
}

SCM
scm_float2num (float n)
{
  return scm_from_double ((double) n);
}

SCM
scm_double2num (double n)
{
  return scm_from_double (n);
}

SCM
scm_make_complex (double x, double y)
{
  return scm_c_make_rectangular (x, y);
}

SCM
scm_mem2symbol (const char *mem, size_t len)
{
  return scm_from_locale_symboln (mem, len);
}

SCM
scm_mem2uninterned_symbol (const char *mem, size_t len)
{
  return scm_make_symbol (scm_from_locale_stringn (mem, len));
}

SCM
scm_str2symbol (const char *str)
{
  return scm_from_locale_symbol (str);
}


/* This function must only be applied to memory obtained via malloc,
   since the GC is going to apply `free' to it when the string is
   dropped.

   Also, s[len] must be `\0', since we promise that strings are
   null-terminated.  Perhaps we could handle non-null-terminated
   strings by claiming they're shared substrings of a string we just
   made up.  */
SCM
scm_take_str (char *s, size_t len)
{
  SCM answer = scm_from_locale_stringn (s, len);
  free (s);
  return answer;
}

/* `s' must be a malloc'd string.  See scm_take_str.  */
SCM
scm_take0str (char *s)
{
  return scm_take_locale_string (s);
}

SCM 
scm_mem2string (const char *src, size_t len)
{
  return scm_from_locale_stringn (src, len);
}

SCM
scm_str2string (const char *src)
{
  return scm_from_locale_string (src);
}

SCM 
scm_makfrom0str (const char *src)
{
  if (!src) return SCM_BOOL_F;
  return scm_from_locale_string (src);
}

SCM 
scm_makfrom0str_opt (const char *src)
{
  return scm_makfrom0str (src);
}


SCM
scm_allocate_string (size_t len)
{
  return scm_i_make_string (len, NULL);
}

void
scm_i_init_discouraged (void)
{
#include "libguile/discouraged.x"
}

#endif