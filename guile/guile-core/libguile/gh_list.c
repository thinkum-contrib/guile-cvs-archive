/*      Copyright (C) 1995,1996,1997, 2000 Free Software Foundation, Inc.

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


/* list manipulation */

#include "libguile/gh.h"

/* returns the length of a list */
unsigned long 
gh_length (SCM l)
{
  return gh_scm2ulong (scm_length (l));
}

/* list operations */

/* gh_list(SCM elt, ...) is implemented as a macro in gh.h. */

/* gh_append() takes a args, which is a list of lists, and appends
   them all together into a single list, which is returned.  This is
   equivalent to the Scheme procedure (append list1 list2 ...) */
SCM gh_append(SCM args)
{
  return scm_append(args);
}

SCM gh_append2(SCM l1, SCM l2)
{
  return scm_append(scm_listify(l1, l2, SCM_UNDEFINED));
}

SCM gh_append3(SCM l1, SCM l2, SCM l3)
{
  return scm_append(scm_listify(l1, l2, l3, SCM_UNDEFINED));
}

SCM gh_append4(SCM l1, SCM l2, SCM l3, SCM l4)
{
  return scm_append(scm_listify(l1, l2, l3, l4, SCM_UNDEFINED));
}

/* gh_reverse() is defined as a macro in gh.h */
/* gh_list_tail() is defined as a macro in gh.h */
/* gh_list_ref() is defined as a macro in gh.h */
/* gh_memq() is defined as a macro in gh.h */
/* gh_memv() is defined as a macro in gh.h */
/* gh_member() is defined as a macro in gh.h */
/* gh_assq() is defined as a macro in gh.h */
/* gh_assv() is defined as a macro in gh.h */
/* gh_assoc() is defined as a macro in gh.h */

/* analogous to the Scheme cons operator */
SCM 
gh_cons (SCM x, SCM y)
{
  return scm_cons (x, y);
}

/* analogous to the Scheme car operator */
SCM 
gh_car (SCM x)
{
  return SCM_CAR (x);
}

/* analogous to the Scheme cdr operator */
SCM 
gh_cdr (SCM x)
{
  return SCM_CDR (x);
}

/* now for the multiple car/cdr utility procedures */
SCM 
gh_caar (SCM x)
{
  return SCM_CAAR (x);
}
SCM 
gh_cadr (SCM x)
{
  return SCM_CADR (x);
}
SCM 
gh_cdar (SCM x)
{
  return SCM_CDAR (x);
}
SCM 
gh_cddr (SCM x)
{
  return SCM_CDDR (x);
}

SCM 
gh_caaar (SCM x)
{
  return SCM_CAAAR (x);
}
SCM 
gh_caadr (SCM x)
{
  return SCM_CAADR (x);
}
SCM 
gh_cadar (SCM x)
{
  return SCM_CADAR (x);
}
SCM 
gh_caddr (SCM x)
{
  return SCM_CADDR (x);
}
SCM 
gh_cdaar (SCM x)
{
  return SCM_CDAAR (x);
}
SCM 
gh_cdadr (SCM x)
{
  return SCM_CDADR (x);
}
SCM 
gh_cddar (SCM x)
{
  return SCM_CDDAR (x);
}
SCM 
gh_cdddr (SCM x)
{
  return SCM_CDDDR (x);
}

/* equivalent to (set-car! pair value) */
SCM
gh_set_car_x(SCM pair, SCM value)
{
  return scm_set_car_x(pair, value);
}

/* equivalent to (set-cdr! pair value) */
SCM
gh_set_cdr_x(SCM pair, SCM value)
{
  return scm_set_cdr_x(pair, value);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
