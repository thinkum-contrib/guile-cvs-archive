/*      Copyright (C) 1995,1996 Free Software Foundation, Inc.

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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


/* type predicates and equality predicates */

#include <stdio.h>

#include <gh.h>

/* type predicates: tell you if an SCM object has a given type */
int 
gh_boolean_p (SCM val)
{
  return (scm_boolean_p (val) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_symbol_p (SCM val)
{
  return (scm_symbol_p (val) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_char_p (SCM val)
{
  return (scm_char_p (val) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_vector_p (SCM val)
{
  return (scm_vector_p (val) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_pair_p (SCM val)
{
  return (scm_pair_p (val) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_number_p (SCM val)
{
  return (scm_number_p (val) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_string_p (SCM val)
{
  return (scm_string_p (val) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_procedure_p (SCM val)
{
  return (scm_procedure_p (val) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_list_p (SCM val)
{
  return (scm_list_p (val) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_inexact_p (SCM val)
{
  return (scm_inexact_p (val) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_exact_p (SCM val)
{
  return (scm_exact_p (val) == SCM_BOOL_T) ? 1 : 0;
}

/* the three types of equality */
int 
gh_eq_p (SCM x, SCM y)
{
  return (scm_eq_p (x, y) == SCM_BOOL_T) ? 1 : 0;
}
int 
gh_eqv_p (SCM x, SCM y)
{
  return (scm_eqv_p (x, y) != SCM_BOOL_T) ? 1 : 0;
}
int 
gh_equal_p (SCM x, SCM y)
{
  return (scm_equal_p (x, y) != SCM_BOOL_T) ? 1 : 0;
}
