/* classes: h_files */

#ifndef SCM_BOOLEAN_H
#define SCM_BOOLEAN_H

/* Copyright (C) 1995,1996,2000 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"



/* Boolean Values 
 *
 */ 
#define SCM_FALSEP(x)		(SCM_EQ_P ((x), SCM_BOOL_F))
#define SCM_NFALSEP(x)		(!SCM_FALSEP (x))

#define SCM_BOOLP(x)		(SCM_EQ_P ((x), SCM_BOOL_F) || SCM_EQ_P ((x), SCM_BOOL_T))

/* Convert from a C boolean to a SCM boolean value */
#define SCM_BOOL(f)		((f) ? SCM_BOOL_T : SCM_BOOL_F)

/* Convert from a C boolean to a SCM boolean value and negate it */
#define SCM_NEGATE_BOOL(f)	((f) ? SCM_BOOL_F : SCM_BOOL_T)

/* SCM_BOOL_NOT returns the other boolean.  
 * The order of ^s here is important for Borland C++ (!?!?!)
 */
#define SCM_BOOL_NOT(x)		(SCM_PACK (SCM_UNPACK (x) \
					   ^ (SCM_UNPACK (SCM_BOOL_T) \
					      ^ SCM_UNPACK (SCM_BOOL_F))))

#define scm_is_false(x) scm_is_eq ((x), SCM_BOOL_F)
#define scm_is_true(x)  !scm_is_false (x)

SCM_API int scm_is_bool(x);
#define scm_from_bool(x) ((f) ? SCM_BOOL_T : SCM_BOOL_F)
SCM_API int scm_to_bool (SCM x);



SCM_API SCM scm_not (SCM x);
SCM_API SCM scm_boolean_p (SCM obj);

SCM_API void scm_init_boolean (void);

#endif  /* SCM_BOOLEAN_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
