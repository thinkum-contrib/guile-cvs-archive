/* classes: h_files */

#ifndef SCM_STROP_H
#define SCM_STROP_H

/* Copyright (C) 1995,1996,1997,2000,2001 Free Software Foundation, Inc.
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



SCM_API SCM scm_string_index (SCM str, SCM chr, SCM frm, SCM to);
SCM_API SCM scm_string_rindex (SCM str, SCM chr, SCM frm, SCM to);
SCM_API SCM scm_substring_move_x (SCM str1, SCM start1, SCM end1, 
				  SCM str2, SCM start2);
SCM_API SCM scm_substring_fill_x (SCM str, SCM start, SCM end, SCM fill);
SCM_API SCM scm_string_null_p (SCM str);
SCM_API SCM scm_string_to_list (SCM str);
SCM_API SCM scm_string_copy (SCM str);
SCM_API SCM scm_string_fill_x (SCM str, SCM chr);
SCM_API void scm_init_strop (void);
SCM_API SCM scm_string_upcase_x (SCM v);
SCM_API SCM scm_string_upcase (SCM v);
SCM_API SCM scm_string_downcase_x (SCM v);
SCM_API SCM scm_string_downcase (SCM v);
SCM_API SCM scm_string_capitalize_x (SCM v);
SCM_API SCM scm_string_capitalize (SCM v);
SCM_API SCM scm_string_split (SCM str, SCM chr);
SCM_API SCM scm_string_ci_to_symbol (SCM v);

#endif  /* SCM_STROP_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
