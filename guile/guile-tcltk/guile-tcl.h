/* classes: h_files */

#ifndef GUILE_TCLH
#define GUILE_TCLH
/*	Copyright (C) 1998, 2001, 2002 Free Software Foundation, Inc.
 * 
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


#include <tcl.h>

/* We represent an interpreter using SCM's "smob" representation.  The
   interpreter value points to a pair whose car is
   scm_tc16_tcl_interp, and whose cdr is a pointer to a struct
   gtcltk_interp, defined below.

   The following paragraph isn't true yet:

   We use Tcl_GetAssocData and Tcl_SetAssocData to map interpreter
   pointers onto their schemey representations, for use when we only
   have a Tcl interpreter pointer, and need to find the correponding
   scheme object.  */

struct gtcltk_interp {
  Tcl_Interp *interp;		/* the Tcl interpreter object */
  SCM props;			/* property list */
};
  
#define SCM_TERPP(OBJ) (SCM_CAR (OBJ) == scm_tc16_tcl_interp)
#define SCM_GTCLTK(OBJ) ((struct gtcltk_interp *) SCM_CDR (OBJ))
#define SCM_TERP(OBJ) (SCM_GTCLTK(OBJ)->interp)
#define SCM_PROPS(OBJ) (SCM_GTCLTK(OBJ)->props)

#define SCM_ENTER_TCL
#define SCM_LEAVE_TCL

extern int scm_tc16_tcl_interp;

#ifdef USE_THREADS
extern scm_t_mutex scm_tcl_mutex;
extern scm_t_cond scm_tcl_condvar;
extern int scm_tcl_handle_event_p;
#endif


#endif  /* GUILE_TCLH */
