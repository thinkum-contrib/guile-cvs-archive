/* classes: h_files */

#ifndef CONTINUATIONSH
#define CONTINUATIONSH
/*	Copyright (C) 1995,1996, 2000 Free Software Foundation, Inc.
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

#include "libguile/__scm.h"



/* a continuation SCM is a non-immediate pointing to a heap cell with:
   word 0: bits 0-15: unused.
           bits 16-31: smob type tag: scm_tc16_continuation.
   word 1: malloc block containing an scm_contregs_t structure with a
           tail array of SCM_STACKITEM.  the size of the array is stored
	   in the num_stack_items field of the structure.
*/

extern scm_bits_t scm_tc16_continuation;

typedef struct 
{
  SCM throw_value;
  jmp_buf jmpbuf;
  SCM dynenv;
  SCM_STACKITEM *base;      /* base of the live stack, before it was saved.  */
  scm_bits_t num_stack_items; /* size of the saved stack.  */
  scm_ubits_t seq;         /* dynamic root identifier.  */

#ifdef DEBUG_EXTENSIONS
  /* the most recently created debug frame on the live stack, before
     it was saved.  */
  struct scm_debug_frame_t *dframe;
#endif
  SCM_STACKITEM stack[1];    /* copied stack of size num_stack_items.  */ 
} scm_contregs_t;

#if (SCM_DEBUG_DEPRECATED == 0)
# define scm_contregs scm_contregs_t
#endif

#define SCM_CONTINUATIONP(x)	SCM_TYP16_PREDICATE (scm_tc16_continuation, x)

#define SCM_CONTREGS(x)		((scm_contregs_t *) SCM_CELL_WORD_1 (x))

#define SCM_CONTINUATION_LENGTH(x) (SCM_CONTREGS (x)->num_stack_items)
#define SCM_SET_CONTINUATION_LENGTH(x,n)\
   (SCM_CONTREGS (x)->num_stack_items = (n))
#define SCM_JMPBUF(x)		((SCM_CONTREGS (x))->jmpbuf)
#define SCM_DYNENV(x)		((SCM_CONTREGS (x))->dynenv)
#define SCM_THROW_VALUE(x)	((SCM_CONTREGS (x))->throw_value)
#define SCM_BASE(x)		((SCM_CONTREGS (x))->base) 
#define SCM_SEQ(x)		((SCM_CONTREGS (x))->seq)   
#define SCM_DFRAME(x)		((SCM_CONTREGS (x))->dframe)



extern SCM scm_make_continuation (int *first);
extern void scm_init_continuations (void);

#endif  /* CONTINUATIONSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
