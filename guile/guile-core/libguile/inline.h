/* classes: h_files */

#ifndef SCM_INLINE_H
#define SCM_INLINE_H

/* Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
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

/* This file is for inline functions.  On platforms that don't support
   inlining functions, they are turned into ordinary functions.  See
   "inline.c".
*/

#include "libguile/__scm.h"

#if (SCM_DEBUG_CELL_ACCESSES == 1)
#include <stdio.h>
#endif

#include "libguile/pairs.h"
#include "libguile/gc.h"
#include "libguile/threads.h"


SCM_API SCM scm_cell (scm_t_bits car, scm_t_bits cdr);
SCM_API SCM scm_double_cell (scm_t_bits car, scm_t_bits cbr,
			     scm_t_bits ccr, scm_t_bits cdr);



#if defined SCM_C_INLINE || defined SCM_INLINE_C_INCLUDING_INLINE_H
/* either inlining, or being included from inline.c.  We use (and
   repeat) this long #if test here and below so that we don't have to
   introduce any extraneous symbols into the public namespace.  We
   only need SCM_C_INLINE to be seen publically . */

extern unsigned scm_newcell2_count;
extern unsigned scm_newcell_count;

#if defined SCM_C_INLINE && ! defined SCM_INLINE_C_INCLUDING_INLINE_H
/* definitely inlining */
#ifdef __GNUC__
extern
#else
static
#endif
SCM_C_INLINE
#endif
SCM
scm_cell (scm_t_bits car, scm_t_bits cdr)
{
  SCM z;
  /* We retrieve the SCM pointer only once since the call to
     SCM_FREELIST_LOC will be slightly expensive when we support
     preemptive multithreading.  SCM_FREELIST_LOC will then retrieve
     the thread specific freelist.
   
     Until then, SCM_FREELIST_DOC expands to (&scm_i_freelist) and the
     following code will compile to the same as if we had worked
     directly on the scm_i_freelist variable.
   */
  SCM *freelist = SCM_FREELIST_LOC (scm_i_freelist);

  if (SCM_NULLP (*freelist))
    z = scm_gc_for_newcell (&scm_i_master_freelist, freelist);
  else
    {
      z = *freelist;
      *freelist = SCM_FREE_CELL_CDR (*freelist);
    }

  /*
    We update scm_cells_allocated from this function. If we don't
    update this explicitly, we will have to walk a freelist somewhere
    later on, which seems a lot more expensive.
   */
  scm_cells_allocated += 1;  

#if (SCM_DEBUG_CELL_ACCESSES == 1)
    if (scm_debug_cell_accesses_p)
      {
	if (SCM_GC_MARK_P (z))
	  {
	    fprintf(stderr, "scm_cell tried to allocate a marked cell.\n");
	    abort();
	  }
	else if (SCM_GC_CELL_WORD(z, 0) != scm_tc_free_cell)
	  {
	    fprintf(stderr, "cell from freelist is not a free cell.\n");
	    abort();
	  }
      }

    /*
      Always set mark. Otherwise cells that are alloced before
      scm_debug_cell_accesses_p is toggled seem invalid.
    */
    SCM_SET_GC_MARK (z);

    /*
      TODO: figure out if this use of mark bits is valid with
      threading. What if another thread is doing GC at this point
      ... ?
     */
      
#endif

  
  /* Initialize the type slot last so that the cell is ignored by the
     GC until it is completely initialized.  This is only relevant
     when the GC can actually run during this code, which it can't for
     cooperating threads, but it might be important when we get true
     preemptive threads.
  */
  SCM_GC_SET_CELL_WORD (z, 1, cdr);
  SCM_GC_SET_CELL_WORD (z, 0, car);

#if 0 /*fixme* Hmm... let's consider this later. */
#if !defined(SCM_USE_COOP_THREADS) && !defined(SCM_USE_NULL_THREADS) && !defined(SCM_USE_COPT_THREADS)
  /* When we are using preemtive threads, we might need to make
     sure that the initial values for the slots are protected until
     the cell is completely initialized.
  */
#error review me
  scm_remember_upto_here_1 (SCM_PACK (cdr));
#endif
#endif

#if (SCM_DEBUG_CELL_ACCESSES == 1)
  if (scm_expensive_debug_cell_accesses_p )
    scm_i_expensive_validation_check (z);
#endif
  
  return z;
}

#if defined SCM_C_INLINE && ! defined SCM_INLINE_C_INCLUDING_INLINE_H
/* definitely inlining */
#ifdef __GNUC__
extern
#else
static
#endif
SCM_C_INLINE
#endif
SCM
scm_double_cell (scm_t_bits car, scm_t_bits cbr,
		 scm_t_bits ccr, scm_t_bits cdr)
{
  SCM z;
  SCM *freelist = SCM_FREELIST_LOC (scm_i_freelist2);

  if (SCM_NULLP (*freelist))
    z = scm_gc_for_newcell (&scm_i_master_freelist2, freelist);
  else
    {
      z = *freelist;
      *freelist = SCM_FREE_CELL_CDR (*freelist);
    }

  scm_cells_allocated += 2;

  /* Initialize the type slot last so that the cell is ignored by the
     GC until it is completely initialized.  This is only relevant
     when the GC can actually run during this code, which it can't for
     cooperating threads, but it might be important when we get true
     preemptive threads.
  */
  SCM_GC_SET_CELL_WORD (z, 1, cbr);
  SCM_GC_SET_CELL_WORD (z, 2, ccr);
  SCM_GC_SET_CELL_WORD (z, 3, cdr);
  SCM_GC_SET_CELL_WORD (z, 0, car);

#if 0 /*fixme* Hmm... let's consider this later. */
#if !defined(SCM_USE_COOP_THREADS) && !defined(SCM_USE_NULL_THREADS) && !defined(SCM_USE_COPT_THREADS)
  /* When we are using non-cooperating threads, we might need to make
     sure that the initial values for the slots are protected until
     the cell is completely initialized.
  */
#error review me
  scm_remember_upto_here_3 (SCM_PACK (cbr), SCM_PACK (ccr), SCM_PACK (cdr));
#endif
#endif


#if (SCM_DEBUG_CELL_ACCESSES == 1)
  if (scm_debug_cell_accesses_p)
    {
      if (SCM_GC_MARK_P (z))
	{
	  fprintf(stderr,
		  "scm_double_cell tried to allocate a marked cell.\n");
	  abort();
	}
    }

  /* see above. */
  SCM_SET_GC_MARK (z);

#endif

  /* When this function is inlined, it's possible that the last
     SCM_GC_SET_CELL_WORD above will be adjacent to a following
     initialization of z.  E.g., it occurred in scm_make_real.  GCC
     from around version 3 (e.g., certainly 3.2) began taking
     advantage of strict C aliasing rules which say that it's OK to
     interchange the initialization above and the one below when the
     pointer types appear to differ sufficiently.  We don't want that,
     of course.  GCC allows this behaviour to be disabled with the
     -fno-strict-aliasing option, but would also need to be supplied
     by Guile users.  Instead, the following statements prevent the
     reordering.
   */
#ifdef __GNUC__
  asm volatile ("" : : : "memory");
#else
  /* portable version, just in case any other compiler does the same
     thing.  */
  scm_remember_upto_here_1 (z);
#endif

  return z;
}



#endif
#endif