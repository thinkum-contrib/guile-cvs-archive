/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002 Free Software Foundation, Inc.
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


#include <stdio.h>
#include <gmp.h>

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/stime.h"
#include "libguile/stackchk.h"
#include "libguile/struct.h"
#include "libguile/smob.h"
#include "libguile/unif.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/weaks.h"
#include "libguile/hashtab.h"
#include "libguile/tags.h"
#include "libguile/private-gc.h"
#include "libguile/validate.h"
#include "libguile/deprecation.h"
#include "libguile/gc.h"


#include "libguile/private-gc.h"

long int scm_i_deprecated_memory_return;


/* During collection, this accumulates structures which are to be freed.
 */
SCM scm_i_structs_to_free;


/*
  Init all the free cells in CARD, prepending to *FREE_LIST.

  Return: number of free cells found in this card.

  It would be cleaner to have a separate function sweep_value(), but
  that is too slow (functions with switch statements can't be
  inlined).



  
  NOTE:

  This function is quite efficient. However, for many types of cells,
  allocation and a de-allocation involves calling malloc() and
  free().

  This is costly for small objects (due to malloc/free overhead.)
  (should measure this).

  It might also be bad for threads: if several threads are allocating
  strings concurrently, then mallocs for both threads may have to
  fiddle with locks.

  It might be interesting to add a separate memory pool for small
  objects to each freelist.

  --hwn.
 */
int
scm_i_sweep_card (scm_t_cell *  p, SCM *free_list, scm_t_heap_segment*seg)
#define FUNC_NAME "sweep_card"
{
  scm_t_c_bvec_long *bitvec = SCM_GC_CARD_BVEC(p);
  scm_t_cell * end = p + SCM_GC_CARD_N_CELLS;
  int span = seg->span;
  int offset =SCM_MAX (SCM_GC_CARD_N_HEADER_CELLS, span);
  int free_count  = 0;

  ++ scm_gc_running_p;

  /*
    I tried something fancy with shifting by one bit every word from
    the bitvec in turn, but it wasn't any faster, but quite a bit
    hairier.
   */
  for (p += offset; p < end; p += span, offset += span)
    {
      SCM scmptr = PTR2SCM (p);
      if (SCM_C_BVEC_GET (bitvec, offset))
        continue;

      switch (SCM_TYP7 (scmptr))
	{
	case scm_tcs_struct:
	  /* The card can be swept more than once.  Check that it's
	   * the first time!
	   */
	  if (!SCM_STRUCT_GC_CHAIN (scmptr))
	    {
	      /* Structs need to be freed in a special order.
	       * This is handled by GC C hooks in struct.c.
	       */
	      SCM_SET_STRUCT_GC_CHAIN (scmptr, scm_i_structs_to_free);
	      scm_i_structs_to_free = scmptr;
	    }
	  continue;
      
	case scm_tcs_cons_imcar:
	case scm_tcs_cons_nimcar:
	case scm_tcs_closures:
	case scm_tc7_pws:
	  break;
	case scm_tc7_wvect:
	case scm_tc7_vector:
	  {
	    unsigned long int length = SCM_VECTOR_LENGTH (scmptr);
	    if (length > 0)
	      {
		scm_gc_free (SCM_VECTOR_BASE (scmptr),
			     length * sizeof (scm_t_bits),
			     "vector");
	      }
	    break;
	  }
#ifdef CCLO
	case scm_tc7_cclo:
	  scm_gc_free (SCM_CCLO_BASE (scmptr), 
		       SCM_CCLO_LENGTH (scmptr) * sizeof (SCM),
		       "compiled closure");
	  break;
#endif
#if SCM_HAVE_ARRAYS
	case scm_tc7_bvect:
	  {
	    unsigned long int length = SCM_BITVECTOR_LENGTH (scmptr);
	    if (length > 0)
	      {
		scm_gc_free (SCM_BITVECTOR_BASE (scmptr),
			     (sizeof (long)
			      * ((length+SCM_LONG_BIT-1) / SCM_LONG_BIT)),
			     "vector");
	      }
	  }
	  break;
	case scm_tc7_byvect:
	case scm_tc7_ivect:
	case scm_tc7_uvect:
	case scm_tc7_svect:
#if SCM_SIZEOF_LONG_LONG != 0
	case scm_tc7_llvect:
#endif
	case scm_tc7_fvect:
	case scm_tc7_dvect:
	case scm_tc7_cvect:
	  scm_gc_free (SCM_UVECTOR_BASE (scmptr), 
		       (SCM_UVECTOR_LENGTH (scmptr)
			* scm_uniform_element_size (scmptr)),
		       "vector");
	  break;
#endif
	case scm_tc7_string:
	  scm_gc_free (SCM_STRING_CHARS (scmptr), 
		       SCM_STRING_LENGTH (scmptr) + 1, "string");
	  break;
	case scm_tc7_symbol:
	  scm_gc_free (SCM_SYMBOL_CHARS (scmptr), 
		       SCM_SYMBOL_LENGTH (scmptr) + 1, "symbol");
	  break;
	case scm_tc7_variable:
	  break;
	case scm_tcs_subrs:
	  /* the various "subrs" (primitives) are never freed */
	  continue;
	case scm_tc7_port:
	  if SCM_OPENP (scmptr)
	    {
	      int k = SCM_PTOBNUM (scmptr);
	      size_t mm;
#if (SCM_DEBUG_CELL_ACCESSES == 1)
	      if (!(k < scm_numptob))
		{
		  fprintf (stderr, "undefined port type");
		  abort();
		}
#endif
	      /* Keep "revealed" ports alive.  */
	      if (scm_revealed_count (scmptr) > 0)
		continue;
	  
	      /* Yes, I really do mean scm_ptobs[k].free */
	      /* rather than ftobs[k].close.  .close */
	      /* is for explicit CLOSE-PORT by user */
	      mm = scm_ptobs[k].free (scmptr);

	      if (mm != 0)
		{
#if SCM_ENABLE_DEPRECATED == 1
		  scm_c_issue_deprecation_warning
		    ("Returning non-0 from a port free function is "
		     "deprecated.  Use scm_gc_free et al instead.");
		  scm_c_issue_deprecation_warning_fmt
		    ("(You just returned non-0 while freeing a %s.)",
		     SCM_PTOBNAME (k));
		  scm_i_deprecated_memory_return += mm;
#else
		  abort ();
#endif
		}

	      SCM_SETSTREAM (scmptr, 0);
	      scm_remove_from_port_table (scmptr);
	      scm_gc_ports_collected++;
	      SCM_CLR_PORT_OPEN_FLAG (scmptr);
	    }
	  break;
	case scm_tc7_smob:
	  switch SCM_TYP16 (scmptr)
	    {
	    case scm_tc_free_cell:
	    case scm_tc16_real:
	      break;
            case scm_tc16_big:
              mpz_clear (SCM_I_BIG_MPZ (scmptr));
              /* nothing else to do here since the mpz is in a double cell */
              break;
	    case scm_tc16_complex:
	      scm_gc_free (SCM_COMPLEX_MEM (scmptr), 2*sizeof (double),
			   "complex");
	      break;
	    default:
	      {
		int k;
		k = SCM_SMOBNUM (scmptr);
#if (SCM_DEBUG_CELL_ACCESSES == 1)
		if (!(k < scm_numsmob))
		  {
		    fprintf (stderr, "undefined smob type");
		    abort();
		  }
#endif
		if (scm_smobs[k].free)
		  {
		    size_t mm;
		    mm = scm_smobs[k].free (scmptr);
		    if (mm != 0)
		      {
#if SCM_ENABLE_DEPRECATED == 1
			scm_c_issue_deprecation_warning
			  ("Returning non-0 from a smob free function is "
			   "deprecated.  Use scm_gc_free et al instead.");
			scm_c_issue_deprecation_warning_fmt
			  ("(You just returned non-0 while freeing a %s.)",
			   SCM_SMOBNAME (k));
			scm_i_deprecated_memory_return += mm;
#else
			abort();
#endif
		      }
		  }
		break;
	      }
	    }
	  break;
	default:
	  fprintf (stderr, "unknown type");
	  abort();
	}

	  
      SCM_SET_CELL_TYPE (scmptr, scm_tc_free_cell);
      SCM_SET_FREE_CELL_CDR (scmptr, PTR2SCM (*free_list));
      *free_list = scmptr;
      free_count ++;
    }

  --scm_gc_running_p;
  return free_count;
}
#undef FUNC_NAME


/*
  Like sweep, but no complicated logic to do the sweeping.
 */
int
scm_i_init_card_freelist (scm_t_cell *  card, SCM *free_list,
			scm_t_heap_segment*seg)
{
  int span = seg->span;
  scm_t_cell *end = card + SCM_GC_CARD_N_CELLS;
  scm_t_cell *p = end - span;

  scm_t_c_bvec_long * bvec_ptr =  (scm_t_c_bvec_long* ) seg->bounds[1];
  int idx = (card  - seg->bounds[0]) / SCM_GC_CARD_N_CELLS; 

  bvec_ptr += idx *SCM_GC_CARD_BVEC_SIZE_IN_LONGS;
  SCM_GC_CELL_BVEC (card) = bvec_ptr;
  
  /*
     ASSUMPTION: n_header_cells <= 2. 
   */
  for (; p > card;  p -= span)
    {
      const SCM scmptr = PTR2SCM (p);
      SCM_SET_CELL_TYPE (scmptr, scm_tc_free_cell);
      SCM_SET_FREE_CELL_CDR (scmptr, PTR2SCM (*free_list));
      *free_list = scmptr;
    }

  return SCM_GC_CARD_N_CELLS - SCM_MAX(span, SCM_GC_CARD_N_HEADER_CELLS);
}


#if (SCM_DEBUG_DEBUGGING_SUPPORT == 1)

typedef struct scm_dbg_t_list_cell {
  scm_t_bits car;  
  struct scm_dbg_t_list_cell * cdr;
} scm_dbg_t_list_cell;


typedef struct scm_dbg_t_double_cell {
  scm_t_bits word_0;
  scm_t_bits word_1;
  scm_t_bits word_2;
  scm_t_bits word_3;
} scm_dbg_t_double_cell;


int scm_dbg_gc_marked_p (SCM obj);
scm_t_cell * scm_dbg_gc_get_card (SCM obj);
long * scm_dbg_gc_get_bvec (SCM obj);


int
scm_dbg_gc_marked_p (SCM obj)
{
  if (!SCM_IMP (obj))
    return SCM_GC_MARK_P(obj);
  else
    return 0;
}

scm_t_cell *
scm_dbg_gc_get_card (SCM obj)
{
  if (!SCM_IMP (obj))
    return SCM_GC_CELL_CARD(obj);
  else
    return NULL;
}

long *
scm_dbg_gc_get_bvec (SCM obj)
{
  if (!SCM_IMP (obj))
    return SCM_GC_CARD_BVEC (SCM_GC_CELL_CARD (obj));
  else
    return NULL;
}

#endif
