/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002 Free Software Foundation, Inc.
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

#include <assert.h> 
#include <stdio.h>
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/pairs.h"
#include "libguile/gc.h"
#include "libguile/private-gc.h"



#define SCM_GC_CARD_BVEC_SIZE_IN_LONGS \
    ((SCM_GC_CARD_N_CELLS + SCM_C_BVEC_LONG_BITS - 1) / SCM_C_BVEC_LONG_BITS)
#define SCM_GC_IN_CARD_HEADERP(x) \
  (scm_t_cell *) (x) <  SCM_GC_CELL_CARD (x) + SCM_GC_CARD_N_HEADER_CELLS


size_t scm_max_segment_size;

scm_t_heap_segment *
scm_i_make_empty_heap_segment (scm_t_cell_type_statistics *fl)
{
  scm_t_heap_segment * shs = malloc (sizeof (scm_t_heap_segment));

  if (!shs)
    {
      fprintf (stderr, "scm_i_get_new_heap_segment: out of memory.\n");
      abort ();
    }
  
  shs->bounds[0] = NULL;
  shs->bounds[1] = NULL;
  shs->malloced = NULL;
  shs->span = fl->span;
  shs->freelist  = fl;
  shs->next_free_card = NULL;
  
  return shs;
}


/*
  Fill SEGMENT with memory both for data and mark bits.

  RETURN:  1 on success, 0 failure  
 */
int 
scm_i_initialize_heap_segment_data (scm_t_heap_segment * segment, size_t requested)
{
  /*
    round upwards
   */
  int card_data_cell_count = (SCM_GC_CARD_N_CELLS - SCM_GC_CARD_N_HEADER_CELLS);
  int card_count =1 + (requested / sizeof (scm_t_cell)) /  card_data_cell_count; 

  /*
    one card extra due to alignment
  */
  size_t mem_needed = (1+card_count) * SCM_GC_SIZEOF_CARD
    + SCM_GC_CARD_BVEC_SIZE_IN_LONGS * card_count * SCM_SIZEOF_LONG
    ;
  scm_t_c_bvec_long * bvec_ptr = 0;
  scm_t_cell *  memory = 0;

  /*
    We use malloc to alloc the heap. On GNU libc this is 
    equivalent to mmapping /dev/zero
   */
  SCM_SYSCALL (memory = (scm_t_cell * ) calloc (1, mem_needed));

  if (memory == NULL)
    return 0;

  segment->malloced = memory;
  segment->bounds[0] = SCM_GC_CARD_UP (memory);
  segment->bounds[1] = segment->bounds[0] + card_count * SCM_GC_CARD_N_CELLS;

  segment->freelist->heap_size += scm_i_segment_cell_count (segment);
  
  bvec_ptr = (scm_t_c_bvec_long*) segment->bounds[1];


  {
    scm_t_cell *  ptr  = segment->bounds [0];

    for (;
	 ptr < segment->bounds[1]; ptr += SCM_GC_CARD_N_CELLS)
      {
	SCM_GC_CELL_BVEC (ptr) = bvec_ptr;
	if (segment->span == 2) 
	  SCM_GC_SET_CARD_DOUBLECELL (ptr);
 
	bvec_ptr += SCM_GC_CARD_BVEC_SIZE_IN_LONGS;

	/*
	  Don't init the mem. This is handled by lazy sweeping.
	 */
      }
  }

  segment->next_free_card = segment->bounds[0];
  segment->first_time = 1;
  return 1;
}

int
scm_i_segment_card_count (scm_t_heap_segment * seg)
{
  return (seg->bounds[1] - seg->bounds[0]) / SCM_GC_CARD_N_CELLS;
}

/*
  Return the number of available single-cell data cells. 
 */
int
scm_i_segment_cell_count (scm_t_heap_segment * seg)
{
  return scm_i_segment_card_count (seg) * (SCM_GC_CARD_N_CELLS - SCM_GC_CARD_N_HEADER_CELLS)
    + ((seg->span == 2) ? -1 : 0);
}

void
scm_i_clear_segment_mark_space (scm_t_heap_segment *seg)
{
  scm_t_cell *  markspace = seg->bounds[1];

  memset (markspace, 0x00,
	  scm_i_segment_card_count (seg) *  SCM_GC_CARD_BVEC_SIZE_IN_LONGS * SCM_SIZEOF_LONG);
}

/*
  RETURN:

  Freelist. 
*/
SCM
scm_i_sweep_some_cards (scm_t_heap_segment *seg)
{
  SCM cells = SCM_EOL;
  int threshold = 512;
  int collected = 0;
  int (*sweeper) (scm_t_cell *, SCM *, int )
    = (seg->first_time) ? &scm_init_card_freelist : &scm_i_sweep_card;

  scm_t_cell * next_free = seg->next_free_card;
  int cards_swept = 0;
  
  while (collected < threshold && next_free < seg->bounds[1])
    {
      collected += (*sweeper) (next_free, &cells, seg->span);
      next_free += SCM_GC_CARD_N_CELLS;
      cards_swept ++;
    }

  scm_gc_cells_swept +=  cards_swept * (SCM_GC_CARD_N_CELLS - SCM_GC_CARD_N_HEADER_CELLS);
  scm_gc_cells_collected += collected * seg->span;

  if (!seg->first_time)
    scm_cells_allocated -= collected * seg->span;
  
  seg->freelist->collected += collected  * seg->span;
  

  if(next_free == seg->bounds[1])
    {
      seg->first_time = 0;
    }

  seg->next_free_card = next_free;
  return cells;
}


/*
  Force a sweep of this entire segment. This doesn't modify sweep
  statistics, it just frees the memory pointed to by to-be-swept
  cells.

  Implementation is slightly ugh.

  FIXME: if you do scm_i_sweep_segment(), and then allocate from this
  segment again, the statistics are off.
 */
void
scm_i_sweep_segment (scm_t_heap_segment * seg)
{
  scm_t_cell * p = seg->next_free_card;
  int yield = scm_gc_cells_collected;
  int coll = seg->freelist->collected;
  int alloc = scm_cells_allocated ;
  
  while (scm_i_sweep_some_cards (seg) != SCM_EOL)
    ;

  scm_gc_cells_collected = yield;
  scm_cells_allocated = alloc;
  seg->freelist->collected = coll; 
  
  seg->next_free_card =p;
}

void
scm_i_sweep_all_segments (char const  *reason)
{
  int i= 0; 

  for (i = 0; i < scm_i_heap_segment_table_size; i++)
    {
      scm_i_sweep_segment (scm_i_heap_segment_table[i]);
    }
}


/*
  Heap segment table.

  The table is sorted by the address of the data itself. This makes
  for easy lookups. This is not portable: according to ANSI C,
  pointers can only be compared within the same object (i.e. the same
  block of malloced memory.). For machines with weird architectures,
  this should be revised.
  
  (Apparently, for this reason 1.6 and earlier had macros for pointer
  comparison. )
  
  perhaps it is worthwhile to remove the 2nd level of indirection in
  the table, but this certainly makes for cleaner code.
*/
scm_t_heap_segment ** scm_i_heap_segment_table;
size_t scm_i_heap_segment_table_size;
scm_t_cell *lowest_cell;
scm_t_cell *highest_cell; 


void
scm_i_clear_mark_space (void)
{
  int i = 0;
  for (; i < scm_i_heap_segment_table_size; i++)
    {
      scm_i_clear_segment_mark_space (scm_i_heap_segment_table[i]);
    }
}


/*
  RETURN: index of inserted segment.
 */
int
scm_i_insert_segment (scm_t_heap_segment * seg)
{
  size_t size = (scm_i_heap_segment_table_size + 1) * sizeof (scm_t_heap_segment *);
  SCM_SYSCALL(scm_i_heap_segment_table = ((scm_t_heap_segment **)
			       realloc ((char *)scm_i_heap_segment_table, size)));

  /*
    We can't alloc 4 more bytes. This is hopeless.
   */
  if (!scm_i_heap_segment_table)
    {
      fprintf (stderr, "scm_i_get_new_heap_segment: Could not grow heap segment table.\n");
      abort ();
    }

  if (!lowest_cell)
    {
      lowest_cell = seg->bounds[0];
      highest_cell = seg->bounds[1];
    }
  else
    {
      lowest_cell = SCM_MIN (lowest_cell, seg->bounds[0]);
      highest_cell = SCM_MAX (highest_cell, seg->bounds[1]);
    }

  
  {
    int i = 0;
    int j = 0;

    while (i < scm_i_heap_segment_table_size
	   && scm_i_heap_segment_table[i]->bounds[0] <= seg->bounds[0])
      i++;
    for (j = scm_i_heap_segment_table_size; j > i; --j)
      scm_i_heap_segment_table[j] = scm_i_heap_segment_table[j - 1];

    scm_i_heap_segment_table [i] = seg;
    scm_i_heap_segment_table_size ++;

    return i;
  }
}

SCM
scm_i_sweep_some_segments (scm_t_cell_type_statistics * fl)
{
  int i = fl->heap_segment_idx;
  SCM collected =SCM_EOL;
  
  if (i == -1)
    i++;
  
  for (;
       i < scm_i_heap_segment_table_size; i++)
    {
      if (scm_i_heap_segment_table[i]->freelist != fl)
	continue;
      
      collected = scm_i_sweep_some_cards (scm_i_heap_segment_table[i]);


      if (collected != SCM_EOL)       /* Don't increment i */
	break;
    }

  fl->heap_segment_idx = i;
  
  return collected;
}




void
scm_i_reset_segments (void)
{
  int i = 0;
  for (; i < scm_i_heap_segment_table_size; i++)
    {
      scm_t_heap_segment * seg = scm_i_heap_segment_table[i];
      seg->next_free_card = seg->bounds[0];
    }
}


/*
  Determine whether the given value does actually represent a cell in
  some heap segment.  If this is the case, the number of the heap
  segment is returned.  Otherwise, -1 is returned.  Binary search is
  used to determine the heap segment that contains the cell.


  I think this function is too long to be inlined. --hwn
*/
long int
scm_i_find_heap_segment_containing_object (SCM obj)
{
  if (!CELL_P (obj))
    return -1;

  if ((scm_t_cell* ) obj < lowest_cell || (scm_t_cell*) obj >= highest_cell)
    return -1;

  
    {
      scm_t_cell *  ptr = SCM2PTR (obj);
      unsigned long int i = 0;
      unsigned long int j = scm_i_heap_segment_table_size - 1;

      if (ptr < scm_i_heap_segment_table[i]->bounds[0])
	return -1;
      else if (scm_i_heap_segment_table[j]->bounds[1] <= ptr)
	return -1;
      else
	{
	  while (i < j)
	    {
	      if (ptr < scm_i_heap_segment_table[i]->bounds[1])
		{
		  break;
		}
	      else if (scm_i_heap_segment_table[j]->bounds[0] <= ptr)
		{
		  i = j;
		  break;
		}
	      else
		{
		  unsigned long int k = (i + j) / 2;

		  if (k == i)
		    return -1;
		  else if (ptr <  scm_i_heap_segment_table[k]->bounds[1])
		    {
		      j = k;
		      ++i;
		      if (ptr <  scm_i_heap_segment_table[i]->bounds[0])
			return -1;
		    }
		  else if (scm_i_heap_segment_table[k]->bounds[0] <= ptr)
		    {
		      i = k;
		      --j;
		      if (scm_i_heap_segment_table[j]->bounds[1] <= ptr)
			return -1;
		    }
		}
	    }

	  if (!DOUBLECELL_ALIGNED_P (obj) && scm_i_heap_segment_table[i]->span == 2)
	    return -1;
	  else if (SCM_GC_IN_CARD_HEADERP (ptr))
	    return -1;
	  else
	    return i;
	}
    }
}


/*
  Important entry point: try to grab some memory, and make it into a
  segment.

  RETURN: the index of the segment.
 */
int 
scm_i_get_new_heap_segment (scm_t_cell_type_statistics *freelist, policy_on_error error_policy)
{
  size_t len;

  if (scm_gc_heap_lock)
    {
      /* Critical code sections (such as the garbage collector) aren't
       * supposed to add heap segments.
       */
      fprintf (stderr, "scm_i_get_new_heap_segment: Can not extend locked heap.\n");
      abort ();
    }


  /* Pick a size for the new heap segment.
   * The rule for picking the size of a segment is explained in
   * gc.h
   */
  {
    /* Assure that the new segment is predicted to be large enough.
     *
     * New yield should at least equal GC fraction of new heap size, i.e.
     *
     *   y + dh > f * (h + dh)
     *
     *    y : yield
     *    f : min yield fraction
     *    h : heap size
     *   dh : size of new heap segment
     *
     * This gives dh > (f * h - y) / (1 - f)
     */

    /*
      where is is this explanation supposed to be?  --hwn
     */
    int f = freelist->min_yield_fraction;
    unsigned long h = SCM_HEAP_SIZE;
    size_t min_cells = (f * h - 100 * (long) scm_gc_cells_collected) / (99 - f);

    /* Make heap grow with factor 1.5 */
    len =  freelist->heap_size / 2;
#ifdef DEBUGINFO
    fprintf (stderr, "(%ld < %ld)", (long) len, (long) min_cells);
#endif

    /*
      Original code adds freelist->cluster_size here.
     */
    if (len < min_cells)
      len = min_cells;  
    len *= sizeof (scm_t_cell);
    /* force new sampling */
    freelist->collected = LONG_MAX;
  }

  if (len > SCM_MIN_HEAP_SEG_SIZE)
    len = SCM_MIN_HEAP_SEG_SIZE;

  {
    scm_t_heap_segment * seg = scm_i_make_empty_heap_segment (freelist);

    /* Allocate with decaying ambition. */
    while (len >= SCM_MIN_HEAP_SEG_SIZE)
      {
	if (scm_i_initialize_heap_segment_data (seg, len))
	  {
	    return scm_i_insert_segment (seg);
	  }
	
	len /= 2;
      }
  }

  if (error_policy == abort_on_error)
    {
      fprintf (stderr, "scm_i_get_new_heap_segment: Could not grow heap.\n");
      abort ();
    }
  return -1;
}



void
scm_i_make_initial_segment (size_t init_heap_size, scm_t_cell_type_statistics *freelist)
{
  scm_t_heap_segment * seg = scm_i_make_empty_heap_segment (freelist);
 
  if (scm_i_initialize_heap_segment_data (seg, init_heap_size))
    {
      freelist->heap_segment_idx = scm_i_insert_segment (seg);
    }

  /*
    Why the fuck  try twice? --hwn
   */
  if (!seg->malloced)
    {
      scm_i_initialize_heap_segment_data (seg, SCM_HEAP_SEG_SIZE);
    }

  if (freelist->min_yield_fraction)
    freelist->min_yield = (freelist->heap_size * freelist->min_yield_fraction
			    / 100);
}
