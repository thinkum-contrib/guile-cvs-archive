/* classes: h_files */

#ifndef GCH
#define GCH
/* Copyright (C) 1995, 96, 98, 99, 2000 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/__scm.h"

#include "libguile/hooks.h"



typedef struct scm_cell
{
  scm_bits_t word_0;
  scm_bits_t word_1;
} scm_cell;


/* SCM_CELLPTR is a pointer to a cons cell which may be compared or
 * differenced.
 */
typedef scm_cell * SCM_CELLPTR;


/* Cray machines have pointers that are incremented once for each word,
 * rather than each byte, the 3 most significant bits encode the byte
 * within the word.  The following macros deal with this by storing the
 * native Cray pointers like the ones that looks like scm expects.  This
 * is done for any pointers that might appear in the car of a scm_cell,
 *  pointers to scm_vector elts, functions, &c are not munged.
 */
#ifdef _UNICOS
    #define SCM2PTR(x) ((SCM_CELLPTR) (SCM_UNPACK (x) >> 3))
    #define PTR2SCM(x) (SCM_PACK (((scm_bits_t) (x)) << 3))
#else
    #define SCM2PTR(x) ((SCM_CELLPTR) (SCM_UNPACK (x)))
    #define PTR2SCM(x) (SCM_PACK ((scm_bits_t) (x)))
#endif /* def _UNICOS */


/* Low level cell data accessing macros:
 */

#define SCM_CELL_WORD(x, n) (((scm_bits_t *) SCM2PTR (x)) [n])
#define SCM_CELL_WORD_0(x) SCM_CELL_WORD (x, 0)
#define SCM_CELL_WORD_1(x) SCM_CELL_WORD (x, 1)
#define SCM_CELL_WORD_2(x) SCM_CELL_WORD (x, 2)
#define SCM_CELL_WORD_3(x) SCM_CELL_WORD (x, 3)

#define SCM_CELL_OBJECT(x, n) (SCM_PACK (((scm_bits_t *) SCM2PTR (x)) [n]))
#define SCM_CELL_OBJECT_0(x) SCM_CELL_OBJECT (x, 0)
#define SCM_CELL_OBJECT_1(x) SCM_CELL_OBJECT (x, 1)
#define SCM_CELL_OBJECT_2(x) SCM_CELL_OBJECT (x, 2)
#define SCM_CELL_OBJECT_3(x) SCM_CELL_OBJECT (x, 3)

#define SCM_SET_CELL_WORD(x, n, v) ((((scm_bits_t *) SCM2PTR (x)) [n]) = (scm_bits_t) (v))
#define SCM_SET_CELL_WORD_0(x, v) SCM_SET_CELL_WORD (x, 0, v)
#define SCM_SET_CELL_WORD_1(x, v) SCM_SET_CELL_WORD (x, 1, v)
#define SCM_SET_CELL_WORD_2(x, v) SCM_SET_CELL_WORD (x, 2, v)
#define SCM_SET_CELL_WORD_3(x, v) SCM_SET_CELL_WORD (x, 3, v)

#define SCM_SET_CELL_OBJECT(x, n, v) ((((scm_bits_t *) SCM2PTR (x)) [n]) = SCM_UNPACK (v))
#define SCM_SET_CELL_OBJECT_0(x, v) SCM_SET_CELL_OBJECT (x, 0, v)
#define SCM_SET_CELL_OBJECT_1(x, v) SCM_SET_CELL_OBJECT (x, 1, v)
#define SCM_SET_CELL_OBJECT_2(x, v) SCM_SET_CELL_OBJECT (x, 2, v)
#define SCM_SET_CELL_OBJECT_3(x, v) SCM_SET_CELL_OBJECT (x, 3, v)

#define SCM_CELL_TYPE(x) SCM_CELL_WORD_0 (x)
#define SCM_SET_CELL_TYPE(x, t) SCM_SET_CELL_WORD_0 (x, t)

#define SCM_SETAND_CAR(x, y) \
  (SCM_SETCAR ((x), SCM_PACK (SCM_UNPACK (SCM_CAR (x)) & (y))))
#define SCM_SETAND_CDR(x, y)\
  (SCM_SETCDR ((x), SCM_PACK (SCM_UNPACK (SCM_CDR (x)) & (y))))
#define SCM_SETOR_CAR(x, y)\
  (SCM_SETCAR ((x), SCM_PACK (SCM_UNPACK (SCM_CAR (x)) | (y))))
#define SCM_SETOR_CDR(x, y)\
  (SCM_SETCDR ((x), SCM_PACK (SCM_UNPACK (SCM_CDR (x)) | (y))))

#define SCM_CELL_WORD_LOC(x, n) (&SCM_CELL_WORD (x, n))
#define SCM_CARLOC(x) (&SCM_CAR (x))
#define SCM_CDRLOC(x) (&SCM_CDR (x))


/* SCM_PTR_LT defines how to compare two SCM_CELLPTRs (which may not be in the
 * same scm_array).
 */

#ifdef nosve
    #define SCM_PTR_MASK      0xffffffffffff
    #define SCM_PTR_LT(x, y)  (((int) (x) & SCM_PTR_MASK) < ((int) (y) & SCM_PTR_MASK))
#else
    #define SCM_PTR_LT(x, y)  ((x) < (y))
#endif /* def nosve */

#define SCM_PTR_GT(x, y) SCM_PTR_LT (y, x)
#define SCM_PTR_LE(x, y) (!SCM_PTR_GT (x, y))
#define SCM_PTR_GE(x, y) (!SCM_PTR_LT (x, y))


/* Dirk:FIXME:: */
/* Freelists consist of linked cells where the type entry holds the value
 * scm_tc_free_cell and the second entry holds a pointer to the next cell of
 * the freelist.  Due to this structure, freelist cells are not cons cells
 * and thus may not be accessed using SCM_CAR and SCM_CDR.
 */

/* the allocated thing:  The car of new cells is set to
   scm_tc16_allocated to avoid the fragile state of newcells wrt the
   gc.  If it stays as a freecell, any allocation afterwards could
   cause the cell to go back on the freelist, which will bite you
   sometime afterwards. */

#ifdef GUILE_DEBUG_FREELIST
#define SCM_NEWCELL(_into) do { _into = scm_debug_newcell (); } while (0)
#define SCM_NEWCELL2(_into) do { _into = scm_debug_newcell2 (); } while (0)
#else
#ifdef GUILE_NEW_GC_SCHEME
/* When we introduce POSIX threads support, every thread will have
   a freelist of its own.  Then it won't any longer be necessary to
   initialize cells with scm_tc16_allocated.  */
#define SCM_NEWCELL(_into) \
        do { \
          if (SCM_IMP (scm_freelist)) \
             _into = scm_gc_for_newcell (&scm_master_freelist, \
                                         &scm_freelist); \
          else \
            { \
               _into = scm_freelist; \
               scm_freelist = SCM_CDR (scm_freelist); \
               SCM_SET_CELL_TYPE (_into, scm_tc16_allocated); \
            } \
        } while(0)
#define SCM_NEWCELL2(_into) \
        do { \
          if (SCM_IMP (scm_freelist2)) \
             _into = scm_gc_for_newcell (&scm_master_freelist2, \
                                         &scm_freelist2); \
          else \
            { \
               _into = scm_freelist2; \
               scm_freelist2 = SCM_CDR (scm_freelist2); \
               SCM_SET_CELL_TYPE (_into, scm_tc16_allocated); \
            } \
        } while(0)
#else /* GUILE_NEW_GC_SCHEME */
#define SCM_NEWCELL(_into) \
        do { \
          if (SCM_IMP (scm_freelist.cells)) \
             _into = scm_gc_for_newcell (&scm_freelist); \
          else \
            { \
               _into = scm_freelist.cells; \
               scm_freelist.cells = SCM_CDR (scm_freelist.cells); \
               SCM_SET_CELL_TYPE (_into, scm_tc16_allocated); \
               ++scm_cells_allocated; \
            } \
        } while(0)
#define SCM_NEWCELL2(_into) \
        do { \
          if (SCM_IMP (scm_freelist2.cells)) \
             _into = scm_gc_for_newcell (&scm_freelist2); \
          else \
            { \
               _into = scm_freelist2.cells; \
               scm_freelist2.cells = SCM_CDR (scm_freelist2.cells); \
               SCM_SET_CELL_TYPE (_into, scm_tc16_allocated); \
               scm_cells_allocated += 2; \
            } \
        } while(0)
#endif /* GUILE_NEW_GC_SCHEME */
#endif


#define SCM_FREEP(x) (SCM_NIMP (x) && (SCM_CELL_TYPE (x) == scm_tc_free_cell))
#define SCM_NFREEP(x) (!SCM_FREEP (x))

/* 1. This shouldn't be used on immediates.
   2. It thinks that subrs are always unmarked (harmless). */
#define SCM_MARKEDP(x) ((SCM_CELL_TYPE (x) & 5) == 5 \
			? SCM_GC8MARKP (x) \
			: SCM_GCMARKP (x))
#define SCM_NMARKEDP(x) (!SCM_MARKEDP (x))

extern struct scm_heap_seg_data_t *scm_heap_table;
extern int scm_n_heap_segs;
extern int scm_take_stdin;
extern int scm_block_gc;
extern int scm_gc_heap_lock;


extern scm_sizet scm_max_segment_size;
extern SCM_CELLPTR scm_heap_org;
#ifdef GUILE_NEW_GC_SCHEME
extern SCM scm_freelist;
extern struct scm_freelist_t scm_master_freelist;
extern SCM scm_freelist2;
extern struct scm_freelist_t scm_master_freelist2;
#else
extern struct scm_freelist_t scm_freelist;
extern struct scm_freelist_t scm_freelist2;
#endif
extern unsigned long scm_gc_cells_collected;
#ifdef GUILE_NEW_GC_SCHEME
extern unsigned long scm_gc_yield;
#endif
extern unsigned long scm_gc_malloc_collected;
extern unsigned long scm_gc_ports_collected;
extern unsigned long scm_cells_allocated;
extern long scm_mallocated;
extern unsigned long scm_mtrigger;

extern SCM scm_after_gc_hook;

extern scm_c_hook_t scm_before_gc_c_hook;
extern scm_c_hook_t scm_before_mark_c_hook;
extern scm_c_hook_t scm_before_sweep_c_hook;
extern scm_c_hook_t scm_after_sweep_c_hook;
extern scm_c_hook_t scm_after_gc_c_hook;

#if defined (GUILE_DEBUG) || defined (GUILE_DEBUG_FREELIST)
extern SCM scm_map_free_list (void);
extern SCM scm_free_list_length (void);
#endif
#ifdef GUILE_DEBUG_FREELIST
extern SCM scm_debug_newcell (void);
extern SCM scm_debug_newcell2 (void);
extern SCM scm_gc_set_debug_check_freelist_x (SCM flag);
#endif



extern SCM scm_object_address (SCM obj);
extern SCM scm_unhash_name (SCM name);
extern SCM scm_gc_stats (void);
extern void scm_gc_start (const char *what);
extern void scm_gc_end (void);
extern SCM scm_gc (void);
extern void scm_gc_for_alloc (struct scm_freelist_t *freelist);
#ifdef GUILE_NEW_GC_SCHEME
extern SCM scm_gc_for_newcell (struct scm_freelist_t *master, SCM *freelist);
#if 0
extern void scm_alloc_cluster (struct scm_freelist_t *master);
#endif
#else
extern SCM scm_gc_for_newcell (struct scm_freelist_t *freelist);
#endif
extern void scm_igc (const char *what);
extern void scm_gc_mark (SCM p);
extern void scm_mark_locations (SCM_STACKITEM x[], scm_sizet n);
extern int scm_cellp (SCM value);
extern void scm_gc_sweep (void);
extern void * scm_must_malloc (scm_sizet len, const char *what);
extern void * scm_must_realloc (void *where,
				scm_sizet olen, scm_sizet len,
				const char *what);
extern void scm_done_malloc (long size);
extern void scm_must_free (void *obj);
extern void scm_remember (SCM * ptr);
extern SCM scm_return_first (SCM elt, ...);
extern int scm_return_first_int (int x, ...);
extern SCM scm_permanent_object (SCM obj);
extern SCM scm_protect_object (SCM obj);
extern SCM scm_unprotect_object (SCM obj);
#ifdef GUILE_NEW_GC_SCHEME
extern int scm_init_storage (scm_sizet init_heap_size, int trig,
                             scm_sizet init_heap2_size, int trig2,
			     scm_sizet max_segment_size);
#else
extern int scm_init_storage (scm_sizet init_heap_size,
                             scm_sizet init_heap2_size);
#endif
extern void scm_init_gc (void);
#endif  /* GCH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
