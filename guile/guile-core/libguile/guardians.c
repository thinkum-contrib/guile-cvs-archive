/*	Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
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



/* This is an implementation of guardians as described in
 * R. Kent Dybvig, Carl Bruggeman, and David Eby (1993) "Guardians in
 * a Generation-Based Garbage Collector" ACM SIGPLAN Conference on
 * Programming Language Design and Implementation, June 1993
 * ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/guardians.ps.gz
 *
 * Author:      Michael N. Livshin
 * Modified by: Mikael Djurfeldt
 */

#include <stdio.h>
#include <assert.h>

#include "_scm.h"
#include "ports.h"
#include "print.h"
#include "smob.h"
#include "vectors.h"

#include "validate.h"
#include "guardians.h"

static long scm_tc16_guardian;

/* The live and zombies FIFOs are implemented as tconcs as described
   in Dybvig's paper.  This decouples addition and removal of elements
   so that no synchronization between these needs to take place.
*/
#define TCONC_IN(tc, obj, pair) \
do { \
  SCM_SETCAR ((tc).tail, obj); \
  SCM_SETCAR (pair, SCM_BOOL_F); \
  SCM_SETCDR (pair, SCM_EOL); \
  SCM_SETCDR ((tc).tail, pair); \
  (tc).tail = pair; \
} while (0)

#define TCONC_OUT(tc, res) \
do { \
  (res) = SCM_CAR ((tc).head); \
  (tc).head = SCM_CDR ((tc).head); \
} while (0)

#define TCONC_EMPTYP(tc) (SCM_EQ_P ((tc).head, (tc).tail))

typedef struct tconc_t
{
  SCM head;
  SCM tail;
} tconc_t;

typedef struct guardian_t
{
  tconc_t live;
  tconc_t zombies;
  struct guardian_t *next;
} guardian_t;

#define GUARDIAN(x) ((guardian_t *) SCM_CELL_WORD_1 (x))
#define GUARDIAN_LIVE(x) (GUARDIAN (x)->live)
#define GUARDIAN_ZOMBIES(x) (GUARDIAN (x)->zombies)
#define GUARDIAN_NEXT(x) (GUARDIAN (x)->next)

#define CCLO_G(cclo) (SCM_VELTS (cclo)[1])

/* subr constructed from guard below.  */
static SCM guard1;

/* this is wrapped in a compiled closure and is the Scheme entry point
   for each guardian: if arg is an object, it's added to the
   guardian's live list.  if arg is unbound, the next available
   zombified object (or #f if none) is returned.  */
static SCM
guard (SCM cclo, SCM arg)
{
  if (!SCM_UNBNDP (arg))
    {
      scm_guard (cclo, arg);
      return SCM_UNSPECIFIED;
    }
  else
    return scm_get_one_zombie (cclo);
}

void
scm_guard (SCM guardian, SCM obj)
{
  SCM g = CCLO_G (guardian);

  if (SCM_NIMP (obj))
    {
      SCM z;
      
      SCM_NEWCELL (z);

      /* This critical section barrier will be replaced by a mutex. */
      SCM_DEFER_INTS;
      TCONC_IN (GUARDIAN_LIVE (g), obj, z);
      SCM_ALLOW_INTS;
    }
}

SCM
scm_get_one_zombie (SCM guardian)
{
  SCM g = CCLO_G (guardian);
  SCM res = SCM_BOOL_F;

  /* This critical section barrier will be replaced by a mutex. */
  SCM_DEFER_INTS;
  if (!TCONC_EMPTYP (GUARDIAN_ZOMBIES (g)))
    TCONC_OUT (GUARDIAN_ZOMBIES (g), res);
  SCM_ALLOW_INTS;
  return res;
}

SCM_DEFINE (scm_make_guardian, "make-guardian", 0, 0, 0, 
            (),
            "Create a new guardian.\n"
	    "A guardian protects a set of objects from garbage collection,\n"
	    "allowing a program to apply cleanup or other actions.\n\n"

	    "make-guardian returns a procedure representing the guardian.\n"
	    "Calling the guardian procedure with an argument adds the\n"
	    "argument to the guardian's set of protected objects.\n"
	    "Calling the guardian procedure without an argument returns\n"
	    "one of the protected objects which are ready for garbage\n"
	    "collection or @code{#f} if no such object is available.\n"
	    "Objects which are returned in this way are removed from\n"
	    "the guardian.\n\n".

            "See R. Kent Dybvig, Carl Bruggeman, and David Eby (1993)\n"
            "\"Guardians in a Generation-Based Garbage Collector\".\n"
            "ACM SIGPLAN Conference on Programming Language Design\n"
            "and Implementation, June 1993.")
#define FUNC_NAME s_scm_make_guardian
{
  SCM cclo = scm_makcclo (guard1, 2L);
  guardian_t *g = SCM_MUST_MALLOC_TYPE(guardian_t);
  SCM z1 = scm_cons (SCM_BOOL_F, SCM_EOL);
  SCM z2 = scm_cons (SCM_BOOL_F, SCM_EOL);
  SCM z;

  /* A tconc starts out with one tail pair. */
  g->live.head = g->live.tail = z1;
  g->zombies.head = g->zombies.tail = z2;

  SCM_NEWSMOB (z, scm_tc16_guardian, g);

  CCLO_G (cclo) = z;

  return cclo;
}
#undef FUNC_NAME

/* during the gc mark phase, live guardians are linked into a list
   here.  */
static guardian_t *first_live_guardian = NULL;
static guardian_t **current_link_field = NULL;

/* called before gc mark phase begins to initialise the live guardian
   list.  */
void
scm_guardian_gc_init()
{
  current_link_field = &first_live_guardian;
  first_live_guardian = NULL;
}

/* mark a guardian by adding it to the live guardian list.  */
static SCM
g_mark (SCM ptr)
{
  *current_link_field = GUARDIAN (ptr);
  current_link_field = &GUARDIAN_NEXT (ptr);
  GUARDIAN_NEXT (ptr) = NULL;

  /* the objects protected by the guardian are not marked here: that
     would prevent them from ever getting collected.  instead marking
     is done at the end of the mark phase by scm_guardian_zombify.  */
  return SCM_BOOL_F;
}

/* this is called by the garbage collector between the mark and sweep
   phases.  for each marked guardian, it moves any unmarked object in
   its live list (tconc) to its zombie list (tconc).  */
void scm_guardian_zombify (void)
{
  guardian_t *g;

  /* Note that new guardians may be stuck on the end of the live
     guardian list as we run this loop.  As we move unmarked objects
     to the zombie list and mark them, we may find some guarded
     guardians.  The guardian mark function will stick them on the end
     of this list, so they'll be processed properly.  */

  for (g = first_live_guardian; g; g = g->next)
    {
      SCM tconc_tail = g->live.tail;
      SCM *prev_ptr = &g->live.head;
      SCM pair = g->live.head;

      while (! SCM_EQ_P (pair, tconc_tail))
	{
	  SCM next_pair = SCM_CDR (pair);

	  if (SCM_NMARKEDP (SCM_CAR (pair)))
	    {
	      /* got you, zombie! */

	      /* out of the live list! */
	      *prev_ptr = next_pair;

	      /* into the zombie list! */
	      TCONC_IN (g->zombies, SCM_CAR (pair), pair);
	    }
	  else
	    prev_ptr = SCM_CDRLOC (pair);

	  pair = next_pair;
	}

      /* Mark the cells of the live list (yes, the cells in the list,
	 even though we don't care about objects pointed to by the list
	 cars, since we know they are already marked).  */
      for (pair = g->live.head; SCM_NIMP (pair); pair = SCM_GCCDR (pair))
      	SCM_SETGCMARK (pair);

      /* Preserve the zombies in their undead state, by marking to
	 prevent collection.  */

      /* ghouston: possible bug: this may mark objects which are
	 protected by other guardians, but which have no references
	 from outside of the guardian system.  section 3 of the paper
	 mentions shared and cyclic objects and it seems that all
	 parts should be made available for collection.  Currently the
	 behaviour depends on the order in which guardians are
	 scanned.

	 Doesn't it seem a bit disturbing that if a zombie is returned
	 to full life after getting returned from the guardian
	 procedure, it may reference objects which are in a guardian's
	 zombie list?  Is it not necessary to move such zombies back
	 to the live list, to avoid allowing the guardian procedure to
	 return an object which is referenced, so not collectable?
	 The paper doesn't give this impression.  */

      scm_gc_mark (g->zombies.head);
    }
}

/* not generally used, since guardian smob is wrapped in a closure.
   maybe useful for debugging.  */
static int
g_print (SCM exp, SCM port, scm_print_state *pstate)
{
  char buf[256];
  sprintf (buf, "#<guardian live objs: %lu zombies: %lu>",
	   scm_ilength (SCM_CDR (GUARDIAN_LIVE (exp).head)),
	   scm_ilength (SCM_CDR (GUARDIAN_ZOMBIES (exp).head)));
  scm_puts (buf, port);

  return 1;
}

void
scm_init_guardian()
{
  scm_tc16_guardian = scm_make_smob_type_mfpe ("guardian", sizeof (guardian_t),
					       g_mark, NULL, g_print, NULL);
  guard1 = scm_make_subr_opt ("guardian", scm_tc7_subr_2o, guard, 0);

#include "guardians.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
