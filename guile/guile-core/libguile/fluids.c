/* Copyright (C) 1996,1997,2000,2001, 2004 Free Software Foundation, Inc.
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
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/print.h"
#include "libguile/smob.h"
#include "libguile/dynwind.h"
#include "libguile/fluids.h"
#include "libguile/alist.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/deprecation.h"
#include "libguile/lang.h"
#include "libguile/validate.h"

#define INITIAL_FLUIDS  20
#define FLUID_INCREMENT 20

scm_t_bits scm_tc16_fluid;

static size_t allocated_fluids_len;
static char *allocated_fluids;

SCM
scm_i_make_initial_fluids ()
{
  return scm_c_make_vector (allocated_fluids_len, SCM_BOOL_F);
}

static void
grow_fluids (scm_root_state *root_state, int new_length)
{
  SCM old_fluids, new_fluids;
  long old_length, i;

  old_fluids = root_state->fluids;
  old_length = SCM_SIMPLE_VECTOR_LENGTH (old_fluids);
  new_fluids = scm_c_make_vector (new_length, SCM_BOOL_F);
  i = 0;
  while (i < old_length)
    {
      SCM_SIMPLE_VECTOR_SET (new_fluids, i,
			     SCM_SIMPLE_VECTOR_REF (old_fluids, i));
      i++;
    }

  root_state->fluids = new_fluids;
}

void
scm_i_copy_fluids (scm_root_state *root_state)
{
  grow_fluids (root_state, SCM_SIMPLE_VECTOR_LENGTH (root_state->fluids));
}

static size_t
fluid_free (SCM fluid)
{
  //fprintf (stderr, "freeing fluid %d\n", SCM_FLUID_NUM (fluid));
  allocated_fluids[SCM_FLUID_NUM (fluid)] = 0;
  return 0;
}

static int
fluid_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<fluid ", port);
  scm_intprint ((int) SCM_FLUID_NUM (exp), 10, port);
  scm_putc ('>', port);
  return 1;
}

static long
next_fluid_num ()
{
  long n;

  scm_frame_begin (0);
  scm_frame_pthread_mutex_lock (&scm_i_misc_mutex);

  for (n = 0; n < allocated_fluids_len; n++)
    if (allocated_fluids[n] ==  0)
      {
	allocated_fluids[n] = 1;
	break;
      }

  if (n == allocated_fluids_len)
    {
      allocated_fluids = scm_realloc (allocated_fluids,
				      allocated_fluids_len +  FLUID_INCREMENT);
      memset (allocated_fluids + allocated_fluids_len, FLUID_INCREMENT, 0);
      allocated_fluids_len += FLUID_INCREMENT;
      allocated_fluids[n] = 1;
    }

  scm_frame_end ();
  //fprintf (stderr, "allocated fluid %ld\n", n);
  return n;
}

SCM_DEFINE (scm_make_fluid, "make-fluid", 0, 0, 0, 
	    (),
	    "Return a newly created fluid.\n"
	    "Fluids are objects of a certain type (a smob) that can hold one SCM\n"
	    "value per dynamic root.  That is, modifications to this value are\n"
	    "only visible to code that executes within the same dynamic root as\n"
	    "the modifying code.  When a new dynamic root is constructed, it\n"
	    "inherits the values from its parent.  Because each thread executes\n"
	    "in its own dynamic root, you can use fluids for thread local storage.")
#define FUNC_NAME s_scm_make_fluid
{
  long n;

  n = next_fluid_num ();
  SCM_RETURN_NEWSMOB (scm_tc16_fluid, n);
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_p, "fluid?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} iff @var{obj} is a fluid; otherwise, return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_fluid_p
{
  return scm_from_bool (SCM_FLUIDP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_ref, "fluid-ref", 1, 0, 0, 
	    (SCM fluid),
	    "Return the value associated with @var{fluid} in the current\n"
	    "dynamic root.  If @var{fluid} has not been set, then return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_fluid_ref
{
  unsigned long int n;

  SCM_VALIDATE_FLUID (1, fluid);
  n = SCM_FLUID_NUM (fluid);

  if (SCM_SIMPLE_VECTOR_LENGTH (scm_root->fluids) <= n)
    grow_fluids (scm_root, n+1);
  return SCM_SIMPLE_VECTOR_REF (scm_root->fluids, n);
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_set_x, "fluid-set!", 2, 0, 0,
	    (SCM fluid, SCM value),
	    "Set the value associated with @var{fluid} in the current dynamic root.")
#define FUNC_NAME s_scm_fluid_set_x
{
  unsigned long int n;

  SCM_VALIDATE_FLUID (1, fluid);
  n = SCM_FLUID_NUM (fluid);

  if (SCM_SIMPLE_VECTOR_LENGTH (scm_root->fluids) <= n)
    grow_fluids (scm_root, n+1);
  SCM_SIMPLE_VECTOR_SET (scm_root->fluids, n, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void
swap_fluids (SCM data)
{
  SCM fluids = SCM_CAR (data), vals = SCM_CDR (data);
  
  while (!SCM_NULL_OR_NIL_P (fluids))
    {
      SCM fl = SCM_CAR (fluids);
      SCM old_val = scm_fluid_ref (fl);
      scm_fluid_set_x (fl, SCM_CAR (vals));
      SCM_SETCAR (vals, old_val);
      fluids = SCM_CDR (fluids);
      vals = SCM_CDR (vals);
    }
}

/* Swap the fluid values in reverse order.  This is important when the
same fluid appears multiple times in the fluids list. */

static void
swap_fluids_reverse_aux (SCM fluids, SCM vals)
{
  if (!SCM_NULL_OR_NIL_P (fluids))
    {
      SCM fl, old_val;

      swap_fluids_reverse_aux (SCM_CDR (fluids), SCM_CDR (vals));
      fl = SCM_CAR (fluids);
      old_val = scm_fluid_ref (fl);
      scm_fluid_set_x (fl, SCM_CAR (vals));
      SCM_SETCAR (vals, old_val);
    }
}

static void
swap_fluids_reverse (SCM data)
{
  swap_fluids_reverse_aux (SCM_CAR (data), SCM_CDR (data));
}

static SCM
apply_thunk (void *thunk)
{
  return scm_call_0 (SCM_PACK (thunk));
}

SCM_DEFINE (scm_with_fluids, "with-fluids*", 3, 0, 0, 
	    (SCM fluids, SCM values, SCM thunk),
	    "Set @var{fluids} to @var{values} temporary, and call @var{thunk}.\n"
	    "@var{fluids} must be a list of fluids and @var{values} must be the same\n"
	    "number of their values to be applied.  Each substitution is done\n"
	    "one after another.  @var{thunk} must be a procedure with no argument.")
#define FUNC_NAME s_scm_with_fluids
{
  return scm_c_with_fluids (fluids, values,
			    apply_thunk, (void *) SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM
scm_c_with_fluids (SCM fluids, SCM values, SCM (*cproc) (), void *cdata)
#define FUNC_NAME "scm_c_with_fluids"
{
  SCM ans, data;
  long flen, vlen;

  SCM_VALIDATE_LIST_COPYLEN (1, fluids, flen);
  SCM_VALIDATE_LIST_COPYLEN (2, values, vlen);
  if (flen != vlen)
    scm_out_of_range (s_scm_with_fluids, values);

  if (flen == 1)
    return scm_c_with_fluid (SCM_CAR (fluids), SCM_CAR (values),
			     cproc, cdata);
  
  data = scm_cons (fluids, values);
  scm_frame_begin (SCM_F_FRAME_REWINDABLE);
  scm_frame_rewind_handler_with_scm (swap_fluids, data,
				     SCM_F_WIND_EXPLICITLY);
  scm_frame_unwind_handler_with_scm (swap_fluids_reverse, data,
				     SCM_F_WIND_EXPLICITLY);
  ans = cproc (cdata);
  scm_frame_end ();
  return ans;
}
#undef FUNC_NAME

SCM_DEFINE (scm_with_fluid, "with-fluid*", 3, 0, 0, 
	    (SCM fluid, SCM value, SCM thunk),
	    "Set @var{fluid} to @var{value} temporarily, and call @var{thunk}.\n"
	    "@var{thunk} must be a procedure with no argument.")
#define FUNC_NAME s_scm_with_fluid
{
  return scm_c_with_fluid (fluid, value,
			   apply_thunk, (void *) SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM
scm_c_with_fluid (SCM fluid, SCM value, SCM (*cproc) (), void *cdata)
#define FUNC_NAME "scm_c_with_fluid"
{
  SCM ans;

  scm_frame_begin (SCM_F_FRAME_REWINDABLE);
  scm_frame_fluid (fluid, value);
  ans = cproc (cdata);
  scm_frame_end ();
  return ans;
}
#undef FUNC_NAME

static void
swap_fluid (SCM data)
{
  SCM f = SCM_CAR (data);
  SCM t = scm_fluid_ref (f);
  scm_fluid_set_x (f, SCM_CDR (data));
  SCM_SETCDR (data, t);
}

void
scm_frame_fluid (SCM fluid, SCM value)
{
  SCM data = scm_cons (fluid, value);
  scm_frame_rewind_handler_with_scm (swap_fluid, data, SCM_F_WIND_EXPLICITLY);
  scm_frame_unwind_handler_with_scm (swap_fluid, data, SCM_F_WIND_EXPLICITLY);
}

void
scm_init_fluids ()
{
  scm_tc16_fluid = scm_make_smob_type ("fluid", 0);
  scm_set_smob_free (scm_tc16_fluid, fluid_free);
  scm_set_smob_print (scm_tc16_fluid, fluid_print);

  allocated_fluids_len = INITIAL_FLUIDS;
  allocated_fluids = scm_calloc (allocated_fluids_len);

#include "libguile/fluids.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
