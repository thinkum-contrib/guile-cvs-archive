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


#include <stdio.h>
#include <libguile.h>
#include "compat.h"
#include <tk.h>
#include "guile-tcl.h"

#include "guile-tk.h"



SCM_PROC(s_tk_init_main_window, "tk-init-main-window", 4, 0, 0, scm_tk_init_main_window);
SCM scm_tk_init_main_window (SCM tobj, SCM display, SCM name, SCM class);
SCM
scm_tk_init_main_window (tobj, display, name, class)
     SCM tobj;
     SCM display;
     SCM name;
     SCM class;
{
  int status;

  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tk_init_main_window);
  SCM_ASSERT (SCM_NIMP (display) && SCM_STRINGP (display),
	  display, SCM_ARG2, s_tk_init_main_window);
  SCM_ASSERT (SCM_NIMP (name) && SCM_STRINGP (name), name, SCM_ARG3,
	      s_tk_init_main_window);
  SCM_ASSERT (SCM_NIMP (class) && SCM_STRINGP (class), class, SCM_ARG4,
	      s_tk_init_main_window);

  SCM_DEFER_INTS;
  status = Tcl_Init(SCM_TERP (tobj));
  SCM_ALLOW_INTS;

  if (status == TCL_ERROR)
    return scm_makfrom0str (SCM_TERP (tobj)->result);

  SCM_DEFER_INTS;
  status = Tk_Init(SCM_TERP (tobj));
  SCM_ALLOW_INTS;
  if (status == TCL_ERROR)
    return scm_makfrom0str (SCM_TERP (tobj)->result);

  SCM_DEFER_INTS;
  Tcl_SetVar (SCM_TERP (tobj), "tcl_interactive", "0", TCL_GLOBAL_ONLY);
  SCM_ALLOW_INTS;

  return SCM_BOOL_T;
}

static int in_tk_loop_p = 0;

SCM_PROC (s_tk_loop_p, "tk-loop?", 0, 0, 0, scm_tk_loop_p);
SCM
scm_tk_loop_p ()
{
  return in_tk_loop_p ? SCM_BOOL_T : SCM_BOOL_F;
}

#ifdef USE_THREADS

static SCM
main_loop (SCM loop_invocation)
{
  int events;
  in_tk_loop_p = 1;
  scm_mutex_lock (&scm_tcl_mutex);
  while (Tk_GetNumMainWindows () > 0)
    {
      if (!scm_tcl_handle_event_p)
	scm_cond_wait (&scm_tcl_condvar, &scm_tcl_mutex);
      scm_tcl_handle_event_p = 0;
      do
	{
	  SCM_DEFER_INTS;
	  scm_mask_ints = 1;
	  events = Tcl_DoOneEvent (TCL_ALL_EVENTS | TCL_DONT_WAIT);
	  scm_mask_ints = 0;
	  SCM_ALLOW_INTS;
	  SCM_ASYNC_TICK;
	}
      while (events);
    }
  scm_mutex_unlock (&scm_tcl_mutex);
  SCM_SETCAR (loop_invocation, SCM_BOOL_F);
  in_tk_loop_p = 0;
  return SCM_UNSPECIFIED;
}

static SCM
main_loop_handler (SCM loop_invocation, SCM tag, SCM throw_args)
{
  scm_mutex_unlock (&scm_tcl_mutex);
  SCM_SETCAR (loop_invocation, SCM_BOOL_F);
  in_tk_loop_p = 0;
  scm_mask_ints = 0;
  scm_ithrow (tag, throw_args, 1);
  return SCM_UNSPECIFIED;
}

extern void Tcl_GetCheckMasks (int*, SELECT_TYPE*);

static SCM
io_loop (SCM loop_invocation)
{
  int nfds;
  SELECT_TYPE masks[3];
  while (SCM_NFALSEP (SCM_CAR (loop_invocation))
	 && Tk_GetNumMainWindows () > 0)
    {
      scm_mutex_lock (&scm_tcl_mutex);
      SCM_DEFER_INTS;
      Tcl_GetCheckMasks (&nfds, masks);
      SCM_ALLOW_INTS;
      SCM_ASYNC_TICK;
      scm_mutex_unlock (&scm_tcl_mutex);
      scm_internal_select (nfds, &masks[0], &masks[1], &masks[2], 0);
      scm_tcl_handle_event_p = 1;
      scm_cond_signal (&scm_tcl_condvar);
    }
  return SCM_UNSPECIFIED;
}

static SCM
io_loop_handler (void *dummy, SCM tag, SCM throw_args)
{
  scm_puts ("Internal error in io_loop_handler\n\
Please send a bug-report to bug-guile@gnu.org\n", scm_cur_errp);
  scm_mask_ints = 0;
  scm_handle_by_message (0, tag, throw_args);
  return SCM_UNSPECIFIED;
}

#endif /* USE_THREADS */

SCM_PROC(s_tk_main_loop, "tk-main-loop", 0, 0, 0, scm_tk_main_loop);
SCM scm_tk_main_loop (void);
SCM
scm_tk_main_loop ()
{
#ifdef USE_THREADS
  SCM loop_invocation = scm_cons (SCM_BOOL_T, SCM_BOOL_F);
  if (in_tk_loop_p)
    scm_misc_error (s_tk_main_loop, "Loop already active", SCM_EOL);
  if (Tk_GetNumMainWindows () == 0)
    scm_misc_error (s_tk_main_loop, "No main window active", SCM_EOL);
  scm_spawn_thread ((scm_t_catch_body) io_loop, (void*) loop_invocation,
		    (scm_t_catch_handler) io_loop_handler, NULL);
  scm_tcl_handle_event_p = 1; /* Request an initial call to Tcl_DoOneEvent */
  scm_internal_catch (SCM_BOOL_T,
		      (scm_t_catch_body) main_loop,
		      (void*) loop_invocation,
		      (scm_t_catch_handler) main_loop_handler,
		      (void*) loop_invocation);
#else
  SCM_DEFER_INTS;
  in_tk_loop_p = 1;
  Tk_MainLoop ();
  in_tk_loop_p = 0;
  SCM_ALLOW_INTS;
#endif
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_tk_num_main_windows, "tk-num-main-windows", 0, 0, 0, scm_tk_num_main_windows);
SCM scm_tk_num_main_windows (void);
SCM
scm_tk_num_main_windows ()
{
  int n;
#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  n = SCM_MAKINUM (Tk_GetNumMainWindows ());
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  return n;
}


void scm_init_gtk (void);
void
scm_init_gtk ()
{
#include "guile-tk.x"
}
