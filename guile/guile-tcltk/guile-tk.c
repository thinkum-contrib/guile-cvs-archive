/*	Copyright (C) 1995 Cygnus Support, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, Cygnus Support gives permission
 * for additional uses of the text contained in its release of this library.
 *
 * The exception is that, if you link this library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking this library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by 
 * Cygnus Support as part of this library.  If you copy
 * code from other releases distributed under the terms of the GPL into a copy of
 * this library, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from such code.
 *
 * If you write modifications of your own for this library, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */


#include <stdio.h>
#include "_scm.h"
#include <tk.h>
#include "guile-tcl.h"
#include "guile-tk.h"



SCM_PROC(s_init_main_window, "tk-init-main-window", 4, 0, 0, scm_init_main_window);
SCM scm_init_main_window SCM_P ((SCM tobj, SCM display, SCM name, SCM class));
SCM
scm_init_main_window (tobj, display, name, class)
     SCM tobj;
     SCM display;
     SCM name;
     SCM class;
{
  int status;

  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_init_main_window);
  SCM_ASSERT (SCM_NIMP (display) && SCM_STRINGP (display),
	  display, SCM_ARG2, s_init_main_window);
  SCM_ASSERT (SCM_NIMP (name) && SCM_STRINGP (name), name, SCM_ARG3,
	      s_init_main_window);
  SCM_ASSERT (SCM_NIMP (class) && SCM_STRINGP (class), class, SCM_ARG4,
	      s_init_main_window);

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


SCM_PROC(s_do_one_event, "tk-do-one-event", 1, 0, 0, scm_do_one_event);
SCM scm_do_one_event SCM_P ((SCM flags));
SCM
scm_do_one_event (flags)
     SCM flags;
{
  int answer;
  SCM_ASSERT (SCM_INUMP (flags), flags, SCM_ARG1, s_do_one_event);
  SCM_DEFER_INTS;
  answer = (Tk_DoOneEvent (SCM_INUM (flags)));
  SCM_ALLOW_INTS;
  return SCM_MAKINUM (answer);
}

SCM_PROC(s_main_loop, "tk-main-loop", 0, 0, 0, scm_main_loop);
SCM scm_main_loop SCM_P ((void));
SCM
scm_main_loop ()
{
  SCM_DEFER_INTS;
  Tk_MainLoop ();
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_num_main_windows, "tk-num-main-windows", 0, 0, 0, scm_num_main_windows);
SCM scm_num_main_windows SCM_P ((void));
SCM
scm_num_main_windows ()
{
  return SCM_MAKINUM (Tk_GetNumMainWindows ());
}


void scm_init_gtk SCM_P ((void));
void
scm_init_gtk ()
{
#include "guile-tk.x"
}
