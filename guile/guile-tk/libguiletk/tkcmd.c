/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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


#include <libguile.h>

#include "gstk.h"
#include "tk-glue.h"

#include "tkcmd.h"



SCM scm_class_tk_command;

static SCM
apply_tkcmd (struct Tk_command *W, int argc, char *argv[])
{
  int tkres;

  /* Call the Tk library function */
  Tcl_ResetResult(STk_main_interp);

  tkres = (*W->fct)(W->ptr, STk_main_interp, argc, argv);
  
  /* return result as a string or "evaluated" depending of string_result field */
  if (tkres == TCL_OK)
    return TkResult2Scheme(STk_main_interp);
  
  scm_tk_error (0, STk_main_interp->result, SCM_EOL);
}

static char s_tkcmd_dispatch0[] = "tkcmd-dispatch0";
static SCM
tkcmd_dispatch0 (SCM cmd)
{
  char *argv[2];
  struct Tk_command *W = SCM_TKCMD (cmd);

  /* First initialize an argv array */
  argv[0] = W->Id;
  argv[1] = NULL;

  return apply_tkcmd (W, 1, argv);
}

static char s_tkcmd_dispatch1[] = "tkcmd-dispatch1";
static SCM
tkcmd_dispatch1 (SCM cmd, SCM arg1)
{
  char *argv[3];
  /* 
   * conv_res refers to the values returned by convert_for_Tk. 
   * It serves only to have pointers in the stack on the converted values. 
   * This permits to avoid GC problems (i.e. a GC between 1 and argc 
   * whereas convert_for_Tk has created new cells in a previous iteration 
   */
  SCM conv_res;
  struct Tk_command *W = SCM_TKCMD (cmd);

  /* First initialize an argv array */
  argv[0] = W->Id;
  argv[1] = STk_convert_for_Tk (arg1, &conv_res);
  argv[2] = NULL;

  return apply_tkcmd (W, 2, argv);  
}

static char s_tkcmd_dispatch2[] = "tkcmd-dispatch2";
static SCM
tkcmd_dispatch2 (SCM cmd, SCM arg1, SCM arg2)
{
  char *argv[4];
  SCM conv_res1, conv_res2;
  struct Tk_command *W = SCM_TKCMD (cmd);

  /* First initialize an argv array */
  argv[0] = W->Id;
  argv[1] = STk_convert_for_Tk (arg1, &conv_res1);
  argv[2] = STk_convert_for_Tk (arg2, &conv_res2);
  argv[3] = NULL;

  return apply_tkcmd (W, 3, argv);  
}

static char s_tkcmd_dispatch3[] = "tkcmd-dispatch3";
static SCM
tkcmd_dispatch3 (SCM cmd, SCM arg1, SCM rest)
{
  int argc  	       = 1 + scm_length (rest);
  char **argv 	       = alloca ((argc + 2) * sizeof (char *));
  /* 
   * conv_res is (roughly) a vector of the values returned by convert_for_Tk. 
   * It serves only to have pointers in the stack on the converted values. 
   * This permits to avoid GC problems (i.e. a GC between 1 and argc 
   * whereas convert_for_Tk has created new cells in a previous iteration 
   */
  SCM *conv_res	       = alloca ((argc + 1) * sizeof (SCM));
  struct Tk_command *W = SCM_TKCMD (cmd);

  /* First initialize an argv array */
  argv[0] = W->Id;
  argv[1] = STk_convert_for_Tk (arg1, &conv_res[1]);
  for (argc = 2; SCM_NNULLP(rest); argc++, rest=SCM_CDR(rest))
    argv[argc] = STk_convert_for_Tk (SCM_CAR(rest), &conv_res[argc]);
  argv[argc] = NULL;

  return apply_tkcmd (W, 1, argv);  
}

SCM
scm_make_tkcmd (char *name,
		Tcl_CmdProc *proc,
		ClientData clientData,
		Tcl_CmdDeleteProc *deleteProc)
{
  int namelen = strlen (name);
  SCM cmd = scm_make_struct (scm_class_tk_command,
			     SCM_MAKINUM ((namelen + 4) >> 2),
			     SCM_EOL);
  struct Tk_command *W = SCM_TKCMD (cmd);

  W->ptr = clientData;
  W->fct = proc;
  W->delproc = deleteProc;
  W->deldata = clientData;
  W->deleted = 0;
  W->Id = &W->Id_chars[0];
  strcpy (W->Id, name);

  /*fixme* intern */

  return cmd;
}

static char s_tk_command_printer[] = "tk-command-printer";
static SCM
tk_command_printer (obj, port)
     SCM obj;
     SCM port;
{
  /* This function can be made visible by means of struct-ref, so
     we need to make sure that it gets what it wants. */
  SCM_ASSERT (SCM_NIMP (obj) && SCM_TKCMDP (obj),
	      obj,
	      SCM_ARG1,
	      s_tk_command_printer);
  SCM_ASSERT (scm_valid_oport_value_p (port),
	      port,
	      SCM_ARG2,
	      s_tk_command_printer);
  port = SCM_COERCE_OPORT (port);
  scm_gen_puts (scm_regular_string, "#<tk-command ", port);
  scm_gen_puts (scm_regular_string, SCM_TKCMD (obj)->Id, port);
  scm_gen_putc ('>', port);
  return SCM_UNSPECIFIED;
}

void
scm_init_tkcmd ()
{
  SCM dispatch0, dispatch1, dispatch2, dispatch3;
  SCM s = scm_makfrom0str (SCM_TKCMD_LAYOUT);
  SCM l = scm_make_struct_layout (s);
  SCM p = scm_make_subr_opt (s_tk_command_printer,
			     scm_tc7_subr_2,
			     (SCM (*) ()) tk_command_printer,
			     0 /* Don't bind the name. */);
  SCM t = scm_make_struct (t, SCM_INUM0,
			   SCM_LIST4 (l, p, SCM_EOL, SCM_EOL));

  SCM_SET_CLASS_FLAGS (t, SCM_CLASSF_OPERATOR | SCM_CLASSF_TKCMD);
  dispatch0 = scm_make_subr_opt (s_tkcmd_dispatch0,
				 scm_tc7_subr_1,
				 (SCM (*) ()) tkcmd_dispatch0,
				 0);
  dispatch1 = scm_make_subr_opt (s_tkcmd_dispatch1,
				 scm_tc7_subr_2,
				 (SCM (*) ()) tkcmd_dispatch1,
				 0);
  dispatch2 = scm_make_subr_opt (s_tkcmd_dispatch2,
				 scm_tc7_subr_3,
				 (SCM (*) ()) tkcmd_dispatch2,
				 0);
  dispatch3 = scm_make_subr_opt (s_tkcmd_dispatch3,
				 scm_tc7_lsubr_2,
				 (SCM (*) ()) tkcmd_dispatch3,
				 0);
  SCM_OPERATOR_CLASS (t)->proc0 = dispatch0;
  SCM_OPERATOR_CLASS (t)->proc1 = dispatch1;
  SCM_OPERATOR_CLASS (t)->proc2 = dispatch2;
  SCM_OPERATOR_CLASS (t)->proc3 = dispatch3;
  scm_class_tk_command = scm_permanent_object (t);
  scm_sysintern ("<tk-command>", t);
#include "tkcmd.x"
}
