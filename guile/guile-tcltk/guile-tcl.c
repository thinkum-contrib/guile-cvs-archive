/*	Copyright (C) 1998, 2000, 2001, 2002 Free Software Foundation, Inc.
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
#include <malloc.h>
#include <tcl.h>
#include <libguile.h>
#include "compat.h"

#include "guile-tcl.h"



#ifdef USE_THREADS
scm_t_mutex scm_tcl_mutex;
scm_t_cond scm_tcl_condvar;
int scm_tcl_handle_event_p;
#endif

static size_t free_interp (SCM obj);
static size_t
free_interp (obj)
     SCM obj;
{
  SCM_PROPS (obj) = SCM_EOL;
  Tcl_DeleteInterp (SCM_TERP (obj));
  free (SCM_GTCLTK (obj));
  return sizeof (struct gtcltk_interp);
}

static SCM mark_interp (SCM obj);
static SCM
mark_interp (obj)
     SCM obj;
{
  return SCM_PROPS (obj); 
}

int scm_tc16_tcl_interp;



SCM_PROC(s_tcl_create_interp, "tcl-create-interp", 0, 0, 0, scm_tcl_create_interp);

SCM scm_tcl_create_interp (void);
SCM
scm_tcl_create_interp ()
{
  SCM answer;
  struct gtcltk_interp *gtcltk;

#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;
  gtcltk = (struct gtcltk_interp *) malloc (sizeof (*gtcltk));
  if (! gtcltk)
    {
      SCM_ALLOW_INTS;
      scm_memory_error (s_tcl_create_interp);
    }
  SCM_NEWCELL (answer);
  SCM_SETCAR (answer, scm_tc16_tcl_interp);
  SCM_SETCDR (answer, (SCM) gtcltk);
  SCM_TERP (answer) = Tcl_CreateInterp ();
  SCM_PROPS (answer) = SCM_EOL;
  SCM_ALLOW_INTS;
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  return answer;
}


SCM_PROC(s_tcl_global_eval, "tcl-global-eval", 2, 0, 0, scm_tcl_global_eval);
SCM scm_tcl_global_eval (SCM tobj, SCM script);
SCM
scm_tcl_global_eval (tobj, script)
     SCM tobj;
     SCM script;
{
  int status;

  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_global_eval);
  SCM_ASSERT (SCM_NIMP (script) && SCM_ROSTRINGP (script), script, SCM_ARG2,
	      s_tcl_global_eval);
  
  SCM_STRING_COERCE_0TERMINATION_X (script);

#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;
  status = Tcl_GlobalEval (SCM_TERP (tobj), SCM_ROCHARS (script));
  SCM_ALLOW_INTS;
  
  {
    SCM answer;
    answer = scm_cons (SCM_MAKINUM (status),
		       scm_makfrom0str (SCM_TERP (tobj)->result));
    SCM_DEFER_INTS;
    Tcl_FreeResult (SCM_TERP (tobj));
    SCM_ALLOW_INTS;
#ifdef USE_THREADS
    scm_mutex_unlock (&scm_tcl_mutex);
    if (TclIdlePending ())
      {
	scm_tcl_handle_event_p = 1;
	scm_cond_signal (&scm_tcl_condvar);
      }
#endif
    return answer;
  }
}



static SCM listify_strings (int argc, char * argv[]);
static SCM
listify_strings (argc, argv)
     int argc;
     char * argv[];
{
  SCM answer;

  answer = SCM_EOL;
  while (argc--)
    {
      answer = scm_cons (scm_makfrom0str (argv[argc]), answer);
    }
  return answer;
  
}

/* defining Tcl commands that call Scheme code

   The function tcl-create-command below lets you export a Scheme
   procedure to Tcl.  (tcl-create-command INTERP NAME CLOSURE) defines a
   new command in the Tcl interpreter INTERP named NAME, which invokes
   the Scheme procedure CLOSURE.

   The new Tcl command uses invoke_tcl_command as its 'proc', and a
   pair of the form (CLOSURE INTERP) as its ClientData, so we can find
   CLOSURE when the Tcl command is invoked.  We also add this same
   pair to INTERP's property list, to protect CLOSURE from garbage
   collection.

   When the command is deleted from the Tcl level, delete_tcl_command
   takes care of removing the (CLOSURE INTERP) pair from the
   interpreter, so the closure can be GC'd.  */

static int invoke_tcl_command (ClientData data, 
			       Tcl_Interp *interp,
			       int argc, char *argv[]);

static void
mask_signals (void *mask)
{
  int *m = (int *) mask;
  *m = scm_mask_ints;
  scm_mask_ints = 1;
}

static void
revert_signals (void *mask)
{
  int *m = (int *) mask;
  scm_mask_ints = *m;
}

static SCM
inner_masked_apply (void *args)
{
  SCM a = (SCM) args;
  return scm_apply (SCM_CAR (a), SCM_CDR (a), SCM_EOL);
}

static SCM
masked_apply (SCM proc, SCM args)
{
  int old_mask;
  return scm_internal_dynamic_wind (mask_signals,
				    inner_masked_apply,
				    revert_signals,
				    (void *) scm_cons (proc, args),
				    &old_mask);
}

static int
invoke_tcl_command (data, interp, argc, argv)
     ClientData data;
     Tcl_Interp * interp;
     int argc;
     char * argv[];
{
  SCM proc;
  SCM result;

  proc = SCM_CAR ((SCM)data);

  /* proc had better not longjmp past us -- see:
     with-tcl-error-handling in gtcltk/tcl.SCM */
  SCM_ALLOW_INTS;
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  result = masked_apply (proc, listify_strings (argc - 1, argv + 1));
#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;

  if (SCM_NIMP (result) && SCM_ROSTRINGP (result))
    {
      Tcl_SetResult (interp, SCM_ROCHARS (result), TCL_VOLATILE);
      scm_return_first (result);
      return TCL_OK;
    }
  else if (SCM_NUMBERP (result))
    {
      SCM name;
      name = scm_number_to_string (result, SCM_MAKINUM (10));
      Tcl_SetResult (interp, SCM_STRING_CHARS (SCM_CDR (name)), TCL_VOLATILE);
      return TCL_OK;
    }
  else if (SCM_NIMP (result)
	   && SCM_CONSP (result)
	   && SCM_INUMP (SCM_CAR (result))
	   && SCM_NIMP (SCM_CDR (result))
	   && SCM_ROSTRINGP (SCM_CDR (result)))
    {
      Tcl_SetResult (interp, SCM_ROCHARS (SCM_CDR (result)), TCL_VOLATILE);
      return SCM_INUM (SCM_CAR (result));
    }
  else
    {
      Tcl_SetResult (interp, "Strange Scheme result", TCL_STATIC);
      return TCL_ERROR;
    }
}

static void delete_tcl_command (ClientData data);
static void
delete_tcl_command (data)
     ClientData data;
{
  SCM obj = (SCM) data;

  assert (SCM_NIMP (obj)
	  && SCM_CONSP (obj)
	  && SCM_TERPP (SCM_CDR (obj)));

  {
    SCM terp = SCM_CDR (obj);
    SCM_PROPS (terp) = scm_delq_x (obj, SCM_PROPS (terp));
  }
}


SCM_PROC(s_tcl_create_command, "tcl-create-command", 3, 0, 0, scm_tcl_create_command);
SCM scm_tcl_create_command (SCM tobj, SCM name, SCM proc);
SCM
scm_tcl_create_command (tobj, name, proc)
     SCM tobj;
     SCM name;
     SCM proc;
{
  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_create_command);
  SCM_ASSERT (SCM_NIMP (name) && SCM_ROSTRINGP (name), name, SCM_ARG2, 
	      s_tcl_create_command);
  SCM_ASSERT (scm_procedure_p (proc) == SCM_BOOL_T, proc, SCM_ARG3, 
	      s_tcl_create_command);
  SCM_PROPS (tobj) = scm_acons (proc, tobj, SCM_PROPS (tobj));
  SCM_STRING_COERCE_0TERMINATION_X (name);
#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;
  Tcl_CreateCommand (SCM_TERP (tobj), SCM_ROCHARS (name),
		     invoke_tcl_command,
		     (ClientData)SCM_CAR (SCM_PROPS (tobj)),
		     delete_tcl_command);
  SCM_ALLOW_INTS;
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_tcl_delete_command, "tcl-delete-command", 2, 0, 0, scm_tcl_delete_command);
SCM scm_tcl_delete_command (SCM tobj, SCM name);
SCM
scm_tcl_delete_command (tobj, name)
     SCM tobj;
     SCM name;
{
  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_delete_command);
  SCM_ASSERT (SCM_NIMP (name) && SCM_ROSTRINGP(name), name, SCM_ARG2,
	      s_tcl_delete_command);
  SCM_STRING_COERCE_0TERMINATION_X (name);
#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;
  Tcl_DeleteCommand (SCM_TERP (tobj), SCM_ROCHARS (name));
  SCM_ALLOW_INTS;
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  return SCM_UNSPECIFIED;
}



SCM_PROC(s_tcl_get_int, "tcl-get-int", 2, 0, 0, scm_tcl_get_int);
SCM scm_tcl_get_int (SCM tobj, SCM name);
SCM
scm_tcl_get_int (tobj, name)
     SCM tobj;
     SCM name;
{
  int stat;
  int c_answer;
  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_get_int);
  SCM_ASSERT (SCM_NIMP (name)
	  && SCM_ROSTRINGP (name),
	  name, SCM_ARG2, s_tcl_get_int);

  SCM_STRING_COERCE_0TERMINATION_X (name);

#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;
  stat = Tcl_GetInt (SCM_TERP (tobj), SCM_ROCHARS (name), &c_answer);
  Tcl_FreeResult (SCM_TERP (tobj));
  SCM_ALLOW_INTS;
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  SCM_ASSERT (stat == TCL_OK, name, SCM_TERP (tobj)->result, s_tcl_get_int);
  return scm_long2num ((long)c_answer);
}

SCM_PROC(s_tcl_get_double, "tcl-get-double", 2, 0, 0, scm_tcl_get_double);
SCM scm_tcl_get_double (SCM tobj, SCM name);
SCM
scm_tcl_get_double (tobj, name)
     SCM tobj;
     SCM name;
{
  int stat;
  double c_answer;
  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_get_double);
  SCM_ASSERT (SCM_NIMP (name)
	  && SCM_STRINGP (name),
	  name, SCM_ARG2, s_tcl_get_double);
  SCM_STRING_COERCE_0TERMINATION_X (name);
#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;
  stat = Tcl_GetDouble (SCM_TERP (tobj), SCM_ROCHARS (name), &c_answer);
  Tcl_FreeResult (SCM_TERP (tobj));
  SCM_ALLOW_INTS;
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  SCM_ASSERT (stat == TCL_OK, name, SCM_TERP (tobj)->result, s_tcl_get_double);
  return scm_make_real (c_answer);
}


SCM_PROC(s_tcl_get_boolean, "tcl-get-boolean", 2, 0, 0, scm_tcl_get_boolean);
SCM scm_tcl_get_boolean (SCM tobj, SCM name);
SCM
scm_tcl_get_boolean (tobj, name)
     SCM tobj;
     SCM name;
{
  int stat;
  int c_answer;
  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_get_boolean);
  SCM_ASSERT (SCM_NIMP (name)
	  && SCM_ROSTRINGP (name),
	  name, SCM_ARG2, s_tcl_get_boolean);
  SCM_STRING_COERCE_0TERMINATION_X (name);
#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;
  stat = Tcl_GetBoolean (SCM_TERP (tobj), SCM_ROCHARS (name), &c_answer);
  Tcl_FreeResult (SCM_TERP (tobj));
  SCM_ALLOW_INTS;
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  SCM_ASSERT (stat == TCL_OK, tobj, SCM_TERP (tobj)->result, 
	      s_tcl_get_boolean);
  return (c_answer ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM_PROC(s_tcl_split_list, "tcl-split-list", 2, 0, 0, scm_tcl_split_list);
SCM scm_tcl_split_list (SCM tobj, SCM name);
SCM
scm_tcl_split_list (tobj, name)
     SCM tobj;
     SCM name;
{
  char **argv;
  int argc;
  int tcl_result;

  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_split_list);
  SCM_ASSERT (SCM_NIMP (name)
	  && SCM_ROSTRINGP (name),
	  name, SCM_ARG2, s_tcl_split_list);
#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;
  tcl_result = (TCL_OK == Tcl_SplitList (SCM_TERP (tobj),
					 SCM_ROCHARS (name), &argc, &argv));
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  if (!tcl_result)
    {
      SCM_ALLOW_INTS;
      SCM_ASSERT (tcl_result, name, SCM_TERP (tobj)->result, s_tcl_split_list);
    }
  {
    SCM answer;
    answer = listify_strings (argc, argv);
    free (argv);
    SCM_ALLOW_INTS;
    return answer;
  }
}

SCM_PROC(s_tcl_merge, "tcl-merge", 2, 0, 0, scm_tcl_merge);
SCM scm_tcl_merge (SCM tobj, SCM args);
SCM
scm_tcl_merge (tobj, args)
     SCM tobj;
     SCM args;
{
  SCM orig_args = args;
  int argc;
  char ** argv;

  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_merge);
  argc = scm_ilength (args);
  SCM_ASSERT (argc >= 0, args, SCM_ARG2, s_tcl_merge);

  if (argc == 0)
    argv = 0;
  else
    {
      int i;
      SCM_DEFER_INTS;
      orig_args = args;
      argv = (char **)malloc (sizeof (char *) * argc);
      if (!argv)
	{
	  SCM_ALLOW_INTS;
	  scm_memory_error (s_tcl_merge);
	}
      for (i = 0; i < argc; ++i)
	{
	  if (!(SCM_NIMP (SCM_CAR (args)) && SCM_ROSTRINGP (SCM_CAR (args))))
	    {
	      SCM_ALLOW_INTS;
	      SCM_ASSERT (0, SCM_CAR (args), "all arguments must be strings",
			  s_tcl_merge);
	    }
	  if (SCM_SUBSTRP (SCM_CAR (args)))
	    SCM_SETCAR (args, scm_mem2string (SCM_ROCHARS (SCM_CAR (args)),
					      SCM_ROLENGTH (SCM_CAR (args))));
	  argv[i] = SCM_ROCHARS (SCM_CAR (args));
	  args = SCM_CDR (args);
	}
    }
  {
    char * c_answer;
    SCM answer;
#ifdef USE_THREADS
    scm_mutex_lock (&scm_tcl_mutex);
#endif
    c_answer = Tcl_Merge (argc, argv);
#ifdef USE_THREADS
    scm_mutex_unlock (&scm_tcl_mutex);
#endif
    answer = scm_makfrom0str (c_answer);
    free (c_answer);
    if (argv) free (argv);
    SCM_ALLOW_INTS;
    return scm_return_first (answer, orig_args);
  }
}

#if 0

char *trace_variable SCM_P ((ClientData data,
			    Tcl_Interp * interp,
			    char * name, char * name2,
			    int flags));
char *
trace_variable (data, interp, name, name2, flags)
     ClientData data;
     Tcl_Interp * interp;
     char * name;
     char * name2;
     int flags;
{
  SCM proc;
  SCM result;

  proc = (SCM)SCM_CAR (data);
  SCM_ALLOW_INTS;
  result = masked_apply (proc,
			 scm_listify (SCM_SELF_INTERP (interp),
				      scm_makfrom0str (name),
				      scm_makfrom0str_opt (name2),
				      SCM_MAKINUM (flags),
				      SCM_UNDEFINED));
  SCM_DEFER_INTS;
  return ((result == SCM_BOOL_F)
	  ? "Error from Scheme variable trace."
	  : 0);
}

SCM_PROC(s_tcl_trace_var2, "tcl-trace-var2", 5, 0, 0, scm_tcl_trace_var2);
SCM scm_tcl_trace_var2 SCM_P ((SCM tobj, SCM name, SCM index, SCM flags,
			      SCM thunk));
SCM
scm_tcl_trace_var2 (tobj, name, index, flags, thunk)
     SCM tobj;
     SCM name;
     SCM index;
     SCM flags;
     SCM thunk;
{
  int stat;
  SCM result;
  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_trace_var2);
  SCM_ASSERT (SCM_NIMP (name)
	  && SCM_ROSTRINGP (name),
	  name, SCM_ARG2, s_tcl_trace_var2);
  SCM_STRING_COERCE_0TERMINATION_X (name);
  SCM_ASSERT ((SCM_BOOL_F == index)
	  || (SCM_NIMP (index)
	      && SCM_ROSTRINGP (index)),
	  index, SCM_ARG3, s_tcl_trace_var2);
  if (SCM_NIMP (index))
    SCM_STRING_COERCE_0TERMINATION_X (index);
  SCM_ASSERT (SCM_INUMP (flags), flags, SCM_ARG4, s_tcl_trace_var2);
  SCM_ASSERT (scm_procedure_p (thunk), thunk, SCM_ARG5, s_tcl_trace_var2);
  SCM_PROPS (tobj) = scm_acons (thunk, SCM_EOL, SCM_PROPS (tobj));
  SCM_DEFER_INTS;
  stat = Tcl_TraceVar2 (SCM_TERP (tobj),
			SCM_ROCHARS (name),
			((index == SCM_BOOL_F)
			 ? 0
			 : SCM_ROCHARS (index)),
			SCM_INUM (flags),
			trace_variable,
			(ClientData)SCM_CAR (SCM_PROPS (tobj)));
  if (!stat)
    {
      SCM result;
      result = scm_makfrom0str (SCM_TERP (tobj)->result);
    }
  else
    result = SCM_BOOL_T;
  Tcl_FreeResult (SCM_TERP (tobj));
  SCM_ALLOW_INTS;
  return result;
}



SCM_PROC(s_tcl_untrace_var2, "tcl-untrace-var2", 5, 0, 0, scm_tcl_untrace_var2);
SCM scm_tcl_untrace_var2 SCM_P ((SCM tobj, SCM name, SCM index, SCM flags,
				SCM thunk));
SCM
scm_tcl_untrace_var2 (tobj, name, index, flags, thunk)
     SCM tobj;
     SCM name;
     SCM index;
     SCM flags;
     SCM thunk;
{
  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_untrace_var2);
  SCM_ASSERT ((SCM_NIMP (name) && SCM_ROSTRINGP (name)),
	  name, SCM_ARG2, s_tcl_untrace_var2);
  SCM_STRING_COERCE_0TERMINATION_X (name);
  SCM_ASSERT ((SCM_BOOL_F == index)
	  || (SCM_NIMP (index)
	      && SCM_ROSTRINGP (index)),
	  index, SCM_ARG3, s_tcl_untrace_var2);
  if (SCM_NIMP (index))
    SCM_STRING_COERCE_0TERMINATION_X (index);
  SCM_ASSERT (SCM_INUMP (flags), flags, SCM_ARG4, s_tcl_untrace_var2);
  SCM_ASSERT (scm_procedure_p (thunk), thunk, SCM_ARG5, s_tcl_untrace_var2);

  {
    SCM pos;
    pos = SCM_PROPS (tobj);
    while (pos != SCM_BOOL_F)
      {
	if (SCM_CAR (SCM_CAR (pos)) == thunk)
	  {
	    int got_it;
	    SCM_DEFER_INTS;
	    got_it = Tcl_UntraceVar2 (SCM_TERP (tobj),
				      SCM_ROCHARS (name),
				      ((SCM_BOOL_F == index)
				       ? 0
				       : SCM_ROCHARS (index)),
				      flags,
				      trace_variable,
				      (ClientData)SCM_CAR (pos));
	    if (got_it)
	      {
		SCM_PROPS (tobj) = scm_delq_x (SCM_CAR (pos),
					       SCM_PROPS (tobj));
		Tcl_FreeResult (SCM_TERP (tobj));
		SCM_ALLOW_INTS;
		return SCM_BOOL_T;
	      }
	    SCM_ALLOW_INTS;
	  }
	pos = SCM_CDR (pos);
      }
    return SCM_BOOL_F;
  }
}

#endif


SCM_PROC(s_tcl_set_var2, "tcl-set-var2", 5, 0, 0, scm_tcl_set_var2);
SCM scm_tcl_set_var2 (SCM tobj, SCM name, SCM index, SCM value, SCM flags);
SCM
scm_tcl_set_var2 (tobj, name, index, value, flags)
     SCM tobj;
     SCM name;
     SCM index;
     SCM value;
     SCM flags;
{
  char * c_answer;
  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_set_var2);
  SCM_ASSERT ((SCM_NIMP (name) && SCM_ROSTRINGP (name)),
	  name, SCM_ARG2, s_tcl_set_var2);
  SCM_STRING_COERCE_0TERMINATION_X (name);
  SCM_ASSERT ((SCM_BOOL_F == index)
	  || (SCM_NIMP (index)
	      && SCM_ROSTRINGP (index)),
	  index, SCM_ARG3, s_tcl_set_var2);
  if (SCM_NIMP (index))
    SCM_STRING_COERCE_0TERMINATION_X (index);
  SCM_ASSERT (SCM_NIMP (value) && SCM_ROSTRINGP (value),
	  value, SCM_ARG4, s_tcl_set_var2);
  SCM_STRING_COERCE_0TERMINATION_X (value);
  SCM_ASSERT (SCM_INUMP (flags), flags, SCM_ARG5, s_tcl_set_var2);

  SCM_DEFER_INTS;
  c_answer = Tcl_SetVar2 (SCM_TERP (tobj),
			  SCM_ROCHARS (name),
			  ((index == SCM_BOOL_F) ? 0 : SCM_ROCHARS (index)),
			  SCM_ROCHARS (value),
			  SCM_INUM (flags));
  SCM_ALLOW_INTS;
  return scm_makfrom0str_opt (c_answer);
}


SCM_PROC(s_tcl_get_var2, "tcl-get-var2", 4, 0, 0, scm_tcl_get_var2);
SCM scm_tcl_get_var2 (SCM tobj, SCM name, SCM index, SCM flags);
SCM
scm_tcl_get_var2 (tobj, name, index, flags)
     SCM tobj;
     SCM name;
     SCM index;
     SCM flags;
{
  char * c_answer;
  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1,
	      s_tcl_get_var2);
  SCM_ASSERT ((SCM_NIMP (name) && SCM_ROSTRINGP (name)),
	  name, SCM_ARG2, s_tcl_set_var2);
  SCM_STRING_COERCE_0TERMINATION_X (name);
  SCM_ASSERT ((SCM_BOOL_F == index)
	  || (SCM_NIMP (index)
	      && SCM_ROSTRINGP (index)),
	  index, SCM_ARG3, s_tcl_set_var2);
  if (SCM_NIMP (index))
    SCM_STRING_COERCE_0TERMINATION_X (index);
  SCM_ASSERT (SCM_INUMP (flags), flags, SCM_ARG4, s_tcl_get_var2);

  SCM_DEFER_INTS;
  c_answer = Tcl_GetVar2 (SCM_TERP (tobj),
			  SCM_ROCHARS (name),
			  ((index == SCM_BOOL_F) ? 0 : SCM_ROCHARS (index)),
			  SCM_INUM (flags));
  SCM_ALLOW_INTS;
  return scm_makfrom0str_opt (c_answer);
}


SCM_PROC (s_tcl_defined_p, "tcl-defined?", 2, 0, 0, scm_tcl_defined_p);
SCM scm_tcl_defined_p (SCM tobj, SCM name);
SCM
scm_tcl_defined_p (tobj, name)
     SCM tobj;
     SCM name;
{
  Tcl_CmdInfo info;
  int status;

  SCM_ASSERT (SCM_NIMP (tobj) && SCM_TERPP (tobj), tobj, SCM_ARG1, s_tcl_defined_p);
#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;
  status = Tcl_GetCommandInfo (SCM_TERP (tobj), SCM_ROCHARS (name), &info);
  SCM_ALLOW_INTS;
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  
  return status ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_PROC(s_tcl_do_one_event, "tcl-do-one-event", 1, 0, 0, scm_tcl_do_one_event);
SCM scm_tcl_do_one_event (SCM flags);
SCM
scm_tcl_do_one_event (flags)
     SCM flags;
{
  int answer;
  SCM_ASSERT (SCM_INUMP (flags), flags, SCM_ARG1, s_tcl_do_one_event);
#ifdef USE_THREADS
  scm_mutex_lock (&scm_tcl_mutex);
#endif
  SCM_DEFER_INTS;
  answer = (Tcl_DoOneEvent (SCM_INUM (flags)));
  SCM_ALLOW_INTS;
#ifdef USE_THREADS
  scm_mutex_unlock (&scm_tcl_mutex);
#endif
  return SCM_MAKINUM (answer);
}





void scm_init_gtcl (void);
void
scm_init_gtcl ()
{
  scm_tc16_tcl_interp = scm_make_smob_type ("tcl-interpreter", 0);
  scm_set_smob_mark (scm_tc16_tcl_interp, mark_interp);
  scm_set_smob_free (scm_tc16_tcl_interp, free_interp);
#ifdef USE_THREADS
#ifdef SCM_MUTEX_INIT_TWO_ARGS
  scm_mutex_init (&scm_tcl_mutex, NULL);
#else
  scm_mutex_init (&scm_tcl_mutex);
#endif
  scm_cond_init (&scm_tcl_condvar, NULL);
#endif
#include "guile-tcl.x"
}
