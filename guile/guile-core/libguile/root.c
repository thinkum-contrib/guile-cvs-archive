/* Copyright (C) 1995,1996,1997,1998,1999,2000, 2001, 2002 Free Software Foundation, Inc.
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




#include <string.h>
#include <stdio.h>

#include "libguile/_scm.h"
#include "libguile/stackchk.h"
#include "libguile/dynwind.h"
#include "libguile/eval.h"
#include "libguile/smob.h"
#include "libguile/pairs.h"
#include "libguile/throw.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"

#include "libguile/root.h"


SCM scm_sys_protects[SCM_NUM_PROTECTS];



/* {call-with-dynamic-root}
 *
 * Suspending the current thread to evaluate a thunk on the
 * same C stack but under a new root.
 *
 * Calls to call-with-dynamic-root return exactly once (unless
 * the process is somehow exitted).  */

/* cwdr fills out both of these structures, and then passes a pointer
   to them through scm_internal_catch to the cwdr_body and
   cwdr_handler functions, to tell them how to behave and to get
   information back from them.

   A cwdr is a lot like a catch, except there is no tag (all
   exceptions are caught), and the body procedure takes the arguments
   passed to cwdr as A1 and ARGS.  The handler is also special since
   it is not directly run from scm_internal_catch.  It is executed
   outside the new dynamic root. */

struct cwdr_body_data {
  /* Arguments to pass to the cwdr body function.  */
  SCM a1, args;

  /* Scheme procedure to use as body of cwdr.  */
  SCM body_proc;
};

struct cwdr_handler_data {
  /* Do we need to run the handler? */
  int run_handler;

  /* The tag and args to pass it. */
  SCM tag, args;
};


/* Invoke the body of a cwdr, assuming that the throw handler has
   already been set up.  DATA points to a struct set up by cwdr that
   says what proc to call, and what args to apply it to.

   With a little thought, we could replace this with scm_body_thunk,
   but I don't want to mess with that at the moment.  */
static SCM
cwdr_body (void *data)
{
  struct cwdr_body_data *c = (struct cwdr_body_data *) data;

  return scm_apply (c->body_proc, c->a1, c->args);
}

/* Record the fact that the body of the cwdr has thrown.  Record
   enough information to invoke the handler later when the dynamic
   root has been deestablished.  */

static SCM
cwdr_handler (void *data, SCM tag, SCM args)
{
  struct cwdr_handler_data *c = (struct cwdr_handler_data *) data;

  c->run_handler = 1;
  c->tag = tag;
  c->args = args;
  return SCM_UNSPECIFIED;
}

SCM 
scm_internal_cwdr (scm_t_catch_body body, void *body_data,
		   scm_t_catch_handler handler, void *handler_data,
		   SCM_STACKITEM *stack_start)
{
  struct cwdr_handler_data my_handler_data;
  SCM answer, old_winds;

  /* Exit caller's dynamic state.
   */
  old_winds = scm_i_dynwinds ();
  scm_dowinds (SCM_EOL, scm_ilength (old_winds));

  scm_frame_begin (SCM_F_FRAME_REWINDABLE);
  scm_frame_current_dynamic_state (scm_make_dynamic_state (SCM_UNDEFINED));

  my_handler_data.run_handler = 0;
  answer = scm_i_with_continuation_barrier (body, body_data,
					    cwdr_handler, &my_handler_data);

  scm_frame_end ();

  /* Enter caller's dynamic state.
   */
  scm_dowinds (old_winds, - scm_ilength (old_winds));

  /* Now run the real handler iff the body did a throw. */
  if (my_handler_data.run_handler)
    return handler (handler_data, my_handler_data.tag, my_handler_data.args);
  else
    return answer;
}

/* The original CWDR for invoking Scheme code with a Scheme handler. */

static SCM 
cwdr (SCM proc, SCM a1, SCM args, SCM handler, SCM_STACKITEM *stack_start)
{
  struct cwdr_body_data c;
  
  c.a1 = a1;
  c.args = args;
  c.body_proc = proc;

  return scm_internal_cwdr (cwdr_body, &c,
			    scm_handle_by_proc, &handler,
			    stack_start);
}

SCM_DEFINE (scm_call_with_dynamic_root, "call-with-dynamic-root", 2, 0, 0,
           (SCM thunk, SCM handler),
	    "Evaluate @code{(thunk)} in a new dynamic context, returning its value.\n\n"
	    "If an error occurs during evaluation, apply @var{handler} to the\n"
	    "arguments to the throw, just as @code{throw} would.  If this happens,\n"
	    "@var{handler} is called outside the scope of the new root -- it is\n"
	    "called in the same dynamic context in which\n"
	    "@code{call-with-dynamic-root} was evaluated.\n\n"
	    "If @var{thunk} captures a continuation, the continuation is rooted at\n"
	    "the call to @var{thunk}.  In particular, the call to\n"
	    "@code{call-with-dynamic-root} is not captured.  Therefore,\n"
	    "@code{call-with-dynamic-root} always returns at most one time.\n\n"
	    "Before calling @var{thunk}, the dynamic-wind chain is un-wound back to\n"
	    "the root and a new chain started for @var{thunk}.  Therefore, this call\n"
	    "may not do what you expect:\n\n"
	    "@lisp\n"
	    ";; Almost certainly a bug:\n"
	    "(with-output-to-port\n"
	    " some-port\n\n"
	    " (lambda ()\n"
	    "   (call-with-dynamic-root\n"
	    "    (lambda ()\n"
	    "      (display 'fnord)\n"
	    "      (newline))\n"
	    "    (lambda (errcode) errcode))))\n"
	    "@end lisp\n\n"
	    "The problem is, on what port will @samp{fnord} be displayed?  You\n"
	    "might expect that because of the @code{with-output-to-port} that\n"
	    "it will be displayed on the port bound to @code{some-port}.  But it\n"
	    "probably won't -- before evaluating the thunk, dynamic winds are\n"
	    "unwound, including those created by @code{with-output-to-port}.\n"
	    "So, the standard output port will have been re-set to its default value\n"
	    "before @code{display} is evaluated.\n\n"
	    "(This function was added to Guile mostly to help calls to functions in C\n"
	    "libraries that can not tolerate non-local exits or calls that return\n"
	    "multiple times.  If such functions call back to the interpreter, it should\n"
	    "be under a new dynamic root.)")
#define FUNC_NAME s_scm_call_with_dynamic_root
{
  SCM_STACKITEM stack_place;
  return cwdr (thunk, SCM_EOL, SCM_EOL, handler, &stack_place);
}
#undef FUNC_NAME

SCM_DEFINE (scm_dynamic_root, "dynamic-root", 0, 0, 0, 
           (),
	    "Return an object representing the current dynamic root.\n\n"
	    "These objects are only useful for comparison using @code{eq?}.\n")
#define FUNC_NAME s_scm_dynamic_root
{
  return SCM_I_CURRENT_THREAD->continuation_root;
}
#undef FUNC_NAME

SCM
scm_apply_with_dynamic_root (SCM proc, SCM a1, SCM args, SCM handler)
{
  SCM_STACKITEM stack_place;
  return cwdr (proc, a1, args, handler, &stack_place);
}



void
scm_init_root ()
{
#include "libguile/root.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
