/* Debugging extensions for Guile
 * Copyright (C) 1995,1996,1997,1998,1999,2000,2001 Free Software Foundation
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
 * If you do not wish that, delete this exception notice.
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/stackchk.h"
#include "libguile/throw.h"
#include "libguile/macros.h"
#include "libguile/smob.h"
#include "libguile/procprop.h"
#include "libguile/srcprop.h"
#include "libguile/alist.h"
#include "libguile/continuations.h"
#include "libguile/strports.h"
#include "libguile/read.h"
#include "libguile/feature.h"
#include "libguile/dynwind.h"
#include "libguile/modules.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/fluids.h"

#include "libguile/validate.h"
#include "libguile/debug.h"


/* {Run time control of the debugging evaluator}
 */

SCM_DEFINE (scm_debug_options, "debug-options-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the debug options. Instead of using\n"
	    "this procedure directly, use the procedures @code{debug-enable},\n"
	    "@code{debug-disable}, @code{debug-set!} and @var{debug-options}.")
#define FUNC_NAME s_scm_debug_options
{
  SCM ans;
  SCM_DEFER_INTS;
  ans = scm_options (setting,
		     scm_debug_opts,
		     SCM_N_DEBUG_OPTIONS,
		     FUNC_NAME);
#ifndef SCM_RECKLESS
  if (!(1 <= SCM_N_FRAMES && SCM_N_FRAMES <= SCM_MAX_FRAME_SIZE))
    {
      scm_options (ans, scm_debug_opts, SCM_N_DEBUG_OPTIONS, FUNC_NAME);
      SCM_OUT_OF_RANGE (1, setting);
    }
#endif
  SCM_RESET_DEBUG_MODE;
  scm_stack_checking_enabled_p = SCM_STACK_CHECKING_P;
  scm_debug_eframe_size = 2 * SCM_N_FRAMES;
  SCM_ALLOW_INTS;
  return ans;
}
#undef FUNC_NAME

static void
with_traps_before (void *data)
{
  int *trap_flag = data;
  *trap_flag = SCM_TRAPS_P;
  SCM_TRAPS_P = 1;
}

static void
with_traps_after (void *data)
{
  int *trap_flag = data;
  SCM_TRAPS_P = *trap_flag;
}

static SCM
with_traps_inner (void *data)
{
  SCM thunk = SCM_PACK (data);
  return scm_apply (thunk, SCM_EOL, SCM_EOL);
}

SCM_DEFINE (scm_with_traps, "with-traps", 1, 0, 0, 
            (SCM thunk),
	    "Call @var{thunk} with traps enabled.")
#define FUNC_NAME s_scm_with_traps
{
  int trap_flag;
  SCM_VALIDATE_THUNK (1,thunk);
  return scm_internal_dynamic_wind (with_traps_before,
				    with_traps_inner,
				    with_traps_after,
				    (void *) SCM_UNPACK (thunk),
				    &trap_flag);
}
#undef FUNC_NAME



SCM_SYMBOL (scm_sym_procname, "procname");
SCM_SYMBOL (scm_sym_dots, "...");
SCM_SYMBOL (scm_sym_source, "source");

/* {Memoized Source}
 */

scm_bits_t scm_tc16_memoized;

static int
memoized_print (SCM obj, SCM port, scm_print_state *pstate)
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<memoized ", port);
  SCM_SET_WRITINGP (pstate, 1);
#ifdef GUILE_DEBUG
  scm_iprin1 (SCM_MEMOIZED_EXP (obj), port, pstate);
#else
  scm_iprin1 (scm_unmemoize (obj), port, pstate);
#endif
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return 1;
}

SCM_DEFINE (scm_memoized_p, "memoized?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is memoized.")
#define FUNC_NAME s_scm_memoized_p
{
  return SCM_BOOL(SCM_MEMOIZEDP (obj));
}
#undef FUNC_NAME

SCM
scm_make_memoized (SCM exp, SCM env)
{
  /* *fixme* Check that env is a valid environment. */
  register SCM z, ans;
  SCM_ENTER_A_SECTION;
  SCM_NEWSMOB (z, SCM_UNPACK (exp), SCM_UNPACK (env));
  SCM_NEWSMOB (ans, scm_tc16_memoized, SCM_UNPACK (z));
  SCM_EXIT_A_SECTION;
  return ans;
}

#ifdef GUILE_DEBUG
/*
 * Some primitives for construction of memoized code
 *
 * - procedure: memcons CAR CDR [ENV]
 *
 *     Construct a pair, encapsulated in a memoized object.
 *
 *     The CAR and CDR can be either normal or memoized.  If ENV isn't
 *     specified, the top-level environment of the current module will
 *     be assumed.  All environments must match.
 *
 * - procedure: make-gloc VARIABLE [ENV]
 *
 *     Return a gloc, encapsulated in a memoized object.
 *
 *     (Glocs can't exist in normal list structures, since they will
 *     be mistaken for structs.)
 *
 * - procedure: gloc? OBJECT
 *
 *     Return #t if OBJECT is a memoized gloc.
 *
 * - procedure: make-iloc FRAME BINDING CDRP
 *
 *     Return an iloc referring to frame no. FRAME, binding
 *     no. BINDING.  If CDRP is non-#f, the iloc is referring to a
 *     frame consisting of a single pair, with the value stored in the
 *     CDR.
 *
 * - procedure: iloc? OBJECT
 *
 *     Return #t if OBJECT is an iloc.
 *
 * - procedure: mem->proc MEMOIZED
 *
 *     Construct a closure from the memoized lambda expression MEMOIZED
 *
 *     WARNING! The code is not copied!
 *
 * - procedure: proc->mem CLOSURE
 *
 *     Turn the closure CLOSURE into a memoized object.
 *
 *     WARNING! The code is not copied!
 *
 * - constant: SCM_IM_AND
 * - constant: SCM_IM_BEGIN
 * - constant: SCM_IM_CASE
 * - constant: SCM_IM_COND
 * - constant: SCM_IM_DO
 * - constant: SCM_IM_IF
 * - constant: SCM_IM_LAMBDA
 * - constant: SCM_IM_LET
 * - constant: SCM_IM_LETSTAR
 * - constant: SCM_IM_LETREC
 * - constant: SCM_IM_OR
 * - constant: SCM_IM_QUOTE
 * - constant: SCM_IM_SET
 * - constant: SCM_IM_DEFINE
 * - constant: SCM_IM_APPLY
 * - constant: SCM_IM_CONT
 * - constant: SCM_IM_DISPATCH
 */

#include "libguile/variable.h"
#include "libguile/procs.h"

SCM_DEFINE (scm_make_gloc, "make-gloc", 1, 1, 0, 
            (SCM var, SCM env),
	    "Create a gloc for variable @var{var} in the environment\n"
	    "@var{env}.")
#define FUNC_NAME s_scm_make_gloc
{
  SCM_VALIDATE_VARIABLE (1,var);
  if (SCM_UNBNDP (env))
    env = scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE);
  else
    SCM_VALIDATE_NULLORCONS (2,env);
  return scm_make_memoized (SCM_UNPACK (var) + scm_tc3_cons_gloc, env);
}
#undef FUNC_NAME

SCM_DEFINE (scm_gloc_p, "gloc?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a gloc.")
#define FUNC_NAME s_scm_gloc_p
{
  return
    SCM_BOOL (SCM_MEMOIZEDP (obj) 
	      && ((SCM_UNPACK(SCM_MEMOIZED_EXP(obj))&7) == scm_tc3_cons_gloc));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_iloc, "make-iloc", 3, 0, 0,
            (SCM frame, SCM binding, SCM cdrp),
	    "Return a new iloc with frame offset @var{frame}, binding\n"
	    "offset @var{binding} and the cdr flag @var{cdrp}.")
#define FUNC_NAME s_scm_make_iloc
{
  SCM_VALIDATE_INUM (1,frame);
  SCM_VALIDATE_INUM (2,binding);
  return (SCM_ILOC00
	  + SCM_IFRINC * SCM_INUM (frame)
	  + (SCM_NFALSEP (cdrp) ? SCM_ICDR : 0)
	  + SCM_IDINC * SCM_INUM (binding));
}
#undef FUNC_NAME

SCM_DEFINE (scm_iloc_p, "iloc?", 1, 0, 0, 
          (SCM obj),
	    "Return @code{#t} if @var{obj} is an iloc.")
#define FUNC_NAME s_scm_iloc_p
{
  return SCM_BOOL(SCM_ILOCP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_memcons, "memcons", 2, 1, 0,
            (SCM car, SCM cdr, SCM env),
	    "Return a new memoized cons cell with @var{car} and @var{cdr}\n"
	    "as members and @var{env} as the environment.")
#define FUNC_NAME s_scm_memcons
{
  if (SCM_MEMOIZEDP (car))
    {
      /*fixme* environments may be two different but equal top-level envs */
      if (!SCM_UNBNDP (env) && SCM_MEMOIZED_ENV (car) != env)
	SCM_MISC_ERROR ("environment mismatch arg1 <-> arg3",
			scm_cons2 (car, env, SCM_EOL));
      else
	env = SCM_MEMOIZED_ENV (car);
      car = SCM_MEMOIZED_EXP (car);
    }
  if (SCM_MEMOIZEDP (cdr))
    {
      if (!SCM_UNBNDP (env) && SCM_MEMOIZED_ENV (cdr) != env)
	SCM_MISC_ERROR ("environment mismatch arg2 <-> arg3",
			scm_cons2 (cdr, env, SCM_EOL));
      else
	env = SCM_MEMOIZED_ENV (cdr);
      cdr = SCM_MEMOIZED_EXP (cdr);
    }
  if (SCM_UNBNDP (env))
    env = scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE);
  else
    SCM_VALIDATE_NULLORCONS (3,env);
  return scm_make_memoized (scm_cons (car, cdr), env);
}
#undef FUNC_NAME

SCM_DEFINE (scm_mem_to_proc, "mem->proc", 1, 0, 0, 
            (SCM obj),
	    "Convert a memoized object (which must be a lambda expression)\n"
	    "to a procedure.")
#define FUNC_NAME s_scm_mem_to_proc
{
  SCM env;
  SCM_VALIDATE_MEMOIZED (1,obj);
  env = SCM_MEMOIZED_ENV (obj);
  obj = SCM_MEMOIZED_EXP (obj);
  if (!(SCM_NIMP (obj) && SCM_CAR (obj) == SCM_IM_LAMBDA))
    SCM_MISC_ERROR ("expected lambda expression",
		    scm_cons (obj, SCM_EOL));
  return scm_closure (SCM_CDR (obj), env);
}
#undef FUNC_NAME

SCM_DEFINE (scm_proc_to_mem, "proc->mem", 1, 0, 0, 
            (SCM obj),
	    "Convert a procedure to a memoized object.")
#define FUNC_NAME s_scm_proc_to_mem
{
  SCM_VALIDATE_CLOSURE (1, obj);
  return scm_make_memoized (scm_cons (SCM_IM_LAMBDA, SCM_CODE (obj)),
			    SCM_ENV (obj));
}
#undef FUNC_NAME

#endif /* GUILE_DEBUG */

SCM_DEFINE (scm_unmemoize, "unmemoize", 1, 0, 0, 
            (SCM m),
	    "Unmemoize the memoized expression @var{m},")
#define FUNC_NAME s_scm_unmemoize
{
  SCM_VALIDATE_MEMOIZED (1,m);
  return scm_unmemocopy (SCM_MEMOIZED_EXP (m), SCM_MEMOIZED_ENV (m));
}
#undef FUNC_NAME

SCM_DEFINE (scm_memoized_environment, "memoized-environment", 1, 0, 0, 
            (SCM m),
	    "Return the environment of the memoized expression @var{m}.")
#define FUNC_NAME s_scm_memoized_environment
{
  SCM_VALIDATE_MEMOIZED (1,m);
  return SCM_MEMOIZED_ENV (m);
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_name, "procedure-name", 1, 0, 0, 
            (SCM proc),
	    "Return the name of the procedure @var{proc}")
#define FUNC_NAME s_scm_procedure_name
{
  SCM_VALIDATE_PROC (1,proc);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_subrs:
    return SCM_SNAME (proc);
  default:
    {
      SCM name = scm_procedure_property (proc, scm_sym_name);
#if 0
      /* Source property scm_sym_procname not implemented yet... */
      SCM name = scm_source_property (SCM_CAR (SCM_CDR (SCM_CODE (proc))), scm_sym_procname);
      if (SCM_FALSEP (name))
	name = scm_procedure_property (proc, scm_sym_name);
#endif
      if (SCM_FALSEP (name) && SCM_CLOSUREP (proc))
	name = scm_reverse_lookup (SCM_ENV (proc), proc);
      return name;
    }
  }
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_source, "procedure-source", 1, 0, 0, 
            (SCM proc),
	    "Return the source of the procedure @var{proc}.")
#define FUNC_NAME s_scm_procedure_source
{
  SCM_VALIDATE_NIM (1,proc);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_closures:
    {
      SCM formals = SCM_CLOSURE_FORMALS (proc);
      SCM src = scm_source_property (SCM_CDR (SCM_CODE (proc)), scm_sym_copy);
      if (!SCM_FALSEP (src))
	return scm_cons2 (scm_sym_lambda, formals, src);
      return scm_cons (scm_sym_lambda,
		       scm_unmemocopy (SCM_CODE (proc),
				       SCM_EXTEND_ENV (formals,
						       SCM_EOL,
						       SCM_ENV (proc))));
    }
  case scm_tcs_subrs:
#ifdef CCLO
  case scm_tc7_cclo:
#endif
    /* It would indeed be a nice thing if we supplied source even for
       built in procedures! */
    return scm_procedure_property (proc, scm_sym_source);
  default:
    SCM_WRONG_TYPE_ARG (1, proc);
    /* not reached */
  }
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_environment, "procedure-environment", 1, 0, 0, 
            (SCM proc),
	    "Return the environment of the procedure @var{proc}.")
#define FUNC_NAME s_scm_procedure_environment
{
  SCM_VALIDATE_NIM (1,proc);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_closures:
    return SCM_ENV (proc);
  case scm_tcs_subrs:
#ifdef CCLO
  case scm_tc7_cclo:
#endif
    return SCM_EOL;
  default:
    SCM_WRONG_TYPE_ARG (1, proc);
    /* not reached */
  }
}
#undef FUNC_NAME



/* Eval in a local environment.  We would like to have the ability to
 * evaluate in a specified local environment, but due to the
 * memoization this isn't normally possible.  We solve it by copying
 * the code before evaluating.  One solution would be to have eval.c
 * generate yet another evaluator.  They are not very big actually.
 */
SCM_DEFINE (scm_local_eval, "local-eval", 1, 1, 0,
            (SCM exp, SCM env),
	    "Evaluate @var{exp} in its environment.  If @var{env} is supplied,\n"
	    "it is the environment in which to evaluate @var{exp}.  Otherwise,\n"
	    "@var{exp} must be a memoized code object (in which case, its environment\n"
	    "is implicit).")
#define FUNC_NAME s_scm_local_eval
{
  if (SCM_UNBNDP (env))
  {
    SCM_VALIDATE_MEMOIZED (1, exp);
    return scm_i_eval_x (SCM_MEMOIZED_EXP (exp), SCM_MEMOIZED_ENV (exp));
  }
  return scm_i_eval (exp, env);
}
#undef FUNC_NAME

#if 0
SCM_REGISTER_PROC (s_reverse_lookup, "reverse-lookup", 2, 0, 0, scm_reverse_lookup);
#endif

SCM
scm_reverse_lookup (SCM env, SCM data)
{
  while (SCM_CONSP (env) && SCM_CONSP (SCM_CAR (env)))
    {
      SCM names = SCM_CAAR (env);
      SCM values = SCM_CDAR (env);
      while (SCM_CONSP (names))
	{
	  if (SCM_EQ_P (SCM_CAR (values), data))
	    return SCM_CAR (names);
	  names = SCM_CDR (names);
	  values = SCM_CDR (values);
	}
      if (!SCM_NULLP (names) && SCM_EQ_P (values, data))
	return names;
      env = SCM_CDR (env);
    }
  return SCM_BOOL_F;
}

SCM
scm_start_stack (SCM id, SCM exp, SCM env)
{
  SCM answer;
  scm_debug_frame vframe;
  scm_debug_info vframe_vect_body;
  vframe.prev = scm_last_debug_frame;
  vframe.status = SCM_VOIDFRAME;
  vframe.vect = &vframe_vect_body;
  vframe.vect[0].id = id;
  scm_last_debug_frame = &vframe;
  answer = scm_i_eval (exp, env);
  scm_last_debug_frame = vframe.prev;
  return answer;
}

SCM_SYNTAX(s_start_stack, "start-stack", scm_makacro, scm_m_start_stack);

static SCM
scm_m_start_stack (SCM exp, SCM env)
#define FUNC_NAME s_start_stack
{
  exp = SCM_CDR (exp);
  if (!SCM_ECONSP (exp) 
      || !SCM_ECONSP (SCM_CDR (exp))
      || !SCM_NULLP (SCM_CDDR (exp)))
    SCM_WRONG_NUM_ARGS ();
  return scm_start_stack (scm_eval_car (exp, env), SCM_CADR (exp), env);
}
#undef FUNC_NAME


/* {Debug Objects}
 *
 * The debugging evaluator throws these on frame traps.
 */

scm_bits_t scm_tc16_debugobj;

static int
debugobj_print (SCM obj, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<debug-object ", port);
  scm_intprint ((int) SCM_DEBUGOBJ_FRAME (obj), 16, port);
  scm_putc ('>', port);
  return 1;
}

SCM_DEFINE (scm_debug_object_p, "debug-object?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a debug object.")
#define FUNC_NAME s_scm_debug_object_p
{
  return SCM_BOOL(SCM_DEBUGOBJP (obj));
}
#undef FUNC_NAME


SCM
scm_make_debugobj (scm_debug_frame *frame)
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_ENTER_A_SECTION;
  SCM_SET_DEBUGOBJ_FRAME (z, frame);
  SCM_SET_CELL_TYPE (z, scm_tc16_debugobj);
  SCM_EXIT_A_SECTION;
  return z;
}



/* Undocumented debugging procedure */
#ifdef GUILE_DEBUG
SCM_DEFINE (scm_debug_hang, "debug-hang", 0, 1, 0, 
            (SCM obj),
	    "Go into an endless loop, which can be only terminated with\n"
	    "a debugger.")
#define FUNC_NAME s_scm_debug_hang
{
  int go = 0;
  while (!go) ;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif



void
scm_init_debug ()
{
  scm_init_opts (scm_debug_options, scm_debug_opts, SCM_N_DEBUG_OPTIONS);

  scm_tc16_memoized = scm_make_smob_type ("memoized", 0);
  scm_set_smob_mark (scm_tc16_memoized, scm_markcdr);
  scm_set_smob_print (scm_tc16_memoized, memoized_print);

  scm_tc16_debugobj = scm_make_smob_type ("debug-object", 0);
  scm_set_smob_print (scm_tc16_debugobj, debugobj_print);

#ifdef GUILE_DEBUG
  scm_define ("SCM_IM_AND", SCM_IM_AND);
  scm_define ("SCM_IM_BEGIN", SCM_IM_BEGIN);
  scm_define ("SCM_IM_CASE", SCM_IM_CASE);
  scm_define ("SCM_IM_COND", SCM_IM_COND);
  scm_define ("SCM_IM_DO", SCM_IM_DO);
  scm_define ("SCM_IM_IF", SCM_IM_IF);
  scm_define ("SCM_IM_LAMBDA", SCM_IM_LAMBDA);
  scm_define ("SCM_IM_LET", SCM_IM_LET);
  scm_define ("SCM_IM_LETSTAR", SCM_IM_LETSTAR);
  scm_define ("SCM_IM_LETREC", SCM_IM_LETREC);
  scm_define ("SCM_IM_OR", SCM_IM_OR);
  scm_define ("SCM_IM_QUOTE", SCM_IM_QUOTE);
  scm_define ("SCM_IM_SET_X", SCM_IM_SET_X);
  scm_define ("SCM_IM_DEFINE", SCM_IM_DEFINE);
  scm_define ("SCM_IM_APPLY", SCM_IM_APPLY);
  scm_define ("SCM_IM_CONT", SCM_IM_CONT);
  scm_define ("SCM_IM_DISPATCH", SCM_IM_DISPATCH);
#endif
  scm_add_feature ("debug-extensions");

#ifndef SCM_MAGIC_SNARFER
#include "libguile/debug.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
