/* This file contains definitions for deprecated features.  When you
   deprecate something, move it here when that is feasible.
*/

/* Copyright (C) 2003 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/deprecated.h"
#include "libguile/deprecation.h"
#include "libguile/snarf.h"
#include "libguile/validate.h"
#include "libguile/strings.h"
#include "libguile/strop.h"
#include "libguile/modules.h"
#include "libguile/eval.h"
#include "libguile/smob.h"
#include "libguile/procprop.h"
#include "libguile/vectors.h"
#include "libguile/hashtab.h"
#include "libguile/struct.h"
#include "libguile/variable.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"
#include "libguile/eq.h"
#include "libguile/read.h"
#include "libguile/strports.h"
#include "libguile/smob.h"
#include "libguile/alist.h"

#include <stdio.h>
#include <string.h>

#if (SCM_ENABLE_DEPRECATED == 1)

/* From eval.c: Error messages of the evaluator.  These were deprecated in
 * guile 1.7.0 on 2003-06-02.  */
const char scm_s_expression[] = "missing or extra expression";
const char scm_s_test[] = "bad test";
const char scm_s_body[] = "bad body";
const char scm_s_bindings[] = "bad bindings";
const char scm_s_variable[] = "bad variable";
const char scm_s_clauses[] = "bad or missing clauses";
const char scm_s_formals[] = "bad formals";


SCM_REGISTER_PROC(s_substring_move_left_x, "substring-move-left!", 5, 0, 0, scm_substring_move_x);

SCM_REGISTER_PROC(s_substring_move_right_x, "substring-move-right!", 5, 0, 0, scm_substring_move_x);

SCM
scm_wta (SCM arg, const char *pos, const char *s_subr)
{
  if (!s_subr || !*s_subr)
    s_subr = NULL;
  if ((~0x1fL) & (long) pos)
    {
      /* error string supplied.  */
      scm_misc_error (s_subr, pos, scm_list_1 (arg));
    }
  else
    {
      /* numerical error code.  */
      scm_t_bits error = (scm_t_bits) pos;

      switch (error)
	{
	case SCM_ARGn:
	  scm_wrong_type_arg (s_subr, 0, arg);
	case SCM_ARG1:
	  scm_wrong_type_arg (s_subr, 1, arg);
	case SCM_ARG2:
	  scm_wrong_type_arg (s_subr, 2, arg);
	case SCM_ARG3:
	  scm_wrong_type_arg (s_subr, 3, arg);
	case SCM_ARG4:
	  scm_wrong_type_arg (s_subr, 4, arg);
	case SCM_ARG5:
	  scm_wrong_type_arg (s_subr, 5, arg);
	case SCM_ARG6:
	  scm_wrong_type_arg (s_subr, 6, arg);
	case SCM_ARG7:
	  scm_wrong_type_arg (s_subr, 7, arg);
	case SCM_WNA:
	  scm_wrong_num_args (arg);
	case SCM_OUTOFRANGE:
	  scm_out_of_range (s_subr, arg);
	case SCM_NALLOC:
	  scm_memory_error (s_subr);
	default:
	  /* this shouldn't happen.  */
	  scm_misc_error (s_subr, "Unknown error", SCM_EOL);
	}
    }
  return SCM_UNSPECIFIED;
}

/* Module registry
 */

/* We can't use SCM objects here. One should be able to call
   SCM_REGISTER_MODULE from a C++ constructor for a static
   object. This happens before main and thus before libguile is
   initialized. */

struct moddata {
  struct moddata *link;
  char *module_name;
  void *init_func;
};

static struct moddata *registered_mods = NULL;

void
scm_register_module_xxx (char *module_name, void *init_func)
{
  struct moddata *md;

  scm_c_issue_deprecation_warning 
    ("`scm_register_module_xxx' is deprecated.  Use extensions instead.");

  /* XXX - should we (and can we) DEFER_INTS here? */

  for (md = registered_mods; md; md = md->link)
    if (!strcmp (md->module_name, module_name))
      {
	md->init_func = init_func;
	return;
      }

  md = (struct moddata *) malloc (sizeof (struct moddata));
  if (md == NULL)
    {
      fprintf (stderr,
	       "guile: can't register module (%s): not enough memory",
	       module_name);
      return;
    }

  md->module_name = module_name;
  md->init_func = init_func;
  md->link = registered_mods;
  registered_mods = md;
}

SCM_DEFINE (scm_registered_modules, "c-registered-modules", 0, 0, 0, 
            (),
	    "Return a list of the object code modules that have been imported into\n"
	    "the current Guile process.  Each element of the list is a pair whose\n"
	    "car is the name of the module, and whose cdr is the function handle\n"
	    "for that module's initializer function.  The name is the string that\n"
	    "has been passed to scm_register_module_xxx.")
#define FUNC_NAME s_scm_registered_modules
{
  SCM res;
  struct moddata *md;

  res = SCM_EOL;
  for (md = registered_mods; md; md = md->link)
    res = scm_cons (scm_cons (scm_makfrom0str (md->module_name),
			      scm_ulong2num ((unsigned long) md->init_func)),
		    res);
  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_clear_registered_modules, "c-clear-registered-modules", 0, 0, 0, 
            (),
	    "Destroy the list of modules registered with the current Guile process.\n"
	    "The return value is unspecified.  @strong{Warning:} this function does\n"
	    "not actually unlink or deallocate these modules, but only destroys the\n"
	    "records of which modules have been loaded.  It should therefore be used\n"
	    "only by module bookkeeping operations.")
#define FUNC_NAME s_scm_clear_registered_modules
{
  struct moddata *md1, *md2;

  SCM_DEFER_INTS;

  for (md1 = registered_mods; md1; md1 = md2)
    {
      md2 = md1->link;
      free (md1);
    }
  registered_mods = NULL;

  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_remember (SCM *ptr)
{
  scm_c_issue_deprecation_warning ("`scm_remember' is deprecated. "
                                   "Use the `scm_remember_upto_here*' family of functions instead.");
}

SCM
scm_protect_object (SCM obj)
{
  scm_c_issue_deprecation_warning ("`scm_protect_object' is deprecated. "
                                   "Use `scm_gc_protect_object' instead.");
  return scm_gc_protect_object (obj);
}

SCM
scm_unprotect_object (SCM obj)
{
  scm_c_issue_deprecation_warning ("`scm_unprotect_object' is deprecated. "
                                   "Use `scm_gc_unprotect_object' instead.");
  return scm_gc_unprotect_object (obj);
}

SCM_SYMBOL (scm_sym_app, "app");
SCM_SYMBOL (scm_sym_modules, "modules");
static SCM module_prefix = SCM_BOOL_F;
static SCM make_modules_in_var;
static SCM beautify_user_module_x_var;
static SCM try_module_autoload_var;

static void
init_module_stuff ()
{
#define PERM(x) scm_permanent_object(x)

  if (module_prefix == SCM_BOOL_F)
    {
      module_prefix = PERM (scm_list_2 (scm_sym_app, scm_sym_modules));
      make_modules_in_var = PERM (scm_c_lookup ("make-modules-in"));
      beautify_user_module_x_var =
	PERM (scm_c_lookup ("beautify-user-module!"));
      try_module_autoload_var = PERM (scm_c_lookup ("try-module-autoload"));
    }
}

SCM
scm_the_root_module ()
{
  init_module_stuff ();
  scm_c_issue_deprecation_warning ("`scm_the_root_module' is deprecated. "
				   "Use `scm_c_resolve_module (\"guile\")' "
				   "instead.");

  return scm_c_resolve_module ("guile");
}

static SCM
scm_module_full_name (SCM name)
{
  init_module_stuff ();
  if (SCM_EQ_P (SCM_CAR (name), scm_sym_app))
    return name;
  else
    return scm_append (scm_list_2 (module_prefix, name));
}

SCM
scm_make_module (SCM name)
{
  init_module_stuff ();
  scm_c_issue_deprecation_warning ("`scm_make_module' is deprecated. "
				   "Use `scm_c_define_module instead.");

  return scm_call_2 (SCM_VARIABLE_REF (make_modules_in_var),
		     scm_the_root_module (),
		     scm_module_full_name (name));
}

SCM
scm_ensure_user_module (SCM module)
{
  init_module_stuff ();
  scm_c_issue_deprecation_warning ("`scm_ensure_user_module' is deprecated. "
				   "Use `scm_c_define_module instead.");

  scm_call_1 (SCM_VARIABLE_REF (beautify_user_module_x_var), module);
  return SCM_UNSPECIFIED;
}

SCM
scm_load_scheme_module (SCM name)
{
  init_module_stuff ();
  scm_c_issue_deprecation_warning ("`scm_load_scheme_module' is deprecated. "
				   "Use `scm_c_resolve_module instead.");

  return scm_call_1 (SCM_VARIABLE_REF (try_module_autoload_var), name);
}

/* This is implemented in C solely for SCM_COERCE_OUTPORT ... */

static void
maybe_close_port (void *data, SCM port)
{
  SCM except = (SCM)data;

  while (!SCM_NULLP (except))
    {
      SCM p = SCM_COERCE_OUTPORT (SCM_CAR (except));
      if (SCM_EQ_P (p, port))
	return;
      except = SCM_CDR (except);
    }

  scm_close_port (port);
}

SCM_DEFINE (scm_close_all_ports_except, "close-all-ports-except", 0, 0, 1,
           (SCM ports),
	    "[DEPRECATED] Close all open file ports used by the interpreter\n"
	    "except for those supplied as arguments.  This procedure\n"
	    "was intended to be used before an exec call to close file descriptors\n"
	    "which are not needed in the new process.  However it has the\n"
	    "undesirable side effect of flushing buffers, so it's deprecated.\n"
	    "Use port-for-each instead.")
#define FUNC_NAME s_scm_close_all_ports_except
{
  SCM p;
  SCM_VALIDATE_REST_ARGUMENT (ports);
  
  for (p = ports; !SCM_NULLP (p); p = SCM_CDR (p))
    SCM_VALIDATE_OPPORT (SCM_ARG1, SCM_COERCE_OUTPORT (SCM_CAR (p)));

  scm_c_port_for_each (maybe_close_port, ports);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_variable_set_name_hint, "variable-set-name-hint!", 2, 0, 0,
	    (SCM var, SCM hint),
	    "Do not use this function.")
#define FUNC_NAME s_scm_variable_set_name_hint
{
  SCM_VALIDATE_VARIABLE (1, var);
  SCM_VALIDATE_SYMBOL (2, hint);
  scm_c_issue_deprecation_warning
    ("'variable-set-name-hint!' is deprecated.  Do not use it.");
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_builtin_variable, "builtin-variable", 1, 0, 0, 
            (SCM name),
	    "Do not use this function.")
#define FUNC_NAME s_scm_builtin_variable
{
  SCM_VALIDATE_SYMBOL (1,name);
  scm_c_issue_deprecation_warning ("`builtin-variable' is deprecated. "
				   "Use module system operations instead.");
  return scm_sym2var (name, SCM_BOOL_F, SCM_BOOL_T);
}
#undef FUNC_NAME

SCM 
scm_makstr (size_t len, int dummy)
{
  scm_c_issue_deprecation_warning
    ("'scm_makstr' is deprecated.  Use 'scm_allocate_string' instead.");
  return scm_allocate_string (len);
}

SCM 
scm_makfromstr (const char *src, size_t len, int dummy SCM_UNUSED)
{
  scm_c_issue_deprecation_warning ("`scm_makfromstr' is deprecated. "
				   "Use `scm_mem2string' instead.");

  return scm_mem2string (src, len);
}

SCM
scm_internal_with_fluids (SCM fluids, SCM values, SCM (*cproc) (), void *cdata)
{
  scm_c_issue_deprecation_warning ("`scm_internal_with_fluids' is deprecated. "
				   "Use `scm_c_with_fluids' instead.");

  return scm_c_with_fluids (fluids, values, cproc, cdata);
}

SCM
scm_make_gsubr (const char *name, int req, int opt, int rst, SCM (*fcn)())
{
  scm_c_issue_deprecation_warning
    ("`scm_make_gsubr' is deprecated.  Use `scm_c_define_gsubr' instead.");

  return scm_c_define_gsubr (name, req, opt, rst, fcn);
}

SCM
scm_make_gsubr_with_generic (const char *name,
			     int req, int opt, int rst,
			     SCM (*fcn)(), SCM *gf)
{
  scm_c_issue_deprecation_warning
    ("`scm_make_gsubr_with_generic' is deprecated.  "
     "Use `scm_c_define_gsubr_with_generic' instead.");

  return scm_c_define_gsubr_with_generic (name, req, opt, rst, fcn, gf);
}

SCM
scm_create_hook (const char *name, int n_args)
{
  scm_c_issue_deprecation_warning
    ("'scm_create_hook' is deprecated.  "
     "Use 'scm_make_hook' and 'scm_c_define' instead.");
  {
    SCM hook = scm_make_hook (SCM_MAKINUM (n_args));
    scm_c_define (name, hook);
    return scm_permanent_object (hook);
  }
}

SCM_DEFINE (scm_sloppy_memq, "sloppy-memq", 2, 0, 0,
            (SCM x, SCM lst),
	    "This procedure behaves like @code{memq}, but does no type or error checking.\n"
	    "Its use is recommended only in writing Guile internals,\n"
            "not for high-level Scheme programs.")
#define FUNC_NAME s_scm_sloppy_memq
{
  scm_c_issue_deprecation_warning
    ("'sloppy-memq' is deprecated.  Use 'memq' instead.");

  for(;  SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_EQ_P (SCM_CAR (lst), x))
	return lst;
    }
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_sloppy_memv, "sloppy-memv", 2, 0, 0,
            (SCM x, SCM lst),
 	    "This procedure behaves like @code{memv}, but does no type or error checking.\n"
	    "Its use is recommended only in writing Guile internals,\n"
            "not for high-level Scheme programs.")
#define FUNC_NAME s_scm_sloppy_memv
{
  scm_c_issue_deprecation_warning
    ("'sloppy-memv' is deprecated.  Use 'memv' instead.");

  for(;  SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (! SCM_FALSEP (scm_eqv_p (SCM_CAR (lst), x)))
	return lst;
    }
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_sloppy_member, "sloppy-member", 2, 0, 0,
            (SCM x, SCM lst),
 	    "This procedure behaves like @code{member}, but does no type or error checking.\n"
	    "Its use is recommended only in writing Guile internals,\n"
            "not for high-level Scheme programs.")
#define FUNC_NAME s_scm_sloppy_member
{
  scm_c_issue_deprecation_warning
    ("'sloppy-member' is deprecated.  Use 'member' instead.");

  for(;  SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (! SCM_FALSEP (scm_equal_p (SCM_CAR (lst), x)))
	return lst;
    }
  return lst;
}
#undef FUNC_NAME

SCM_SYMBOL (scm_end_of_file_key, "end-of-file");

SCM_DEFINE (scm_read_and_eval_x, "read-and-eval!", 0, 1, 0, 
            (SCM port),
	    "Read a form from @var{port} (standard input by default), and evaluate it\n"
	    "(memoizing it in the process) in the top-level environment.  If no data\n"
	    "is left to be read from @var{port}, an @code{end-of-file} error is\n"
	    "signalled.")
#define FUNC_NAME s_scm_read_and_eval_x
{
  SCM form;

  scm_c_issue_deprecation_warning
    ("'read-and-eval!' is deprecated.  Use 'read' and 'eval' instead.");

  form = scm_read (port);
  if (SCM_EOF_OBJECT_P (form))
    scm_ithrow (scm_end_of_file_key, SCM_EOL, 1);
  return scm_eval_x (form, scm_current_module ());
}
#undef FUNC_NAME

SCM
scm_make_subr_opt (const char *name, int type, SCM (*fcn) (), int set)
{
  scm_c_issue_deprecation_warning 
    ("`scm_make_subr_opt' is deprecated.  Use `scm_c_make_subr' or "
     "`scm_c_define_subr' instead.");

  if (set)
    return scm_c_define_subr (name, type, fcn);
  else
    return scm_c_make_subr (name, type, fcn);
}

SCM 
scm_make_subr (const char *name, int type, SCM (*fcn) ())
{
  scm_c_issue_deprecation_warning 
    ("`scm_make_subr' is deprecated.  Use `scm_c_define_subr' instead.");

  return scm_c_define_subr (name, type, fcn);
}

SCM
scm_make_subr_with_generic (const char *name, int type, SCM (*fcn) (), SCM *gf)
{
  scm_c_issue_deprecation_warning 
    ("`scm_make_subr_with_generic' is deprecated.  Use "
     "`scm_c_define_subr_with_generic' instead.");
  
  return scm_c_define_subr_with_generic (name, type, fcn, gf);
}

/* Call thunk(closure) underneath a top-level error handler.
 * If an error occurs, pass the exitval through err_filter and return it.
 * If no error occurs, return the value of thunk.
 */

#ifdef _UNICOS
typedef int setjmp_type;
#else
typedef long setjmp_type;
#endif

struct cce_handler_data {
  SCM (*err_filter) ();
  void *closure;
};

static SCM
invoke_err_filter (void *d, SCM tag, SCM args)
{
  struct cce_handler_data *data = (struct cce_handler_data *)d;
  return data->err_filter (SCM_BOOL_F, data->closure);
}

SCM
scm_call_catching_errors (SCM (*thunk)(), SCM (*err_filter)(), void *closure)
{
  scm_c_issue_deprecation_warning
    ("'scm_call_catching_errors' is deprecated.  "
     "Use 'scm_internal_catch' instead.");
  
  {
    struct cce_handler_data data;
    data.err_filter = err_filter;
    data.closure = closure;
    return scm_internal_catch (SCM_BOOL_T,
			       (scm_t_catch_body)thunk, closure,
			       (scm_t_catch_handler)invoke_err_filter, &data);
  }
}

long
scm_make_smob_type_mfpe (char *name, size_t size,
                        SCM (*mark) (SCM),
                        size_t (*free) (SCM),
                        int (*print) (SCM, SCM, scm_print_state *),
                        SCM (*equalp) (SCM, SCM))
{
  scm_c_issue_deprecation_warning
    ("'scm_make_smob_type_mfpe' is deprecated.  "
     "Use 'scm_make_smob_type' plus 'scm_set_smob_*' instead.");

  {
    long answer = scm_make_smob_type (name, size);
    scm_set_smob_mfpe (answer, mark, free, print, equalp);
    return answer;
  }
}

void
scm_set_smob_mfpe (long tc, 
		   SCM (*mark) (SCM),
		   size_t (*free) (SCM),
		   int (*print) (SCM, SCM, scm_print_state *),
		   SCM (*equalp) (SCM, SCM))
{
  scm_c_issue_deprecation_warning
    ("'scm_set_smob_mfpe' is deprecated.  "
     "Use 'scm_set_smob_mark' instead, for example.");

  if (mark) scm_set_smob_mark (tc, mark);
  if (free) scm_set_smob_free (tc, free);
  if (print) scm_set_smob_print (tc, print);
  if (equalp) scm_set_smob_equalp (tc, equalp);
}

SCM
scm_read_0str (char *expr)
{
  scm_c_issue_deprecation_warning 
    ("scm_read_0str is deprecated.  Use scm_c_read_string instead.");

  return scm_c_read_string (expr);
}

SCM
scm_eval_0str (const char *expr)
{
  scm_c_issue_deprecation_warning 
    ("scm_eval_0str is deprecated.  Use scm_c_eval_string instead.");

  return scm_c_eval_string (expr);
}

SCM
scm_strprint_obj (SCM obj)
{
  scm_c_issue_deprecation_warning 
    ("scm_strprint_obj is deprecated.  Use scm_object_to_string instead.");
  return scm_object_to_string (obj, SCM_UNDEFINED);
}

char *
scm_i_object_chars (SCM obj)
{
  scm_c_issue_deprecation_warning 
    ("SCM_CHARS is deprecated.  Use SCM_STRING_CHARS or "
     "SCM_SYMBOL_CHARS instead.");
  if (SCM_STRINGP (obj))
    return SCM_STRING_CHARS (obj);
  if (SCM_SYMBOLP (obj))
    return SCM_SYMBOL_CHARS (obj);
  abort ();
}

long
scm_i_object_length (SCM obj)
{
  scm_c_issue_deprecation_warning 
    ("SCM_LENGTH is deprecated.  Use SCM_STRING_LENGTH instead, for example.");
  if (SCM_STRINGP (obj))
    return SCM_STRING_LENGTH (obj);
  if (SCM_SYMBOLP (obj))
    return SCM_SYMBOL_LENGTH (obj);
  if (SCM_VECTORP (obj))
    return SCM_VECTOR_LENGTH (obj);
  abort ();
}

SCM 
scm_sym2ovcell_soft (SCM sym, SCM obarray)
{
  SCM lsym, z;
  size_t hash = SCM_SYMBOL_HASH (sym) % SCM_VECTOR_LENGTH (obarray);

  scm_c_issue_deprecation_warning ("`scm_sym2ovcell_soft' is deprecated. "
				   "Use hashtables instead.");

  SCM_REDEFER_INTS;
  for (lsym = SCM_VECTOR_REF (obarray, hash);
       SCM_NIMP (lsym);
       lsym = SCM_CDR (lsym))
    {
      z = SCM_CAR (lsym);
      if (SCM_EQ_P (SCM_CAR (z), sym))
	{
	  SCM_REALLOW_INTS;
	  return z;
	}
    }
  SCM_REALLOW_INTS;
  return SCM_BOOL_F;
}


SCM 
scm_sym2ovcell (SCM sym, SCM obarray)
#define FUNC_NAME "scm_sym2ovcell"
{
  SCM answer;

  scm_c_issue_deprecation_warning ("`scm_sym2ovcell' is deprecated. "
				   "Use hashtables instead.");

  answer = scm_sym2ovcell_soft (sym, obarray);
  if (!SCM_FALSEP (answer))
    return answer;
  SCM_MISC_ERROR ("uninterned symbol: ~S", scm_list_1 (sym));
  return SCM_UNSPECIFIED;		/* not reached */
}
#undef FUNC_NAME


/* Intern a symbol whose name is the LEN characters at NAME in OBARRAY.

   OBARRAY should be a vector of lists, indexed by the name's hash
   value, modulo OBARRAY's length.  Each list has the form 
   ((SYMBOL . VALUE) ...), where SYMBOL is a symbol, and VALUE is the
   value associated with that symbol (in the current module?  in the
   system module?)

   To "intern" a symbol means: if OBARRAY already contains a symbol by
   that name, return its (SYMBOL . VALUE) pair; otherwise, create a
   new symbol, add the pair (SYMBOL . SCM_UNDEFINED) to the
   appropriate list of the OBARRAY, and return the pair.

   If softness is non-zero, don't create a symbol if it isn't already
   in OBARRAY; instead, just return #f.

   If OBARRAY is SCM_BOOL_F, create a symbol listed in no obarray and
   return (SYMBOL . SCM_UNDEFINED).  */


SCM 
scm_intern_obarray_soft (const char *name,size_t len,SCM obarray,unsigned int softness)
{
  SCM symbol = scm_mem2symbol (name, len);
  size_t raw_hash = SCM_SYMBOL_HASH (symbol);
  size_t hash;
  SCM lsym;

  scm_c_issue_deprecation_warning ("`scm_intern_obarray_soft' is deprecated. "
				   "Use hashtables instead.");

  if (SCM_FALSEP (obarray))
    {
      if (softness)
	return SCM_BOOL_F;
      else
	return scm_cons (symbol, SCM_UNDEFINED);
    }

  hash = raw_hash % SCM_VECTOR_LENGTH (obarray);

  for (lsym = SCM_VECTOR_REF(obarray, hash);
       SCM_NIMP (lsym); lsym = SCM_CDR (lsym))
    {
      SCM a = SCM_CAR (lsym);
      SCM z = SCM_CAR (a);
      if (SCM_EQ_P (z, symbol))
	return a;
    }
  
  if (softness)
    {
      return SCM_BOOL_F;
    }
  else
    {
      SCM cell = scm_cons (symbol, SCM_UNDEFINED);
      SCM slot = SCM_VECTOR_REF (obarray, hash);

      SCM_VECTOR_SET (obarray, hash, scm_cons (cell, slot));

      return cell;
    }
}


SCM
scm_intern_obarray (const char *name,size_t len,SCM obarray)
{
  scm_c_issue_deprecation_warning ("`scm_intern_obarray' is deprecated. "
				   "Use hashtables instead.");

  return scm_intern_obarray_soft (name, len, obarray, 0);
}

/* Lookup the value of the symbol named by the nul-terminated string
   NAME in the current module.  */
SCM
scm_symbol_value0 (const char *name)
{
  scm_c_issue_deprecation_warning ("`scm_symbol_value0' is deprecated. "
				   "Use `scm_lookup' instead.");

  return scm_variable_ref (scm_c_lookup (name));
}

SCM_DEFINE (scm_string_to_obarray_symbol, "string->obarray-symbol", 2, 1, 0,
           (SCM o, SCM s, SCM softp),
	    "Intern a new symbol in @var{obarray}, a symbol table, with name\n"
	    "@var{string}.\n\n"
	    "If @var{obarray} is @code{#f}, use the default system symbol table.  If\n"
	    "@var{obarray} is @code{#t}, the symbol should not be interned in any\n"
	    "symbol table; merely return the pair (@var{symbol}\n"
	    ". @var{#<undefined>}).\n\n"
	    "The @var{soft?} argument determines whether new symbol table entries\n"
	    "should be created when the specified symbol is not already present in\n"
	    "@var{obarray}.  If @var{soft?} is specified and is a true value, then\n"
	    "new entries should not be added for symbols not already present in the\n"
	    "table; instead, simply return @code{#f}.")
#define FUNC_NAME s_scm_string_to_obarray_symbol
{
  SCM vcell;
  SCM answer;
  int softness;

  SCM_VALIDATE_STRING (2, s);
  SCM_ASSERT (SCM_BOOLP (o) || SCM_VECTORP (o), o, SCM_ARG1, FUNC_NAME);

  scm_c_issue_deprecation_warning ("`string->obarray-symbol' is deprecated. "
				   "Use hashtables instead.");

  softness = (!SCM_UNBNDP (softp) && !SCM_FALSEP(softp));
  /* iron out some screwy calling conventions */
  if (SCM_FALSEP (o))
    {
      /* nothing interesting to do here. */
      return scm_string_to_symbol (s);
    }
  else if (SCM_EQ_P (o, SCM_BOOL_T))
    o = SCM_BOOL_F;
    
  vcell = scm_intern_obarray_soft (SCM_STRING_CHARS(s),
				   SCM_STRING_LENGTH (s),
				   o,
				   softness);
  if (SCM_FALSEP (vcell))
    return vcell;
  answer = SCM_CAR (vcell);
  return answer;
}
#undef FUNC_NAME

SCM_DEFINE (scm_intern_symbol, "intern-symbol", 2, 0, 0,
           (SCM o, SCM s),
	    "Add a new symbol to @var{obarray} with name @var{string}, bound to an\n"
	    "unspecified initial value.  The symbol table is not modified if a symbol\n"
	    "with this name is already present.")
#define FUNC_NAME s_scm_intern_symbol
{
  size_t hval;
  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    return SCM_UNSPECIFIED;

  scm_c_issue_deprecation_warning ("`intern-symbol' is deprecated. "
				   "Use hashtables instead.");

  SCM_VALIDATE_VECTOR (1,o);
  hval = SCM_SYMBOL_HASH (s) % SCM_VECTOR_LENGTH (o);
  /* If the symbol is already interned, simply return. */
  SCM_REDEFER_INTS;
  {
    SCM lsym;
    SCM sym;
    for (lsym = SCM_VECTOR_REF (o, hval);
	 SCM_NIMP (lsym);
	 lsym = SCM_CDR (lsym))
      {
	sym = SCM_CAR (lsym);
	if (SCM_EQ_P (SCM_CAR (sym), s))
	  {
	    SCM_REALLOW_INTS;
	    return SCM_UNSPECIFIED;
	  }
      }
    SCM_VECTOR_SET (o, hval, 
		    scm_acons (s, SCM_UNDEFINED,
			       SCM_VECTOR_REF (o, hval)));
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_unintern_symbol, "unintern-symbol", 2, 0, 0,
           (SCM o, SCM s),
	    "Remove the symbol with name @var{string} from @var{obarray}.  This\n"
	    "function returns @code{#t} if the symbol was present and @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_unintern_symbol
{
  size_t hval;

  scm_c_issue_deprecation_warning ("`unintern-symbol' is deprecated. "
				   "Use hashtables instead.");

  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    return SCM_BOOL_F;
  SCM_VALIDATE_VECTOR (1,o);
  hval = SCM_SYMBOL_HASH (s) % SCM_VECTOR_LENGTH (o);
  SCM_DEFER_INTS;
  {
    SCM lsym_follow;
    SCM lsym;
    SCM sym;
    for (lsym = SCM_VECTOR_REF (o, hval), lsym_follow = SCM_BOOL_F;
	 SCM_NIMP (lsym);
	 lsym_follow = lsym, lsym = SCM_CDR (lsym))
      {
	sym = SCM_CAR (lsym);
	if (SCM_EQ_P (SCM_CAR (sym), s))
	  {
	    /* Found the symbol to unintern. */
	    if (SCM_FALSEP (lsym_follow))
	      SCM_VECTOR_SET (o, hval, lsym);
	    else
	      SCM_SETCDR (lsym_follow, SCM_CDR(lsym));
	    SCM_ALLOW_INTS;
	    return SCM_BOOL_T;
	  }
      }
  }
  SCM_ALLOW_INTS;
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_binding, "symbol-binding", 2, 0, 0,
           (SCM o, SCM s),
	    "Look up in @var{obarray} the symbol whose name is @var{string}, and\n"
	    "return the value to which it is bound.  If @var{obarray} is @code{#f},\n"
	    "use the global symbol table.  If @var{string} is not interned in\n"
	    "@var{obarray}, an error is signalled.")
#define FUNC_NAME s_scm_symbol_binding
{
  SCM vcell;

  scm_c_issue_deprecation_warning ("`symbol-binding' is deprecated. "
				   "Use hashtables instead.");

  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    return scm_variable_ref (scm_lookup (s));
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell (s, o);
  return SCM_CDR(vcell);
}
#undef FUNC_NAME

#if 0
SCM_DEFINE (scm_symbol_interned_p, "symbol-interned?", 2, 0, 0,
	    (SCM o, SCM s),
	    "Return @code{#t} if @var{obarray} contains a symbol with name\n"
	    "@var{string}, and @code{#f} otherwise.")
#define FUNC_NAME s_scm_symbol_interned_p
{
  SCM vcell;

  scm_c_issue_deprecation_warning ("`symbol-interned?' is deprecated. "
				   "Use hashtables instead.");

  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    {
      SCM var = scm_sym2var (s, SCM_BOOL_F, SCM_BOOL_F);
      if (var != SCM_BOOL_F)
	return SCM_BOOL_T;
      return SCM_BOOL_F;
    }
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell_soft (s, o);
  return (SCM_NIMP(vcell)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}
#undef FUNC_NAME
#endif

SCM_DEFINE (scm_symbol_bound_p, "symbol-bound?", 2, 0, 0,
	    (SCM o, SCM s),
	    "Return @code{#t} if @var{obarray} contains a symbol with name\n"
	    "@var{string} bound to a defined value.  This differs from\n"
	    "@var{symbol-interned?} in that the mere mention of a symbol\n"
	    "usually causes it to be interned; @code{symbol-bound?}\n"
	    "determines whether a symbol has been given any meaningful\n"
	    "value.")
#define FUNC_NAME s_scm_symbol_bound_p
{
  SCM vcell;

  scm_c_issue_deprecation_warning ("`symbol-bound?' is deprecated. "
				   "Use hashtables instead.");

  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    {
      SCM var = scm_sym2var (s, SCM_BOOL_F, SCM_BOOL_F);
      if (SCM_VARIABLEP(var) && !SCM_UNBNDP(SCM_VARIABLE_REF(var)))
	return SCM_BOOL_T;
      return SCM_BOOL_F;
    }
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell_soft (s, o);
  return SCM_BOOL (SCM_NIMP (vcell) && !SCM_UNBNDP (SCM_CDR (vcell)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_set_x, "symbol-set!", 3, 0, 0,
           (SCM o, SCM s, SCM v),
	    "Find the symbol in @var{obarray} whose name is @var{string}, and rebind\n"
	    "it to @var{value}.  An error is signalled if @var{string} is not present\n"
	    "in @var{obarray}.")
#define FUNC_NAME s_scm_symbol_set_x
{
  SCM vcell;

  scm_c_issue_deprecation_warning ("`symbol-set!' is deprecated. "
				   "Use the module system instead.");

  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    {
      scm_define (s, v);
      return SCM_UNSPECIFIED;
    }
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell (s, o);
  SCM_SETCDR (vcell, v);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define MAX_PREFIX_LENGTH 30

static int gentemp_counter;

SCM_DEFINE (scm_gentemp, "gentemp", 0, 2, 0,
            (SCM prefix, SCM obarray),
	    "Create a new symbol with a name unique in an obarray.\n"
	    "The name is constructed from an optional string @var{prefix}\n"
	    "and a counter value.  The default prefix is @code{t}.  The\n"
	    "@var{obarray} is specified as a second optional argument.\n"
	    "Default is the system obarray where all normal symbols are\n"
	    "interned.  The counter is increased by 1 at each\n"
	    "call.  There is no provision for resetting the counter.")
#define FUNC_NAME s_scm_gentemp
{
  char buf[MAX_PREFIX_LENGTH + SCM_INTBUFLEN];
  char *name = buf;
  int len, n_digits;

  scm_c_issue_deprecation_warning ("`gentemp' is deprecated. "
				   "Use `gensym' instead.");

  if (SCM_UNBNDP (prefix))
    {
      name[0] = 't';
      len = 1;
    }
  else
    {
      SCM_VALIDATE_STRING (1, prefix);
      len = SCM_STRING_LENGTH (prefix);
      if (len > MAX_PREFIX_LENGTH)
	name = SCM_MUST_MALLOC (MAX_PREFIX_LENGTH + SCM_INTBUFLEN);
      strncpy (name, SCM_STRING_CHARS (prefix), len);
    }

  if (SCM_UNBNDP (obarray))
    return scm_gensym (prefix);
  else
    SCM_ASSERT ((SCM_VECTORP (obarray) || SCM_WVECTP (obarray)),
		obarray,
		SCM_ARG2,
		FUNC_NAME);
  do
    n_digits = scm_iint2str (gentemp_counter++, 10, &name[len]);
  while (!SCM_FALSEP (scm_intern_obarray_soft (name,
					       len + n_digits,
					       obarray,
					       1)));
  {
    SCM vcell = scm_intern_obarray_soft (name,
					 len + n_digits,
					 obarray,
					 0);
    if (name != buf)
      scm_must_free (name);
    return SCM_CAR (vcell);
  }
}
#undef FUNC_NAME

void
scm_i_init_deprecated ()
{
#include "libguile/deprecated.x"
}

#endif