/*	Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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


/* This software is a derivative work of other copyrighted softwares; the
 * copyright notices of these softwares are placed in the file COPYRIGHTS
 *
 * This file is based upon stklos.c from the STk distribution by
 * Erick Gallesio <eg@unice.fr>.
 */

#include <libguile.h>
#include <libguile/objects.h>
#include <libguile/modules.h>
#include <guile/gh.h>

#include "goops.h"
#include "versiondat.h"

#define CLASSP(x)   (SCM_STRUCTP (x) \
		     && SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_METACLASS)
#define GENERICP(x) (SCM_INSTANCEP (x) \
		     && SCM_SUBCLASSP (SCM_CLASS_OF (x), scm_class_generic))
#define METHODP(x)  (SCM_INSTANCEP (x) \
		     && SCM_SUBCLASSP(SCM_CLASS_OF(x), scm_class_method))
#define SPEC_OF(x)  SCM_SLOT (x, scm_si_specializers)


#define DEFVAR(v,val) \
{ scm_eval2 (SCM_LIST3 (scm_sym_define_public, (v), (val)), \
	     scm_goops_lookup_closure); }
/* Temporary hack until we get the new module system */
/*fixme* Should optimize by keeping track of the variable object itself */
#define GETVAR(v) (SCM_CDDR (scm_apply (scm_goops_lookup_closure, \
					SCM_LIST2 ((v), SCM_BOOL_F), \
					SCM_EOL)))
#define Intern(s) (gh_symbol2scm (s))

/* Fixme: Should use already interned symbols */
#define CALL_GF1(name,a)	(scm_apply (GETVAR (Intern(name)), \
					    SCM_LIST1 (a), SCM_EOL))
#define CALL_GF2(name,a,b)	(scm_apply (GETVAR (Intern(name)), \
					    SCM_LIST2 (a, b), SCM_EOL))
#define CALL_GF3(name,a,b,c)	(scm_apply (GETVAR (Intern(name)), \
					    SCM_LIST3 (a, b, c), SCM_EOL))
#define CALL_GF4(name,a,b,c,d)	(scm_apply (GETVAR (Intern(name)), \
					    SCM_LIST4 (a, b, c, d), SCM_EOL))

/* Class redefinition protocol:

   A class is represented by a heap header h1 which points to a
   malloc:ed memory block m1.

   When a new version of a class is created, a new header h2 and
   memory block m2 are allocated.  The headers h1 and h2 then switch
   pointers so that h1 refers to m2 and h2 to m1.  In this way, names
   bound to h1 will point to the new class at the same time as h2 will
   be a handle which the GC will us to free m1.

   The `redefined' slot of m1 will be set to point to h1.  An old
   instance will have it's class pointer (the CAR of the heap header)
   pointing to m1.  The non-immediate `redefined'-slot in m1 indicates
   the class modification and the new class pointer can be found via
   h1.
*/

#define SCM_CLASS_REDEF(c) SCM_SLOT (c, scm_si_redefined)
/* The following definition is located in libguile/objects.h:
#define SCM_OBJ_CLASS_REDEF(x) (SCM_STRUCT_VTABLE_DATA(x)[scm_si_redefined])
*/

#define TEST_CHANGE_CLASS(obj, class) 					      \
	{ 								      \
	  class = SCM_CLASS_OF (obj);					      \
          if (SCM_OBJ_CLASS_REDEF (obj) != SCM_BOOL_F) 			      \
	    CALL_GF3 ("change-object-class",				      \
		      obj, class, SCM_OBJ_CLASS_REDEF (obj));  		      \
	}

#define NXT_MTHD_METHODS(m)	(SCM_VELTS (m)[1])
#define NXT_MTHD_ARGS(m)	(SCM_VELTS (m)[2])

#ifdef SCM_UNBOUND
#define SCM_GOOPS_UNBOUND SCM_UNBOUND
#else
#define SCM_GOOPS_UNBOUND scm_goops_the_unbound_value
#endif
#define SCM_GOOPS_UNBOUNDP(x) ((x) == SCM_GOOPS_UNBOUND)

static int goops_loaded_p = 0;
static scm_rstate *goops_rstate;

#ifndef SCM_UNBOUND
static SCM scm_goops_the_unbound_value;
#endif
static SCM scm_goops_lookup_closure;

/* Some classes are defined in libguile/objects.c. */
SCM scm_class_top, scm_class_object, scm_class_class;
SCM scm_class_entity, scm_class_entity_with_setter;
SCM scm_class_generic, scm_class_generic_with_setter, scm_class_method;
SCM scm_class_simple_method, scm_class_accessor;
SCM scm_class_procedure_class;
SCM scm_class_operator_class, scm_class_operator_with_setter_class;
SCM scm_class_entity_class;
SCM scm_class_number, scm_class_list;
SCM scm_class_keyword;
SCM scm_class_port, scm_class_input_output_port;
SCM scm_class_input_port, scm_class_output_port;
SCM scm_class_foreign_class, scm_class_foreign_object;
SCM scm_class_foreign_slot;
SCM scm_class_self, scm_class_protected;
SCM scm_class_opaque, scm_class_read_only;
SCM scm_class_protected_opaque, scm_class_protected_read_only;
SCM scm_class_scm;
SCM scm_class_int, scm_class_float, scm_class_double;

SCM_SYMBOL (scm_sym_define_public, "define-public");

static SCM scm_make_unbound (void);
static SCM scm_unbound_p (SCM obj);

SCM_PROC (s_goops_version, "goops-version", 0, 0, 0, scm_goops_version);

SCM
scm_goops_version ()
{
  return scm_makfrom0str (GOOPS_VERSION);
}

SCM_PROC (s_oldfmt, "oldfmt", 1, 0, 0, scm_oldfmt);

char *
scm_c_oldfmt (char *from, int n)
{
#ifdef HAVE_SCM_SIMPLE_FORMAT
  return from;
#else
  static struct { int n; char *from; char *to; } *strings;
  static int size = 0;
  static int n_strings = 0;
  char *to;
  int i;
  
  for (i = 0; i < n_strings; ++i)
    if (n == strings[i].n && strncmp (from, strings[i].from, n) == 0)
      return strings[i].to;
  
  if (n_strings == size)
    {
      if (size == 0)
	{
	  size = 10;
	  strings = scm_must_malloc (size * sizeof (*strings), s_oldfmt);
	}
      else
	{
	  int oldsize = size;
	  size = 3 * oldsize / 2;
	  strings = scm_must_realloc (strings,
				      oldsize * sizeof (*strings),
				      size * sizeof (*strings),
				      s_oldfmt);
	}
    }

  strings[n_strings].n = n;
  strings[n_strings].from = strncpy (scm_must_malloc (n, s_oldfmt), from, n);
  to = strings[n_strings].to = scm_must_malloc (n + 1, s_oldfmt);
  n_strings++;

  for (i = 0; i < n; ++i)
    {
      if (from[i] == '~' && ++i < n)
	{
	  if (from[i] == 'A')
	    {
	      to[i - 1] = '%';
	      to[i] = 's';
	    }
	  else if (from[i] == 'S')
	    {
	      to[i - 1] = '%';
	      to[i] = 'S';
	    }
	  else
	    {
	      to[i - 1] = '~';
	      to[i] = from[i];
	    }
	  continue;
	}
      to[i] = from[i];
    }
  to[i] = '\0';
  
  return to;
#endif
}

char *
scm_c_oldfmt0 (char *s)
{
#ifdef HAVE_SCM_SIMPLE_FORMAT
  return s;
#else
  return scm_c_oldfmt (s, strlen (s));
#endif
}

SCM
scm_oldfmt (SCM s)
{
#ifdef HAVE_SCM_SIMPLE_FORMAT
  return s;
#else
  int n;
  SCM_ASSERT (SCM_NIMP (s) && SCM_STRINGP (s), s, 1, s_oldfmt);
  n = SCM_LENGTH (s);
  return scm_return_first (scm_makfromstr (scm_c_oldfmt (SCM_ROCHARS (s), n),
					   n,
					   0),
			   s);
#endif
}

/******************************************************************************
 *
 * Compute-cpl
 *
 *   This version doesn't handle multiple-inheritance. It serves only for
 * booting classes and will be overaloaded in Scheme
 *
 ******************************************************************************/

#if 0
static SCM
compute_cpl (SCM supers, SCM res)
{
  return (SCM_NULLP (supers)
	  ? scm_reverse (res)
	  : compute_cpl (SCM_SLOT (SCM_CAR (supers), scm_si_direct_supers),
			 scm_cons (SCM_CAR (supers), res)));
}
#endif

static SCM
map (SCM (*proc) (SCM), SCM ls)
{
  if (SCM_IMP (ls))
    return ls;
  {
    SCM res = scm_cons (proc (SCM_CAR (ls)), SCM_EOL);
    SCM h = res;
    ls = SCM_CDR (ls);
    while (SCM_NIMP (ls))
      {
	SCM_SETCDR (h, scm_cons (proc (SCM_CAR (ls)), SCM_EOL));
	h = SCM_CDR (h);
	ls = SCM_CDR (ls);
      }
    return res;
  }
}

static SCM
filter_cpl (SCM ls)
{
  SCM res = SCM_EOL;
  while (SCM_NIMP (ls))
    {
      SCM el = SCM_CAR (ls);
      if (SCM_IMP (scm_sloppy_memq (el, res)))
	res = scm_cons (el, res);
      ls = SCM_CDR (ls);
    }
  return res;
}

static SCM
compute_cpl (SCM class)
{
  if (goops_loaded_p)
    return CALL_GF1 ("compute-cpl", class);
  else
    {
      SCM supers = SCM_SLOT (class, scm_si_direct_supers);
      SCM ls = scm_append (scm_acons (class, supers,
				      map (compute_cpl, supers)));
      return scm_reverse_x (filter_cpl (ls), SCM_EOL);
    }
}

/******************************************************************************
 *
 * compute-slots
 *
 ******************************************************************************/

static SCM
remove_duplicate_slots (SCM l, SCM res, SCM slots_already_seen)
{
  SCM tmp;

  if (SCM_NULLP (l))
    return res;

  tmp = SCM_CAAR (l);
  if (!(SCM_NIMP (tmp) && SCM_SYMBOLP (tmp)))
    scm_misc_error ("%compute-slots",
		    scm_c_oldfmt0 ("bad slot name ~S"),
		    SCM_LIST1 (tmp));
  
  if (SCM_NULLP (scm_sloppy_memq (tmp, slots_already_seen))) {
    res 	       = scm_cons (SCM_CAR (l), res);
    slots_already_seen = scm_cons (tmp, slots_already_seen);
  }
  
  return remove_duplicate_slots (SCM_CDR (l), res, slots_already_seen);
}

static SCM
build_slots_list (SCM dslots, SCM cpl)
{
  register SCM res = dslots;

  for (cpl = SCM_CDR(cpl); SCM_NNULLP(cpl); cpl = SCM_CDR(cpl))
    res = scm_append (SCM_LIST2 (SCM_SLOT (SCM_CAR (cpl), scm_si_direct_slots),
				 res));

  /* res contains a list of slots. Remove slots which appears more than once */
  return remove_duplicate_slots (scm_reverse (res), SCM_EOL, SCM_EOL);
}

static SCM
maplist (SCM ls)
{
  SCM orig = ls;
  while (SCM_NIMP (ls))
    {
      if (!(SCM_NIMP (SCM_CAR (ls)) && SCM_CONSP (SCM_CAR (ls))))
	SCM_SETCAR (ls, scm_cons (SCM_CAR (ls), SCM_EOL));
      ls = SCM_CDR (ls);
    }
  return orig;
}

SCM_PROC (s_sys_compute_slots, "%compute-slots", 1, 0, 0, scm_sys_compute_slots);

SCM
scm_sys_compute_slots (SCM class)
{
  SCM_ASSERT (SCM_NIMP (class) && CLASSP (class),
	      class, SCM_ARG1, s_sys_compute_slots);
  return build_slots_list (SCM_SLOT (class, scm_si_direct_slots),
			   SCM_SLOT (class, scm_si_cpl));
}

/******************************************************************************
 *
 * compute-getters-n-setters
 *  
 *   This version doesn't handle slot options. It serves only for booting 
 * classes and will be overaloaded in Scheme.
 *
 ******************************************************************************/

SCM_KEYWORD (k_init_value, "init-value");
SCM_KEYWORD (k_init_thunk, "init-thunk");

static SCM
compute_getters_n_setters (SCM slots)
{
  SCM res = SCM_EOL;
  SCM *cdrloc = &res;
  long i   = 0;

  for (  ; SCM_NNULLP(slots); slots = SCM_CDR(slots))
    {
      SCM init = SCM_BOOL_F;
      SCM options = SCM_CDAR (slots);
      if (SCM_NNULLP (options))
	{
	  init = scm_get_keyword (k_init_value, options, 0);
	  if (init)
	    init = scm_closure (SCM_LIST2 (SCM_EOL, init), SCM_EOL);
	  else
	    init = scm_get_keyword (k_init_thunk, options, SCM_BOOL_F);
	}
      *cdrloc = scm_cons (scm_cons (SCM_CAAR (slots),
				    scm_cons (init,
					      SCM_MAKINUM (i++))),
			  SCM_EOL);
      cdrloc = SCM_CDRLOC (*cdrloc);
    }
  return res;
}

/******************************************************************************
 *
 * initialize-object
 *
 ******************************************************************************/

/*fixme* Manufacture keywords in advance */
SCM
scm_i_get_keyword (SCM key, SCM l, int len, SCM default_value, const char *subr)
{
  int i;
  for (i = 0; i < len; i += 2)
    {
      if (!(SCM_NIMP (SCM_CAR (l)) && SCM_KEYWORDP (SCM_CAR (l))))
	scm_misc_error (subr,
			scm_c_oldfmt0 ("bad keyword: ~S"),
			SCM_LIST1 (SCM_CAR (l)));
      if (SCM_CAR (l) == key)
	return SCM_CADR (l);
      l = SCM_CDDR (l);
    }
  return default_value;
}

SCM_PROC (s_get_keyword, "get-keyword", 3, 0, 0, scm_get_keyword);

SCM
scm_get_keyword (SCM key, SCM l, SCM default_value)
{
  int len;
  SCM_ASSERT (SCM_NIMP (key) && SCM_KEYWORDP (key),
	      key,
	      scm_c_oldfmt0 ("Bad keyword: ~S"),
	      s_get_keyword);
  len = scm_ilength (l);
  SCM_ASSERT (len >= 0 && (len & 1) == 0, l,
	      scm_c_oldfmt0 ("Bad keyword-value list: ~S"),
	      s_get_keyword);
  return scm_i_get_keyword (key, l, len, default_value, s_get_keyword);
}

SCM_PROC (s_sys_initialize_object, "%initialize-object", 2, 0, 0, scm_sys_initialize_object);

SCM_KEYWORD (k_init_keyword, "init-keyword");

static SCM get_slot_value (SCM class, SCM obj, SCM slotdef);
static SCM set_slot_value (SCM class, SCM obj, SCM slotdef, SCM value);

SCM
scm_sys_initialize_object (SCM obj, SCM initargs)
{
  SCM tmp, get_n_set, slots;
  SCM class       = SCM_CLASS_OF (obj);
  int n_initargs;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_sys_initialize_object);
  n_initargs = scm_ilength (initargs);
  SCM_ASSERT ((n_initargs & 1) == 0,
	      initargs, SCM_ARG2, s_sys_initialize_object);
  
  get_n_set = SCM_SLOT (class, scm_si_getters_n_setters);
  slots     = SCM_SLOT (class, scm_si_slots);
  
  /* See for each slot how it must be initialized */
  for (;
       SCM_NNULLP (slots);
       get_n_set = SCM_CDR (get_n_set), slots = SCM_CDR (slots))
    {
      SCM slot_name  = SCM_CAR (slots);
      SCM slot_value = 0;
    
      if (SCM_NIMP (SCM_CDR (slot_name)))
	{
	  /* This slot admits (perhaps) to be initialized at creation time */
	  int n = scm_ilength (SCM_CDR (slot_name));
	  if (n & 1) /* odd or -1 */
	    scm_misc_error (s_sys_initialize_object,
			    scm_c_oldfmt0 ("class contains bogus slot definition: ~S"),
			    SCM_LIST1 (slot_name));
	  tmp 	= scm_i_get_keyword (k_init_keyword,
				     SCM_CDR (slot_name),
				     n,
				     0,
				     s_sys_initialize_object);
	  slot_name = SCM_CAR (slot_name);
	  if (tmp)
	    {
	      /* an initarg was provided for this slot */
	      if (!(SCM_NIMP (tmp) && SCM_KEYWORDP (tmp)))
		scm_misc_error (s_sys_initialize_object,
				scm_c_oldfmt0 ("initarg must be a keyword. It was ~S"),
				SCM_LIST1 (tmp));
	      slot_value = scm_i_get_keyword (tmp,
					      initargs,
					      n_initargs,
					      0,
					      s_sys_initialize_object);
	    }
	}

      if (slot_value)
	/* set slot to provided value */
	set_slot_value (class, obj, SCM_CAR (get_n_set), slot_value);
      else
	{
	  /* set slot to its :init-form if it exists */
	  tmp = SCM_CADAR (get_n_set);
	  if (tmp != SCM_BOOL_F)
	    {
	      slot_value = get_slot_value (class, obj, SCM_CAR (get_n_set));
	      if (SCM_GOOPS_UNBOUNDP (slot_value))
		{
		  SCM env = SCM_EXTEND_ENV (SCM_EOL, SCM_EOL, SCM_ENV (tmp));
		  set_slot_value (class,
				  obj,
				  SCM_CAR (get_n_set),
				  scm_eval_body (SCM_CDR (SCM_CODE (tmp)),
						 env));
		}
	    }
	}
    }
  
  return obj;
}


SCM_KEYWORD (k_class, "class");

SCM_PROC (s_sys_prep_layout_x, "%prep-layout!", 1, 0, 0, scm_sys_prep_layout_x);

SCM
scm_sys_prep_layout_x (SCM class)
{
  int i, n, len;
  char *s, p, a;
  SCM nfields, slots, type;

  SCM_ASSERT (SCM_NIMP (class) && SCM_INSTANCEP (class),
	      class,
	      SCM_ARG1,
	      s_sys_prep_layout_x);
  slots = SCM_SLOT (class, scm_si_slots);
  nfields = SCM_SLOT (class, scm_si_nfields);
  if (!SCM_INUMP (nfields) || SCM_INUM (nfields) < 0)
    scm_misc_error (s_sys_prep_layout_x,
		    scm_c_oldfmt0 ("bad value in nfields slot: ~S"),
		    SCM_LIST1 (nfields));
  n = 2 * SCM_INUM (nfields);
  if (n < sizeof (SCM_CLASS_CLASS_LAYOUT) - 1
      && SCM_SUBCLASSP (class, scm_class_class))
    scm_misc_error (s_sys_prep_layout_x,
		    scm_c_oldfmt0 ("class object doesn't have enough fields: ~S"),
		    SCM_LIST1 (nfields));
  
  s  = n > 0 ? scm_must_malloc (n, s_sys_prep_layout_x) : 0;
  for (i = 0; i < n; i += 2)
    {
      if (!(SCM_NIMP (slots) && SCM_CONSP (slots)))
	scm_misc_error (s_sys_prep_layout_x,
			"to few slot definitions",
			SCM_EOL);
      len = scm_ilength (SCM_CDAR (slots));
      type = scm_i_get_keyword (k_class, SCM_CDAR (slots), len, SCM_BOOL_F,
				s_sys_prep_layout_x);
      if (SCM_NIMP (type) && SCM_SUBCLASSP (type, scm_class_foreign_slot))
	{
	  if (SCM_SUBCLASSP (type, scm_class_self))
	    p = 's';
	  else if (SCM_SUBCLASSP (type, scm_class_protected))
	    p = 'p';
	  else
	    p = 'u';
	  
	  if (SCM_SUBCLASSP (type, scm_class_opaque))
	    a = 'o';
	  else if (SCM_SUBCLASSP (type, scm_class_read_only))
	    a = 'r';
	  else
	    a = 'w';
	}
      else
	{
	  p = 'p';
	  a = 'w';
	}
      s[i] = p;
      s[i + 1] = a;
      slots = SCM_CDR (slots);
    }
  SCM_SLOT (class, scm_si_layout) = SCM_CAR (scm_intern (s, n));
  if (s)
    scm_must_free (s);
  return SCM_UNSPECIFIED;
}

static void prep_hashsets (SCM);

SCM_PROC (s_sys_inherit_magic_x, "%inherit-magic!", 2, 0, 0, scm_sys_inherit_magic_x);

SCM
scm_sys_inherit_magic_x (SCM class, SCM dsupers)
{
  SCM ls = dsupers;
  long flags = 0;
  SCM_ASSERT (SCM_NIMP (class) && SCM_INSTANCEP (class),
	      class,
	      SCM_ARG1,
	      s_sys_inherit_magic_x);
  while (SCM_NNULLP (ls))
    {
      SCM_ASSERT (SCM_NIMP (ls)
		  && SCM_CONSP (ls)
		  && SCM_NIMP (SCM_CAR (ls))
		  && SCM_INSTANCEP (SCM_CAR (ls)),
		  dsupers,
		  SCM_ARG2,
		  s_sys_inherit_magic_x);
      flags |= SCM_CLASS_FLAGS (SCM_CAR (ls));
      ls = SCM_CDR (ls);
    }
  flags &= SCM_CLASSF_INHERIT;
  if (flags & SCM_CLASSF_ENTITY)
    SCM_SET_CLASS_DESTRUCTOR (class, scm_struct_free_entity);
  else
    {
      int n = SCM_INUM (SCM_SLOT (class, scm_si_nfields));
#if 0
      /*
       * We could avoid calling scm_must_malloc in the allocation code
       * (in which case the following two lines are needed).  Instead
       * we make 0-slot instances non-light, so that the light case
       * can be handled without special cases.
       */
      if (n == 0)
	SCM_SET_CLASS_DESTRUCTOR (class, scm_struct_free_0);
#endif
      if (n > 0 && !(flags & SCM_CLASSF_METACLASS))
	{
	  /* NOTE: The following depends on scm_struct_i_size. */
	  flags |= SCM_STRUCTF_LIGHT + n * sizeof (SCM); /* use light representation */
	  SCM_SET_CLASS_DESTRUCTOR (class, scm_struct_free_light);
	}
    }
  SCM_SET_CLASS_FLAGS (class, flags);

  prep_hashsets (class);
  
  return SCM_UNSPECIFIED;
}

void
prep_hashsets (SCM class)
{
  int i;

  for (i = 0; i < 7; ++i)
    SCM_SLOT (class, scm_si_hashsets + i)
      = SCM_PACK (scm_c_uniform32 (goops_rstate));
}

/******************************************************************************/

SCM
scm_basic_basic_make_class (SCM class, SCM name, SCM dsupers, SCM dslots)
{
  SCM z, cpl, slots, nfields, g_n_s;

  /* Allocate one instance */
  z = scm_make_struct (class, SCM_INUM0, SCM_EOL);

  /* Initialize its slots */
#if 0
  cpl   = compute_cpl (dsupers, SCM_LIST1(z));
#endif
  SCM_SLOT (z, scm_si_direct_supers) = dsupers;
  cpl   = compute_cpl (z);
  slots = build_slots_list (maplist (dslots), cpl);
  nfields = SCM_MAKINUM (scm_ilength (slots));
  g_n_s = compute_getters_n_setters (slots);

  SCM_SLOT(z, scm_si_name)	        = name;
  SCM_SLOT(z, scm_si_direct_slots)      = dslots;
  SCM_SLOT(z, scm_si_direct_subclasses) = SCM_EOL;
  SCM_SLOT(z, scm_si_direct_methods)    = SCM_EOL;
  SCM_SLOT(z, scm_si_cpl)               = cpl;
  SCM_SLOT(z, scm_si_slots)	        = slots;
  SCM_SLOT(z, scm_si_nfields)	        = nfields;
  SCM_SLOT(z, scm_si_getters_n_setters) = g_n_s;
  SCM_SLOT(z, scm_si_redefined)         = SCM_BOOL_F;
  SCM_SLOT(z, scm_si_environment)
    = scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE);

  /* Add this class in the direct-subclasses slot of dsupers */
  {
    SCM tmp;
    for (tmp = dsupers; SCM_NNULLP(tmp); tmp = SCM_CDR(tmp))
      SCM_SLOT(SCM_CAR(tmp), scm_si_direct_subclasses)
	= scm_cons(z, SCM_SLOT(SCM_CAR(tmp), scm_si_direct_subclasses));
  }

  /* Support for the underlying structs: */
  SCM_SET_CLASS_FLAGS (z, (class == scm_class_entity_class
			   ? (SCM_CLASSF_GOOPS_OR_VALID
			      | SCM_CLASSF_OPERATOR
			      | SCM_CLASSF_ENTITY)
			   : class == scm_class_operator_class
			   ? SCM_CLASSF_GOOPS_OR_VALID | SCM_CLASSF_OPERATOR
			   : SCM_CLASSF_GOOPS_OR_VALID));
  return z;
}

SCM
scm_basic_make_class (SCM class, SCM name, SCM dsupers, SCM dslots)
{
  SCM z = scm_basic_basic_make_class (class, name, dsupers, dslots);
  scm_sys_inherit_magic_x (z, dsupers);
  scm_sys_prep_layout_x (z);
  return z;
}

/******************************************************************************/

static SCM
build_class_class_slots ()
{
  return maplist (
         scm_cons (SCM_LIST3 (Intern ("layout"),
			      k_class,
			      scm_class_protected_read_only),
	 scm_cons (SCM_LIST3 (Intern ("vcell"),
			      k_class,
			      scm_class_opaque),
	 scm_cons (SCM_LIST3 (Intern ("vtable"),
			      k_class,
			      scm_class_self),
	 scm_cons (Intern ("print"),
	 scm_cons (SCM_LIST3 (Intern ("procedure"),
			      k_class,
			      scm_class_protected_opaque),
	 scm_cons (SCM_LIST3 (Intern ("setter"),
			      k_class,
			      scm_class_protected_opaque),
	 scm_cons (Intern ("redefined"),
	 scm_cons (SCM_LIST3 (Intern ("h0"),
			      k_class,
			      scm_class_int),
	 scm_cons (SCM_LIST3 (Intern ("h1"),
			      k_class,
			      scm_class_int),
	 scm_cons (SCM_LIST3 (Intern ("h2"),
			      k_class,
			      scm_class_int),
	 scm_cons (SCM_LIST3 (Intern ("h3"),
			      k_class,
			      scm_class_int),
	 scm_cons (SCM_LIST3 (Intern ("h4"),
			      k_class,
			      scm_class_int),
	 scm_cons (SCM_LIST3 (Intern ("h5"),
			      k_class,
			      scm_class_int),
	 scm_cons (SCM_LIST3 (Intern ("h6"),
			      k_class,
			      scm_class_int),
	 scm_cons (SCM_LIST3 (Intern ("h7"),
			      k_class,
			      scm_class_int),
	 scm_cons (Intern ("name"),
	 scm_cons (Intern ("direct-supers"),
	 scm_cons (Intern ("direct-slots"),
	 scm_cons (Intern ("direct-subclasses"),
	 scm_cons (Intern ("direct-methods"),
	 scm_cons (Intern ("cpl"),
	 scm_cons (Intern ("default-slot-definition-class"),
	 scm_cons (Intern ("slots"),
	 scm_cons (Intern ("getters-n-setters"), /* name-access */
	 scm_cons (Intern ("keyword-access"),
	 scm_cons (Intern ("nfields"),
	 scm_cons (Intern ("environment"),
	 SCM_EOL))))))))))))))))))))))))))));
}

static void
create_basic_classes (void)
{
  /* SCM slots_of_class = build_class_class_slots (); */

  /**** <scm_class_class> ****/
  SCM cs = scm_makfrom0str (SCM_CLASS_CLASS_LAYOUT
			    + 2 * scm_vtable_offset_user);
  SCM name = Intern ("<class>");
  scm_class_class = scm_permanent_object (scm_make_vtable_vtable (cs,
								  SCM_INUM0,
								  SCM_EOL));
  SCM_SET_CLASS_FLAGS (scm_class_class, (SCM_CLASSF_GOOPS_OR_VALID
					 | SCM_CLASSF_METACLASS));

  SCM_SLOT(scm_class_class, scm_si_name) 		 = name;
  SCM_SLOT(scm_class_class, scm_si_direct_supers)	 = SCM_EOL;  /* will be changed */
  /* SCM_SLOT(scm_class_class, scm_si_direct_slots) = slots_of_class; */
  SCM_SLOT(scm_class_class, scm_si_direct_subclasses)= SCM_EOL;
  SCM_SLOT(scm_class_class, scm_si_direct_methods) = SCM_EOL;  
  SCM_SLOT(scm_class_class, scm_si_cpl)		 = SCM_EOL;  /* will be changed */
  /* SCM_SLOT(scm_class_class, scm_si_slots) = slots_of_class; */
  SCM_SLOT(scm_class_class, scm_si_nfields)	 = SCM_MAKINUM (SCM_N_CLASS_SLOTS);
  /* SCM_SLOT(scm_class_class, scm_si_getters_n_setters)
     = compute_getters_n_setters (slots_of_class); */
  SCM_SLOT(scm_class_class, scm_si_redefined) 	 = SCM_BOOL_F;
  SCM_SLOT(scm_class_class, scm_si_environment)
    = scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE);

  prep_hashsets (scm_class_class);

  DEFVAR(name, scm_class_class);

  /**** <scm_class_top> ****/
  name = Intern ("<top>");
  scm_class_top = scm_permanent_object (scm_basic_make_class (scm_class_class,
						    name,
						    SCM_EOL,
						    SCM_EOL));

  DEFVAR(name, scm_class_top);
  
  /**** <scm_class_object> ****/
  name	 = Intern("<object>");
  scm_class_object = scm_permanent_object (scm_basic_make_class (scm_class_class,
						       name,
						       SCM_LIST1 (scm_class_top),
						       SCM_EOL));

  DEFVAR (name, scm_class_object);

  /* <top> <object> and <class> were partially initialized. Correct them here */
  SCM_SLOT (scm_class_object, scm_si_direct_subclasses) = SCM_LIST1 (scm_class_class);

  SCM_SLOT (scm_class_class, scm_si_direct_supers)      = SCM_LIST1 (scm_class_object);
  SCM_SLOT (scm_class_class, scm_si_cpl)		    = SCM_LIST3 (scm_class_class, scm_class_object, scm_class_top);
}

/******************************************************************************/

SCM_PROC (s_instance_p, "instance?", 1, 0, 0, scm_instance_p);

SCM
scm_instance_p (SCM obj)
{
  return SCM_NIMP (obj) && SCM_INSTANCEP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC (s_class_of, "class-of", 1, 0, 0, scm_class_of);
/* scm_class_of is defined in libguile */

/******************************************************************************
 * 
 * Meta object accessors
 *
 ******************************************************************************/
SCM_PROC (s_class_name, "class-name",  1, 0, 0, scm_class_name);

SCM
scm_class_name (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && CLASSP (obj), obj, SCM_ARG1, s_class_name);
  return scm_slot_ref (obj, Intern ("name"));
}

SCM_PROC (s_class_direct_supers, "class-direct-supers", 1, 0, 0, scm_class_direct_supers);

SCM
scm_class_direct_supers (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && CLASSP (obj), obj, SCM_ARG1, s_class_direct_supers);
  return scm_slot_ref (obj, Intern("direct-supers"));
}

SCM_PROC (s_class_direct_slots, "class-direct-slots", 1, 0, 0, scm_class_direct_slots);

SCM
scm_class_direct_slots (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && CLASSP (obj),
	      obj, SCM_ARG1, s_class_direct_slots);
  return scm_slot_ref (obj, Intern ("direct-slots"));
}

SCM_PROC (s_class_direct_subclasses, "class-direct-subclasses", 1, 0, 0, scm_class_direct_subclasses);

SCM
scm_class_direct_subclasses (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && CLASSP (obj),
	      obj, SCM_ARG1, s_class_direct_subclasses);
  return scm_slot_ref(obj, Intern ("direct-subclasses"));
}

SCM_PROC (s_class_direct_methods, "class-direct-methods", 1, 0, 0, scm_class_direct_methods);

SCM
scm_class_direct_methods (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && CLASSP (obj),
	      obj, SCM_ARG1, s_class_direct_methods);
  return scm_slot_ref (obj, Intern("direct-methods"));
}

SCM_PROC (s_class_direct_precedence_list, "class-precedence-list", 1, 0, 0, scm_class_precedence_list);

SCM
scm_class_precedence_list (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && CLASSP (obj),
	      obj, SCM_ARG1, s_class_direct_precedence_list);
  return scm_slot_ref (obj, Intern ("cpl"));
}

SCM_PROC (s_class_slots, "class-slots", 1, 0, 0, scm_class_slots);

SCM
scm_class_slots (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && CLASSP (obj),
	      obj, SCM_ARG1, s_class_slots);
  return scm_slot_ref (obj, Intern ("slots"));
}

SCM_PROC (s_class_environment, "class-environment", 1, 0, 0, scm_class_environment);

SCM
scm_class_environment (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && CLASSP (obj),
	      obj, SCM_ARG1, s_class_environment);
  return scm_slot_ref(obj, Intern ("environment"));
}


SCM_PROC (s_generic_function_name, "generic-function-name", 1, 0, 0, scm_generic_function_name);

SCM
scm_generic_function_name (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && GENERICP (obj),
	      obj, SCM_ARG1, s_generic_function_name);
  return scm_procedure_property (obj, scm_sym_name);
}

SCM_PROC (s_generic_function_methods, "generic-function-methods", 1, 0, 0, scm_generic_function_methods);

SCM
scm_generic_function_methods (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && GENERICP (obj),
	      obj, SCM_ARG1, s_generic_function_methods);
  return scm_slot_ref (obj, Intern ("methods"));
}


SCM_PROC (s_method_generic_function, "method-generic-function", 1, 0, 0, scm_method_generic_function);

SCM
scm_method_generic_function (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && METHODP (obj),
	      obj, SCM_ARG1, s_method_generic_function);
  return scm_slot_ref (obj, Intern ("generic-function"));
}

SCM_PROC (s_method_specializers, "method-specializers", 1, 0, 0, scm_method_specializers);

SCM
scm_method_specializers (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && METHODP (obj),
	      obj, SCM_ARG1, s_method_specializers);
  return scm_slot_ref (obj, Intern ("specializers"));
}

SCM_PROC (s_method_procedure, "method-procedure", 1, 0, 0, scm_method_procedure);

SCM
scm_method_procedure (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && METHODP (obj),
	      obj, SCM_ARG1, s_method_procedure);
  return scm_slot_ref (obj, Intern ("procedure"));
}

SCM_PROC (s_accessor_method_slot_definition, "accessor-method-slot-definition", 1, 0, 0, scm_accessor_method_slot_definition);

SCM
scm_accessor_method_slot_definition (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && SCM_ACCESSORP (obj),
	      obj, SCM_ARG1, s_method_procedure);
  return scm_slot_ref (obj, Intern ("slot-definition"));
}  

SCM_PROC (s_sys_tag_body, "%tag-body", 1, 0, 0, scm_sys_tag_body);

SCM
scm_sys_tag_body (SCM body)
{
  return scm_cons (SCM_IM_LAMBDA, body);
}

/******************************************************************************
 *
 * S l o t   a c c e s s
 *
 ******************************************************************************/

SCM_PROC (s_make_unbound, "make-unbound", 0, 0, 0, scm_make_unbound);

static SCM
scm_make_unbound ()
{
  return SCM_GOOPS_UNBOUND;
}

SCM_PROC (s_unbound_p, "unbound?", 1, 0, 0, scm_unbound_p);

static SCM
scm_unbound_p (SCM obj)
{
  return SCM_GOOPS_UNBOUNDP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC (s_assert_bound, "assert-bound", 2, 0, 0, scm_assert_bound);

static SCM
scm_assert_bound (SCM value, SCM obj)
{
  if (SCM_GOOPS_UNBOUNDP (value))
    return CALL_GF1 ("slot-unbound", obj);
  return value;
}

SCM_PROC (s_at_assert_bound_ref, "@assert-bound-ref", 2, 0, 0, scm_at_assert_bound_ref);

static SCM
scm_at_assert_bound_ref (SCM obj, SCM index)
{
  SCM value = SCM_SLOT (obj, SCM_INUM (index));
  if (SCM_GOOPS_UNBOUNDP (value))
    return CALL_GF1 ("slot-unbound", obj);
  return value;
}

SCM_PROC (s_sys_fast_slot_ref, "%fast-slot-ref", 2, 0, 0, scm_sys_fast_slot_ref);

SCM
scm_sys_fast_slot_ref (SCM obj, SCM index)
{
  register long i;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_sys_fast_slot_ref);
  SCM_ASSERT (SCM_INUMP (index), index, SCM_ARG2, s_sys_fast_slot_ref);
  i = SCM_INUM (index);
  SCM_ASSERT (i >= 0 && i < SCM_NUMBER_OF_SLOTS (obj),
	      index, SCM_OUTOFRANGE, s_sys_fast_slot_ref);
  return scm_at_assert_bound_ref (obj, index);
}

SCM_PROC (s_sys_fast_slot_set_x, "%fast-slot-set!", 3, 0, 0, scm_sys_fast_slot_set_x);

SCM
scm_sys_fast_slot_set_x (SCM obj, SCM index, SCM value)
{
  register long i;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_sys_fast_slot_set_x);
  SCM_ASSERT (SCM_INUMP (index), index, SCM_ARG2, s_sys_fast_slot_set_x);
  i = SCM_INUM (index);
  SCM_ASSERT (i >= 0 && i < SCM_NUMBER_OF_SLOTS (obj),
	      index, SCM_OUTOFRANGE, s_sys_fast_slot_set_x);

  SCM_SLOT (obj, i) = value;
  return SCM_UNSPECIFIED;
}

/** Utilities **/

/* In the future, this function will return the effective slot
 * definition associated with SLOT_NAME.  Now it just returns some of
 * the information which will be stored in the effective slot
 * definition.
 */

static SCM
slot_definition_using_name (SCM class, SCM slot_name)
{
  register SCM slots = SCM_SLOT (class, scm_si_getters_n_setters);
  for (; SCM_NIMP (slots); slots = SCM_CDR (slots))
    if (SCM_CAAR (slots) == slot_name)
      return SCM_CAR (slots);
  return SCM_BOOL_F;
}

static SCM
get_slot_value (SCM class, SCM obj, SCM slotdef)
{
  SCM access = SCM_CDDR (slotdef);
  /* Two cases here:
   *	- access is an integer (the offset of this slot in the slots vector)
   *	- otherwise (car access) is the getter function to apply
   */
  if (SCM_INUMP (access))
    return SCM_SLOT (obj, SCM_INUM (access));
  else
    {
      /* We must evaluate (apply (car access) (list obj)) 
       * where (car access) is known to be a closure of arity 1  */
      register SCM code, env;

      code = SCM_CAR (access);
      if (!SCM_CLOSUREP (code))
	return SCM_SUBRF (code) (obj);
      env  = SCM_EXTEND_ENV (SCM_CAR (SCM_CODE (code)),
			     SCM_LIST1 (obj),
			     SCM_ENV (code));
      /* Evaluate the closure body */
      return scm_eval_body (SCM_CDR (SCM_CODE (code)), env);
    }
}

static SCM
get_slot_value_using_name (SCM class, SCM obj, SCM slot_name)
{
  SCM slotdef = slot_definition_using_name (class, slot_name);
  if (SCM_NFALSEP (slotdef))
    return get_slot_value (class, obj, slotdef);
  else
    return CALL_GF3 ("slot-missing", class, obj, slot_name);
}

static SCM
set_slot_value (SCM class, SCM obj, SCM slotdef, SCM value)
{
  SCM access = SCM_CDDR (slotdef);
  /* Two cases here:
   *	- access is an integer (the offset of this slot in the slots vector)
   *	- otherwise (cadr access) is the setter function to apply
   */
  if (SCM_INUMP (access))
    SCM_SLOT (obj, SCM_INUM (access)) = value;
  else
    {
      /* We must evaluate (apply (cadr l) (list obj value))
       * where (cadr l) is known to be a closure of arity 2  */
      register SCM code, env;

      code = SCM_CADR (access);
      if (!SCM_CLOSUREP (code))
	SCM_SUBRF (code) (obj, value);
      else
	{
	  env  = SCM_EXTEND_ENV (SCM_CAR (SCM_CODE (code)),
				 SCM_LIST2 (obj, value),
				 SCM_ENV (code));
	  /* Evaluate the closure body */
	  scm_eval_body (SCM_CDR (SCM_CODE (code)), env);
	}
    }
  return SCM_UNSPECIFIED;
}

static SCM
set_slot_value_using_name (SCM class, SCM obj, SCM slot_name, SCM value)
{
  SCM slotdef = slot_definition_using_name (class, slot_name);
  if (SCM_NFALSEP (slotdef))
    return set_slot_value (class, obj, slotdef, value);
  else
    return CALL_GF4 ("slot-missing", class, obj, slot_name, value);
}

static SCM
test_slot_existence (SCM class, SCM obj, SCM slot_name)
{
  register SCM l;

  for (l = SCM_ACCESSORS_OF (obj); SCM_NNULLP (l); l = SCM_CDR (l))
    if (SCM_CAAR (l) == slot_name)
      return SCM_BOOL_T;

  return SCM_BOOL_F;
}

/* The current libguile logand doesn't handle bignums.
 * This (primitive) version handles them up to 32 bits.
 */

SCM_PROC1 (s_sys_logand, "%logand", scm_tc7_asubr, scm_sys_logand);

static unsigned long
scm_sloppy_num2ulong (SCM num, char *pos, const char *s_caller)
{
  unsigned long res;

  if (SCM_INUMP (num))
    {
      if (SCM_INUM (num) < 0)
	goto out_of_range;
      res = SCM_INUM (num);
      return res;
    }
  SCM_ASRTGO (SCM_NIMP (num), wrong_type_arg);
  if (SCM_BIGP (num))
    {
      scm_sizet l;

      res = 0;
      for (l = SCM_NUMDIGS (num); l--;)
	res = SCM_BIGUP (res) + SCM_BDIGITS (num)[l];
      return res;
    }
 wrong_type_arg:
  scm_wrong_type_arg (s_caller, (int) pos, num);
 out_of_range:
  scm_out_of_range (s_caller, num);
}

static SCM
scm_sys_logand (SCM n1, SCM n2)
{
  if (SCM_UNBNDP (n2))
    {
      if (SCM_UNBNDP (n1))
	return SCM_MAKINUM (-1);
      return n1;
    }
  {
    unsigned long u1 = scm_sloppy_num2ulong (n1, (char *) 1, s_sys_logand);
    unsigned long u2 = scm_sloppy_num2ulong (n2, (char *) 2, s_sys_logand);
    return scm_ulong2num (u1 & u2);
  }
}

/* These are needed in save.scm. */
#ifndef HAVE_SCM_SHARED_ARRAY_ROOT

SCM_PROC (s_shared_array_root, "shared-array-root", 1, 0, 0, scm_shared_array_root);

SCM
scm_shared_array_root (SCM ra)
{
  SCM_ASSERT (SCM_NIMP (ra) && SCM_ARRAYP (ra), ra, SCM_ARG1, s_shared_array_root);
  return SCM_ARRAY_V (ra);
}


SCM_PROC (s_shared_array_offset, "shared-array-offset", 1, 0, 0, scm_shared_array_offset);

SCM
scm_shared_array_offset (SCM ra)
{
  SCM_ASSERT (SCM_NIMP (ra) && SCM_ARRAYP (ra), ra, SCM_ARG1, s_shared_array_offset);
  return SCM_MAKINUM (SCM_ARRAY_BASE (ra));
}


SCM_PROC (s_shared_array_increments, "shared-array-increments", 1, 0, 0, scm_shared_array_increments);

SCM
scm_shared_array_increments (SCM ra)
{
  SCM res = SCM_EOL;
  scm_sizet k;
  scm_array_dim *s;
  SCM_ASSERT (SCM_NIMP (ra) && SCM_ARRAYP (ra), ra, SCM_ARG1, s_shared_array_increments);
  k = SCM_ARRAY_NDIM (ra);
  s = SCM_ARRAY_DIMS (ra);
  while (k--)
    res = scm_cons (SCM_MAKINUM (s[k].inc), res);
  return res;
}

#endif /* HAVE_SCM_SHARED_ARRAY_ROOT */

		/* ======================================== */

SCM_PROC (s_slot_ref_using_class, "slot-ref-using-class", 3, 0, 0, scm_slot_ref_using_class);

SCM
scm_slot_ref_using_class (SCM class, SCM obj, SCM slot_name)
{
  SCM res;

  SCM_ASSERT (SCM_NIMP (class) && CLASSP (class),
	      class, SCM_ARG1, s_slot_ref_using_class);
  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_slot_ref_using_class);
  SCM_ASSERT (SCM_NIMP (slot_name) && SCM_SYMBOLP (slot_name),
	      obj, SCM_ARG3, s_slot_ref_using_class);

  res = get_slot_value_using_name (class, obj, slot_name);
  if (SCM_GOOPS_UNBOUNDP (res))
    return CALL_GF3 ("slot-unbound", class, obj, slot_name);
  return res;
}
 
SCM_PROC (s_slot_set_using_class_x, "slot-set-using-class!", 4, 0, 0, scm_slot_set_using_class_x);

SCM
scm_slot_set_using_class_x (SCM class, SCM obj, SCM slot_name, SCM value)
{
  SCM_ASSERT (SCM_NIMP (class) && CLASSP (class),
	      class, SCM_ARG1, s_slot_set_using_class_x);
  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG2, s_slot_set_using_class_x);
  SCM_ASSERT (SCM_NIMP (slot_name) && SCM_SYMBOLP (slot_name),
	      obj, SCM_ARG3, s_slot_set_using_class_x);
  return set_slot_value_using_name (class, obj, slot_name, value);
}

SCM_PROC (s_slot_bound_using_class_p, "slot-bound-using-class?", 3, 0, 0, scm_slot_bound_using_class_p);

SCM
scm_slot_bound_using_class_p (SCM class, SCM obj, SCM slot_name)
{
  SCM_ASSERT (SCM_NIMP (class) && CLASSP (class),
	      class, SCM_ARG1, s_slot_bound_using_class_p);
  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG2, s_slot_bound_using_class_p);
  SCM_ASSERT (SCM_NIMP (slot_name) && SCM_SYMBOLP (slot_name),
	      obj, SCM_ARG3, s_slot_bound_using_class_p);

  return (SCM_GOOPS_UNBOUNDP (get_slot_value_using_name (class, obj, slot_name))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}

SCM_PROC (s_slot_exists_using_class_p, "slot-exists-using-class?", 3, 0, 0, scm_slot_exists_using_class_p);

SCM
scm_slot_exists_using_class_p (SCM class, SCM obj, SCM slot_name)
{
  SCM_ASSERT (SCM_NIMP (class) && CLASSP (class),
	      class, SCM_ARG1, s_slot_exists_using_class_p);
  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG2, s_slot_exists_using_class_p);
  SCM_ASSERT (SCM_NIMP (slot_name) && SCM_SYMBOLP (slot_name),
	      obj, SCM_ARG3, s_slot_exists_using_class_p);
  return test_slot_existence (class, obj, slot_name);
}


		/* ======================================== */

SCM_PROC (s_slot_ref, "slot-ref", 2, 0, 0, scm_slot_ref);

SCM
scm_slot_ref (SCM obj, SCM slot_name)
{
  SCM res, class;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_slot_ref);
  TEST_CHANGE_CLASS (obj, class);

  res = get_slot_value_using_name (class, obj, slot_name);
  if (SCM_GOOPS_UNBOUNDP (res))
    return CALL_GF3 ("slot-unbound", class, obj, slot_name);
  return res;
}

SCM_PROC (s_slot_set_x, "slot-set!", 3, 0, 0, scm_slot_set_x);

const char *scm_s_slot_set_x = s_slot_set_x;

SCM
scm_slot_set_x (SCM obj, SCM slot_name, SCM value)
{
  SCM class;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_slot_set_x);
  TEST_CHANGE_CLASS(obj, class);

  return set_slot_value_using_name (class, obj, slot_name, value);
}

SCM_PROC (s_slot_bound_p, "slot-bound?", 2, 0, 0, scm_slot_bound_p);

SCM
scm_slot_bound_p (SCM obj, SCM slot_name)
{
  SCM class;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_slot_bound_p);
  TEST_CHANGE_CLASS(obj, class);

  return (SCM_GOOPS_UNBOUNDP (get_slot_value_using_name (class,
							 obj,
							 slot_name))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}

SCM_PROC (s_slot_exists_p, "slot-exists?", 2, 0, 0, scm_slots_exists_p);

SCM
scm_slots_exists_p (SCM obj, SCM slot_name)
{
  SCM class;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_slot_exists_p);
  SCM_ASSERT (SCM_NIMP (slot_name) && SCM_SYMBOLP (slot_name),
	      slot_name, SCM_ARG2, s_slot_exists_p);
  TEST_CHANGE_CLASS (obj, class);

  return test_slot_existence (class, obj, slot_name);
}


/******************************************************************************
 *
 * %allocate-instance (the low level instance allocation primitive)
 *
 ******************************************************************************/

static void clear_method_cache (SCM);

static SCM
wrap_init (SCM class, SCM *m, int n)
{
  SCM z;
  int i;
  
  /* Set all slots to unbound */
  for (i = 0; i < n; i++)
    m[i] = SCM_GOOPS_UNBOUND;

#ifdef SCM_STRUCT_GC_CHAIN
  SCM_NEWCELL2 (z);
#else
  SCM_NEWCELL (z);
#endif
  SCM_SETCDR (z, (SCM) m);
#ifdef SCM_STRUCT_GC_CHAIN
  SCM_SET_STRUCT_GC_CHAIN (z, 0);
#endif
  SCM_SETCAR (z, (scm_bits_t) SCM_STRUCT_DATA (class) | scm_tc3_cons_gloc);

  return z;
}

SCM_PROC (s_sys_allocate_instance, "%allocate-instance", 2, 0, 0, scm_sys_allocate_instance);

SCM
scm_sys_allocate_instance (SCM class, SCM initargs)
{
  SCM *m;
  int n;

  SCM_ASSERT (SCM_NIMP (class) && CLASSP (class),
	      class, SCM_ARG1, s_sys_allocate_instance);

  /* Most instances */
  if (SCM_CLASS_FLAGS (class) & SCM_STRUCTF_LIGHT)
    {
      n = SCM_INUM (SCM_SLOT (class, scm_si_nfields));
      m = (SCM *) scm_must_malloc (n * sizeof (SCM), "instance");
      return wrap_init (class, m, n);
    }
  
  /* Foreign objects */
  if (SCM_CLASS_FLAGS (class) & SCM_CLASSF_FOREIGN)
    return scm_make_foreign_object (class, initargs);

  n = SCM_INUM (SCM_SLOT (class, scm_si_nfields));
  
  /* Entities */
  if (SCM_CLASS_FLAGS (class) & SCM_CLASSF_ENTITY)
    {
      m = (SCM *) scm_alloc_struct (n,
				    scm_struct_entity_n_extra_words,
				    "entity");
      m[scm_struct_i_setter] = SCM_BOOL_F;
      m[scm_struct_i_procedure] = SCM_BOOL_F;
      /* Generic functions */
      if (SCM_CLASS_FLAGS (class) & SCM_CLASSF_PURE_GENERIC)
	{
	  SCM gf = wrap_init (class, m, n);
	  clear_method_cache (gf);
	  return gf;
	}
      else
	return wrap_init (class, m, n);
    }
  
  /* Class objects */
  if (SCM_CLASS_FLAGS (class) & SCM_CLASSF_METACLASS)
    {
      int i;

      /* allocate class object */
      SCM z = scm_make_struct (class, SCM_INUM0, SCM_EOL);

      SCM_SLOT (z, scm_si_print) = SCM_GOOPS_UNBOUND;
      for (i = scm_si_goops_fields; i < n; i++)
	SCM_SLOT (z, i) = SCM_GOOPS_UNBOUND;

      if (SCM_SUBCLASSP (class, scm_class_entity_class))
	SCM_SET_CLASS_FLAGS (z, SCM_CLASSF_OPERATOR | SCM_CLASSF_ENTITY);
      else if (SCM_SUBCLASSP (class, scm_class_operator_class))
	SCM_SET_CLASS_FLAGS (z, SCM_CLASSF_OPERATOR);

      return z;
    }
  
  /* Non-light instances */
  {
    m = (SCM *) scm_alloc_struct (n,
				  scm_struct_n_extra_words,
				  "heavy instance");
    return wrap_init (class, m, n);
  }
}

SCM_PROC (s_sys_set_object_setter_x, "%set-object-setter!", 2, 0, 0, scm_sys_set_object_setter_x);

SCM
scm_sys_set_object_setter_x (SCM obj, SCM setter)
{
  SCM_ASSERT (SCM_NIMP (obj) && SCM_STRUCTP (obj)
	      && ((SCM_CLASS_FLAGS (obj) & SCM_CLASSF_OPERATOR)
		  || SCM_I_ENTITYP (obj)),
	      obj,
	      SCM_ARG1,
	      s_sys_set_object_setter_x);
  if (SCM_I_ENTITYP (obj))
    SCM_ENTITY_SETTER (obj) = setter;
  else
    SCM_OPERATOR_CLASS (obj)->setter = setter;
  return SCM_UNSPECIFIED;
}

/******************************************************************************
 *
 * %modify-instance (used by change-class to modify in place)
 * 
 ******************************************************************************/

SCM_PROC (s_sys_modify_instance, "%modify-instance", 2, 0, 0, scm_sys_modify_instance);

SCM
scm_sys_modify_instance (SCM old, SCM new)
{
  SCM_ASSERT (SCM_NIMP (old) && SCM_INSTANCEP (old),
	      old, SCM_ARG1, s_sys_modify_instance);
  SCM_ASSERT (SCM_NIMP (new) && SCM_INSTANCEP (new),
	      new, SCM_ARG2, s_sys_modify_instance);

  /* Exchange the data contained in old and new. We exchange rather than 
   * scratch the old value with new to be correct with GC.
   * See "Class redefinition protocol above".
   */
  SCM_REDEFER_INTS;
  {
    SCM car = SCM_CAR (old);
    SCM cdr = SCM_CDR (old);
    SCM_SETCAR (old, SCM_CAR (new));
    SCM_SETCDR (old, SCM_CDR (new));
    SCM_SETCAR (new, car);
    SCM_SETCDR (new, cdr);
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_sys_modify_class, "%modify-class", 2, 0, 0, scm_sys_modify_class);

SCM
scm_sys_modify_class (SCM old, SCM new)
{
  SCM_ASSERT (SCM_NIMP (old) && CLASSP (old),
	      old, SCM_ARG1, s_sys_modify_class);
  SCM_ASSERT (SCM_NIMP (new) && CLASSP (new),
	      new, SCM_ARG2, s_sys_modify_class);

  SCM_REDEFER_INTS;
  {
    SCM car = SCM_CAR (old);
    SCM cdr = SCM_CDR (old);
    SCM_SETCAR (old, SCM_CAR (new));
    SCM_SETCDR (old, SCM_CDR (new));
    SCM_STRUCT_DATA (old)[scm_vtable_index_vtable] = old;
    SCM_SETCAR (new, car);
    SCM_SETCDR (new, cdr);
    SCM_STRUCT_DATA (new)[scm_vtable_index_vtable] = new;
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_sys_invalidate_class, "%invalidate-class", 1, 0, 0, scm_sys_invalidate_class);

SCM
scm_sys_invalidate_class (SCM class)
{
  SCM_ASSERT (SCM_NIMP (class) && CLASSP (class),
	      class, SCM_ARG1, s_sys_invalidate_class);

  SCM_CLEAR_CLASS_FLAGS (class, SCM_CLASSF_GOOPS_VALID);
  return SCM_UNSPECIFIED;
}

/* When instances change class, they finally get a new body, but
 * before that, they go through purgatory in hell.  Odd as it may
 * seem, this data structure saves us from eternal suffering in
 * infinite recursions.
 */

static SCM **hell;
static int n_hell = 1;		/* one place for the evil one himself */
static int hell_size = 4;
#ifdef USE_THREADS
static scm_mutex_t hell_mutex;
#endif

static int
burnin (SCM o)
{
  int i;
  for (i = 1; i < n_hell; ++i)
    if (SCM_INST (o) == hell[i])
      return i;
  return 0;
}

static void
go_to_hell (void *o)
{
  SCM obj = (SCM) o;
#ifdef USE_THREADS
  scm_mutex_lock (&hell_mutex);
#endif
  if (n_hell == hell_size)
    {
      int new_size = 2 * hell_size;
      hell = scm_must_realloc (hell, hell_size, new_size, "hell");
      hell_size = new_size;
    }
  hell[n_hell++] = SCM_INST (obj);
#ifdef USE_THREADS
  scm_mutex_unlock (&hell_mutex);
#endif
}

static void
go_to_heaven (void *o)
{
#ifdef USE_THREADS
  scm_mutex_lock (&hell_mutex);
#endif
  hell[burnin ((SCM) o)] = hell[--n_hell];
#ifdef USE_THREADS
  scm_mutex_unlock (&hell_mutex);
#endif
}

static SCM
purgatory (void *args)
{
  return scm_apply (GETVAR (Intern ("change-class")), (SCM) args, SCM_EOL);
}

static void
change_class (obj, old_class, new_class)
{
  if (!burnin (obj))
    scm_internal_dynamic_wind (go_to_hell, purgatory, go_to_heaven,
			       (void *) SCM_LIST2 (obj, new_class),
			       (void *) obj);
}

/******************************************************************************
 *
 *   GGGG                FFFFF          
 *  G                    F    
 *  G  GG                FFF    
 *  G   G                F      
 *   GGG  E N E R I C    F    U N C T I O N S
 *
 * This implementation provides
 *	- generic functions (with class specializers)
 *	- multi-methods
 *	- next-method 
 *	- a hard-coded MOP for standard gf, which can be overloaded for non-std gf
 *
 ******************************************************************************/

SCM_KEYWORD (k_name, "name");

SCM_SYMBOL (sym_no_method, "no-method");

static SCM list_of_no_method;

SCM_SYMBOL (scm_sym_args, "args");

SCM
scm_make_method_cache (SCM gf)
{
  return SCM_LIST5 (SCM_IM_DISPATCH, scm_sym_args, SCM_MAKINUM (1),
		    scm_make_vector (SCM_MAKINUM (SCM_INITIAL_MCACHE_SIZE),
				     list_of_no_method),
		    gf);
}

static void
clear_method_cache (SCM gf)
{
  SCM_ENTITY_PROCEDURE (gf) = scm_make_method_cache (gf);
  SCM_SLOT (gf, scm_si_used_by) = SCM_BOOL_F;
}

SCM_PROC (s_sys_invalidate_method_cache_x, "%invalidate-method-cache!", 1, 0, 0, scm_sys_invalidate_method_cache_x);

SCM
scm_sys_invalidate_method_cache_x (SCM gf)
{
  SCM used_by;
  SCM_ASSERT (SCM_NIMP (gf) && SCM_STRUCTP (gf) && SCM_PUREGENERICP (gf),
	      gf, SCM_ARG1, s_sys_invalidate_method_cache_x);
  used_by = SCM_SLOT (gf, scm_si_used_by);
  if (SCM_NFALSEP (used_by))
    {
      SCM methods = SCM_SLOT (gf, scm_si_methods);
      for (; SCM_NIMP (used_by) && SCM_CONSP (used_by);
	   used_by = SCM_CDR (used_by))
	scm_sys_invalidate_method_cache_x (SCM_CAR (used_by));
      clear_method_cache (gf);
      for (; SCM_NIMP (methods) && SCM_CONSP (methods);
	   methods = SCM_CDR (methods))
	SCM_SLOT (SCM_CAR (methods), scm_si_code_table) = SCM_EOL;
    }
  {
    SCM n = SCM_SLOT (gf, scm_si_n_specialized);
    /* The sign of n is a flag indicating rest args. */
    
    /* In the separately packaged GOOPS, n-specialized in the cache
     * is n-specialized in the gf + 1.  This is a workaround for a bug
     * in the old SCM_IM_DISPATCH code in eval.c
     */
    SCM_SET_MCACHE_N_SPECIALIZED (SCM_ENTITY_PROCEDURE (gf),
				  SCM_MAKINUM (SCM_INUM (n) + 1));
  }
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_generic_capability_p, "generic-capability?", 1, 0, 0, scm_generic_capability_p);

SCM
scm_generic_capability_p (SCM proc)
{
  SCM_ASSERT (SCM_NFALSEP (scm_procedure_p (proc)),
	      proc, SCM_ARG1, s_generic_capability_p);
  return (scm_subr_p (proc) && SCM_SUBR_GENERIC (proc)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

SCM_PROC (s_enable_primitive_generic_x, "enable-primitive-generic!", 0, 0, 1, scm_enable_primitive_generic_x);

SCM
scm_enable_primitive_generic_x (SCM subrs)
{
  while (SCM_NIMP (subrs))
    {
      SCM subr = SCM_CAR (subrs);
      SCM_ASSERT (scm_subr_p (subr) && SCM_SUBR_GENERIC (subr),
		  subr, SCM_ARGn, s_enable_primitive_generic_x);
      *SCM_SUBR_GENERIC (subr)
	= scm_make (SCM_LIST3 (scm_class_generic,
			       k_name,
			       SCM_SNAME (subr)));
      subrs = SCM_CDR (subrs);
    }
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_primitive_generic_generic, "primitive-generic-generic", 1, 0, 0, scm_primitive_generic_generic);

SCM
scm_primitive_generic_generic (SCM subr)
{
  if (scm_subr_p (subr) && SCM_SUBR_GENERIC (subr))
    {
      SCM gf = *SCM_SUBR_GENERIC (subr);
      if (gf)
	return gf;
    }
  return scm_wta (subr, (char *) SCM_ARG1, s_primitive_generic_generic);
}

/******************************************************************************
 * 
 * Protocol for calling a generic fumction
 * This protocol is roughly equivalent to (parameter are a little bit different 
 * for efficiency reasons):
 *
 * 	+ apply-generic (gf args)
 *		+ compute-applicable-methods (gf args ...)
 *			+ sort-applicable-methods (methods args)
 *		+ apply-methods (gf methods args)
 *				
 * apply-methods calls make-next-method to build the "continuation" of a a 
 * method.  Applying a next-method will call apply-next-method which in
 * turn will call  apply again to call effectively the following method.
 *
 ******************************************************************************/

static int
applicablep (SCM actual, SCM formal)
{
  register SCM ptr;

  /* We test that (memq formal (slot-ref actual 'cpl))
   * However, we don't call memq here since we already know that
   * the list is well formed 
   */
  for (ptr=SCM_SLOT(actual, scm_si_cpl); SCM_NNULLP(ptr); ptr = SCM_CDR(ptr)) { 
    if (SCM_NIMP (ptr) && SCM_CONSP (ptr)) {
      if (SCM_CAR (ptr) == formal)
	return 1;
    }
    else 
      scm_misc_error (0,
		      scm_c_oldfmt0 ("Internal error in applicable: bad list ~S"),
		      SCM_LIST1 (actual));
  }
  return 0;
}

static int
more_specificp (SCM m1, SCM m2, SCM *targs)
{
  register SCM s1, s2;
  register int i;
  /* 
   * Note: 
   *   m1 and m2 can have != length (i.e. one can be one element longer than the 
   * other when we have a dotted parameter list). For instance, with the call
   *   (M 1)
   * with
   *   (define-method M (a . l) ....)
   *   (define-method M (a) ....) 
   *
   * we consider that the second method is more specific.
   *
   * BTW, targs is an array of types. We don't need it's size since
   * we already know that m1 and m2 are applicable (no risk to go past
   * the end of this array).
   *
   */
  for (i=0,s1=SPEC_OF(m1),s2=SPEC_OF(m2); ; i++,s1=SCM_CDR(s1),s2=SCM_CDR(s2)) {
    if (SCM_NULLP(s1)) return 1;
    if (SCM_NULLP(s2)) return 0;
    if (SCM_CAR(s1) != SCM_CAR(s2)) {
      register SCM l, cs1 = SCM_CAR(s1), cs2 = SCM_CAR(s2);
      
      for (l = SCM_SLOT(targs[i], scm_si_cpl);   ; l = SCM_CDR(l)) {
	if (cs1 == SCM_CAR(l))
	  return 1;
	if (cs2 == SCM_CAR(l))
	  return 0;
      }
      return 0;/* should not occur! */
    }
  }
  return 0; /* should not occur! */
}

#define BUFFSIZE 32		/* big enough for most uses */

static SCM
scm_i_vector2list (SCM l, int len)
{
  int j;
  SCM z = scm_make_vector (SCM_MAKINUM (len), SCM_UNDEFINED);
  
  for (j = 0; j < len; j++, l = SCM_CDR (l)) {
    SCM_VELTS (z)[j] = SCM_CAR (l);
  }
  return z;
}

static SCM
sort_applicable_methods (SCM method_list, int size, SCM *targs)
{
  int i, j, incr;
  SCM *v, vector = SCM_EOL;
  SCM buffer[BUFFSIZE];
  SCM save = method_list;

  /* For reasonably sized method_lists we can try to avoid all the
   * consing and reorder the list in place...
   * This idea is due to David McClain <Dave_McClain@msn.com>
   */
  if (size <= BUFFSIZE)
    {
      for (i = 0;  i < size; i++)
	{
	  buffer[i]   = SCM_CAR (method_list);
	  method_list = SCM_CDR (method_list);
	}
      v = buffer;
    } 
  else
    {
      /* Too many elements in method_list to keep everything locally */
      vector = scm_i_vector2list (save, size);
      v      = SCM_VELTS (vector);
    }

  /* Use a simple shell sort since it is generally faster than qsort on 
   * small vectors (which is probably mostly the case when we have to
   * sort a list of applicable methods).
   */
  for (incr = size / 2; incr; incr /= 2)
    {
      for (i = incr; i < size; i++)
	{
	  for (j = i - incr; j >= 0; j -= incr)
	    {
	      if (more_specificp (v[j], v[j+incr], targs))
		break;
	      else
		{
		  SCM tmp = v[j + incr];
		  v[j + incr] = v[j];
		  v[j] = tmp;
		}
	    }
	}
    }

  if (size <= BUFFSIZE)
    {
      /* We did it in locally, so restore the original list (reordered) in-place */
      for (i = 0, method_list = save; i < size; i++, v++)
	{
	  SCM_SETCAR (method_list, *v);
	  method_list = SCM_CDR (method_list);
	}
      return save;
    }
  /* If we are here, that's that we did it the hard way... */ 
  return scm_vector_to_list (vector);
}

SCM
scm_compute_applicable_methods (SCM gf, SCM args, int len, int find_method_p)
{
  register int i;
  int count = 0;
  SCM l, fl, applicable = SCM_EOL;
  SCM save = args;
  SCM buffer[BUFFSIZE], *types, *p;
  SCM tmp;
 
  /* Build the list of arguments types */
  if (len >= BUFFSIZE) {
    tmp   = scm_make_vector (SCM_MAKINUM (len), SCM_UNDEFINED);
    /* NOTE: Using pointers to malloced memory won't work if we
       1. have preemtive threading, and,
       2. have a GC which moves objects.  */
    types = p = SCM_VELTS(tmp);
  }
  else
    types = p = buffer;
  
  for (  ; SCM_NNULLP (args); args = SCM_CDR (args)) 
    *p++ = scm_class_of (SCM_CAR (args));
 
  /* Build a list of all applicable methods */
  for (l = SCM_SLOT (gf, scm_si_methods); SCM_NNULLP (l); l = SCM_CDR (l))
    {
      fl = SPEC_OF (SCM_CAR (l));
      /* Only accept accessors which match exactly in first arg. */
      if (SCM_ACCESSORP (SCM_CAR (l))
	  && (SCM_IMP (fl) || types[0] != SCM_CAR (fl)))
	continue;
      for (i = 0; ; i++, fl = SCM_CDR (fl))
	{
	  if ((SCM_NIMP (fl) && SCM_INSTANCEP (fl))
	      /* We have a dotted argument list */
	      || (i >= len && SCM_NULLP (fl)))
	    {	/* both list exhausted */
	      applicable = scm_cons (SCM_CAR (l), applicable);
	      count     += 1;
	      break;
	    }
	  if (i >= len
	      || SCM_NULLP (fl)
	      || !applicablep (types[i], SCM_CAR (fl)))
	    break;
	}
    }

  if (count == 0)
    {
      if (find_method_p)
	return SCM_BOOL_F;
      CALL_GF2 ("no-applicable-method", gf, save);
      /* if we are here, it's because no-applicable-method hasn't signaled an error */
      return SCM_BOOL_F;
    }
  return (count == 1
	  ? applicable
	  : sort_applicable_methods (applicable, count, types));
}

#if 0
SCM_PROC (s_sys_compute_applicable_methods, "%compute-applicable-methods", 2, 0, 0, scm_sys_compute_applicable_methods);
#endif

static const char s_sys_compute_applicable_methods[] = "%compute-applicable-methods";

SCM
scm_sys_compute_applicable_methods (SCM gf, SCM args)
{
  int n;
  SCM_ASSERT (SCM_NIMP (gf) && GENERICP (gf),
	      gf, SCM_ARG1, s_sys_compute_applicable_methods);
  n = scm_ilength (args);
  SCM_ASSERT (n >= 0, args, SCM_ARG2, s_sys_compute_applicable_methods);
  return scm_compute_applicable_methods (gf, args, n, 1);
}

SCM_VCELL_INIT (var_compute_applicable_methods, "compute-applicable-methods", scm_make_gsubr (s_sys_compute_applicable_methods, 2, 0, 0, scm_sys_compute_applicable_methods));

SCM_SYNTAX (s_atslot_ref, "@slot-ref", scm_makmmacro, scm_m_atslot_ref);

SCM
scm_m_atslot_ref (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (scm_ilength (x) == 2, xorig, scm_s_expression, s_atslot_ref);
  SCM_ASSYNT (SCM_INUMP (SCM_CADR (x)), SCM_CADR (x), SCM_ARG2, s_atslot_ref);
  return scm_cons (SCM_IM_SLOT_REF, x);
}

SCM_SYNTAX (s_atslot_set_x, "@slot-set!", scm_makmmacro, scm_m_atslot_set_x);

SCM
scm_m_atslot_set_x (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (scm_ilength (x) == 3, xorig, scm_s_expression, s_atslot_set_x);
  SCM_ASSYNT (SCM_INUMP (SCM_CADR (x)), SCM_CADR (x), SCM_ARG2, s_atslot_set_x);
  return scm_cons (SCM_IM_SLOT_SET_X, x);
}

SCM_SYNTAX (s_atdispatch, "@dispatch", scm_makmmacro, scm_m_atdispatch);

SCM_SYMBOL (sym_atdispatch, s_atdispatch);

SCM
scm_m_atdispatch (SCM xorig, SCM env)
{
  SCM args, n, v, gf, x = SCM_CDR (xorig);
  SCM_ASSYNT (scm_ilength (x) == 4, xorig, scm_s_expression, s_atdispatch);
  args = SCM_CAR (x);
  SCM_ASSYNT (SCM_NIMP (args) && (SCM_CONSP (args) || SCM_SYMBOLP (args)),
	      args, SCM_ARG1, s_atdispatch);
  x = SCM_CDR (x);
  n = SCM_XEVALCAR (x, env);
  SCM_ASSYNT (SCM_INUMP (n), n, SCM_ARG2, s_atdispatch);
  SCM_ASSYNT (SCM_INUM (n) >= 1, n, SCM_OUTOFRANGE, s_atdispatch);
  x = SCM_CDR (x);
  v = SCM_XEVALCAR (x, env);
  SCM_ASSYNT (SCM_NIMP (v) && SCM_VECTORP (v), v, SCM_ARG3, s_atdispatch);
  x = SCM_CDR (x);
  gf = SCM_XEVALCAR (x, env);
  SCM_ASSYNT (SCM_NIMP (gf) && SCM_STRUCTP (gf) && SCM_PUREGENERICP (gf),
	      gf, SCM_ARG4, s_atdispatch);
  return SCM_LIST5 (SCM_IM_DISPATCH, args, n, v, gf);
}

#ifdef USE_THREADS
static void
lock_cache_mutex (void *m)
{
  SCM mutex = (SCM) m;
  scm_lock_mutex (mutex);
}

static void
unlock_cache_mutex (void *m)
{
  SCM mutex = (SCM) m;
  scm_unlock_mutex (mutex);
}
#endif

static SCM
call_memoize_method (void *a)
{
  SCM args = (SCM) a;
  SCM gf = SCM_CAR (args);
  SCM x = SCM_CADR (args);
  /* First check if another thread has inserted a method between
   * the cache miss and locking the mutex.
   */
  SCM cmethod = scm_mcache_lookup_cmethod (x, SCM_CDDR (args));
  if (SCM_NIMP (cmethod))
    return cmethod;
  /*fixme* Use scm_apply */
  return CALL_GF3 ("memoize-method!", gf, SCM_CDDR (args), x);
}

static SCM
memoize_method (SCM x, SCM args)
{
  SCM gf = SCM_CAR (scm_last_pair (x));
#ifdef USE_THREADS
  return scm_internal_dynamic_wind (lock_cache_mutex,
				    call_memoize_method,
				    unlock_cache_mutex,
				    (void *) scm_cons2 (gf, x, args),
				    (void *) SCM_SLOT (gf, scm_si_cache_mutex));
#else
  return call_memoize_method ((void *) scm_cons2 (gf, x, args));
#endif
}

/******************************************************************************
 *
 * A simple make (which will be redefined later in Scheme)
 * This version handles only creation of gf, methods and classes (no instances)
 *
 * Since this code will disappear when Goops will be fully booted, 
 * no precaution is taken to be efficient.
 *
 ******************************************************************************/

SCM_KEYWORD (k_setter,		"setter");
SCM_KEYWORD (k_specializers,	"specializers");
SCM_KEYWORD (k_procedure,	"procedure");
SCM_KEYWORD (k_dsupers,		"dsupers");
SCM_KEYWORD (k_slots,		"slots");
SCM_KEYWORD (k_gf,		"generic-function");

SCM_PROC (s_make, "make",  0, 0, 1, scm_make);

SCM
scm_make (SCM args)
{
  SCM class, z;
  int len = scm_ilength (args);

  if (len <= 0 || (len & 1) == 0)
    scm_wrong_num_args (scm_makfrom0str (s_make));

  class = SCM_CAR(args);
  args  = SCM_CDR(args);

  if (class == scm_class_generic || class == scm_class_generic_with_setter)
    {
#ifdef USE_THREADS
      z = scm_make_struct (class, SCM_INUM0,
			   SCM_LIST4 (SCM_EOL,
				      SCM_INUM0,
				      SCM_BOOL_F,
				      scm_make_mutex ()));
#else
      z = scm_make_struct (class, SCM_INUM0,
			   SCM_LIST3 (SCM_EOL, SCM_INUM0, SCM_BOOL_F));
#endif
      scm_set_procedure_property_x (z, scm_sym_name,
				    scm_get_keyword (k_name,
						     args,
						     SCM_BOOL_F));
      clear_method_cache (z);
      if (class == scm_class_generic_with_setter)
	{
	  SCM setter = scm_get_keyword (k_setter, args, SCM_BOOL_F);
	  if (SCM_NIMP (setter))
	    scm_sys_set_object_setter_x (z, setter);
	}
    }
  else
    {
      z = scm_sys_allocate_instance (class, args);

      if (class == scm_class_method
	  || class == scm_class_simple_method
	  || class == scm_class_accessor)
	{
	  SCM_SLOT (z, scm_si_generic_function) =  
	    scm_i_get_keyword (k_gf,
			       args,
			       len - 1,
			       SCM_BOOL_F,
			       s_make);
	  SCM_SLOT (z, scm_si_specializers) =  
	    scm_i_get_keyword (k_specializers,
			       args,
			       len - 1,
			       SCM_EOL,
			       s_make);
	  SCM_SLOT (z, scm_si_procedure) =
	    scm_i_get_keyword (k_procedure,
			       args,
			       len - 1,
			       SCM_EOL,
			       s_make);
	  SCM_SLOT (z, scm_si_code_table) = SCM_EOL;
	}
      else
	{
	  /* In all the others case, make a new class .... No instance here */
	  SCM_SLOT (z, scm_si_name) = 
	    scm_i_get_keyword (k_name,
			       args,
			       len - 1,
			       Intern ("???"),
			       s_make);
	  SCM_SLOT (z, scm_si_direct_supers) = 
	    scm_i_get_keyword (k_dsupers,
			       args,
			       len - 1,
			       SCM_EOL,
			       s_make);
	  SCM_SLOT (z, scm_si_direct_slots)  = 
	    scm_i_get_keyword (k_slots,
			       args,
			       len - 1,
			       SCM_EOL,
			       s_make);
	}
    }
  return z;
}

SCM_PROC (s_find_method, "find-method", 0, 0, 1, scm_find_method);

SCM
scm_find_method (SCM l)
{
  SCM gf;
  int len = scm_ilength (l);

  if (len == 0)
    scm_wrong_num_args (scm_makfrom0str (s_find_method));

  gf = SCM_CAR(l); l = SCM_CDR(l);
  SCM_ASSERT (SCM_NIMP (gf) && GENERICP (gf), gf, SCM_ARG1, s_find_method);
  if (SCM_NULLP (SCM_SLOT (gf, scm_si_methods)))
    scm_misc_error (s_find_method,
		    scm_c_oldfmt0 ("no methods for generic ~S"),
		    SCM_LIST1 (gf));

  return scm_compute_applicable_methods (gf, l, len - 1, 1);
}

SCM_PROC (s_sys_method_more_specific_p, "%method-more-specific?", 3, 0, 0, scm_sys_method_more_specific_p);

SCM
scm_sys_method_more_specific_p (SCM m1, SCM m2, SCM targs)
{
  SCM l, v;
  int i, len;

  SCM_ASSERT (SCM_NIMP (m1) && METHODP (m1),
	      m1, SCM_ARG1, s_sys_method_more_specific_p);
  SCM_ASSERT (SCM_NIMP (m2) && METHODP (m2),
	      m2, SCM_ARG2, s_sys_method_more_specific_p);
  SCM_ASSERT ((len = scm_ilength (targs)) != -1,
	      targs, SCM_ARG3, s_sys_method_more_specific_p);

  /* Verify that all the arguments of targs are classes and place them in a vector*/
  v = scm_make_vector (SCM_MAKINUM (len), SCM_EOL);

  for (i=0, l=targs; SCM_NNULLP(l); i++, l=SCM_CDR(l)) {
    SCM_ASSERT (SCM_NIMP (SCM_CAR (l)) && CLASSP (SCM_CAR (l)),
		targs, SCM_ARG3, s_sys_method_more_specific_p);
    SCM_VELTS(v)[i] = SCM_CAR(l);
  }
  return more_specificp (m1, m2, SCM_VELTS(v)) ? SCM_BOOL_T: SCM_BOOL_F;
}
  
  

/******************************************************************************
 *
 * Initializations 
 *
 ******************************************************************************/


static void
make_stdcls (SCM *var, char *name, SCM meta, SCM super, SCM slots)
{
   SCM tmp = Intern(name);
   
   *var = scm_permanent_object (scm_basic_make_class (meta,
						      tmp,
						      SCM_CONSP (super)
						      ? super
						      : SCM_LIST1 (super),
						      slots));
   DEFVAR(tmp, *var);
}


SCM_KEYWORD (k_slot_definition, "slot-definition");

static void
create_standard_classes (void)
{
  SCM slots;
  SCM method_slots = SCM_LIST4 (Intern ("generic-function"), 
				Intern ("specializers"), 
				Intern ("procedure"),
				Intern ("code-table"));
  SCM amethod_slots = SCM_LIST1 (SCM_LIST3 (Intern ("slot-definition"),
					    k_init_keyword,
					    k_slot_definition));
#ifdef USE_THREADS
  SCM mutex_slot = SCM_LIST1 (Intern ("make-mutex"));
#else
  SCM mutex_slot = SCM_BOOL_F;
#endif
  SCM gf_slots = SCM_LIST4 (Intern ("methods"),
			    SCM_LIST3 (Intern ("n-specialized"),
				       k_init_value,
				       SCM_INUM0),
			    SCM_LIST3 (Intern ("used-by"),
				       k_init_value,
				       SCM_BOOL_F),
			    SCM_LIST3 (Intern ("cache-mutex"),
				       k_init_thunk,
				       scm_closure (SCM_LIST2 (SCM_EOL,
							       mutex_slot),
						    SCM_EOL)));

  /* Foreign class slot classes */
  make_stdcls (&scm_class_foreign_slot,	   "<foreign-slot>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_protected,	   "<protected-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_opaque,	   "<opaque-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_read_only,	   "<read-only-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_self,		   "<self-slot>",
	       scm_class_class,
	       SCM_LIST2 (scm_class_foreign_slot, scm_class_read_only),
	       SCM_EOL);
  make_stdcls (&scm_class_protected_opaque, "<protected-opaque-slot>",
	       scm_class_class,
	       SCM_LIST2 (scm_class_protected, scm_class_opaque),
	       SCM_EOL);
  make_stdcls (&scm_class_protected_read_only, "<protected-read-only-slot>",
	       scm_class_class,
	       SCM_LIST2 (scm_class_protected, scm_class_read_only),
	       SCM_EOL);
  make_stdcls (&scm_class_scm,		   "<scm-slot>",
	       scm_class_class, scm_class_protected, SCM_EOL);
  make_stdcls (&scm_class_int,		   "<int-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_float,	   "<float-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_double,	   "<double-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);

  /* Continue initialization of class <class> */
  
  slots = build_class_class_slots ();
  SCM_SLOT (scm_class_class, scm_si_direct_slots) = slots;
  SCM_SLOT (scm_class_class, scm_si_slots) = slots;
  SCM_SLOT (scm_class_class, scm_si_getters_n_setters)
    = compute_getters_n_setters (slots);
  
  make_stdcls (&scm_class_foreign_class, "<foreign-class>",
	       scm_class_class, scm_class_class,
	       SCM_LIST2 (SCM_LIST3 (Intern ("constructor"),
				     k_class,
				     scm_class_opaque),
			  SCM_LIST3 (Intern ("destructor"),
				     k_class,
				     scm_class_opaque)));
  make_stdcls (&scm_class_foreign_object,  "<foreign-object>",
	       scm_class_foreign_class, scm_class_object,   SCM_EOL);
  SCM_SET_CLASS_FLAGS (scm_class_foreign_object, SCM_CLASSF_FOREIGN);

  /* scm_class_generic functions classes */
  make_stdcls (&scm_class_procedure_class, "<procedure-class>",
	       scm_class_class, scm_class_class, SCM_EOL);
  make_stdcls (&scm_class_entity_class,    "<entity-class>",
	       scm_class_class, scm_class_procedure_class, SCM_EOL);
  make_stdcls (&scm_class_operator_class,  "<operator-class>",
	       scm_class_class, scm_class_procedure_class, SCM_EOL);
  make_stdcls (&scm_class_operator_with_setter_class,
	       "<operator-with-setter-class>",
	       scm_class_class, scm_class_operator_class, SCM_EOL);
  make_stdcls (&scm_class_method,	   "<method>",
	       scm_class_class, scm_class_object,	   method_slots);
  make_stdcls (&scm_class_simple_method,   "<simple-method>",
	       scm_class_class, scm_class_method,	   SCM_EOL);
  SCM_SET_CLASS_FLAGS (scm_class_simple_method, SCM_CLASSF_SIMPLE_METHOD);
  make_stdcls (&scm_class_accessor,	   "<accessor-method>",
	       scm_class_class, scm_class_simple_method,   amethod_slots);
  SCM_SET_CLASS_FLAGS (scm_class_accessor, SCM_CLASSF_ACCESSOR_METHOD);
  make_stdcls (&scm_class_entity,	   "<entity>",
	       scm_class_entity_class, scm_class_object,   SCM_EOL);
  make_stdcls (&scm_class_entity_with_setter, "<entity-with-setter>",
	       scm_class_entity_class, scm_class_entity,   SCM_EOL);
  make_stdcls (&scm_class_generic,	   "<generic>",
	       scm_class_entity_class, scm_class_entity,   gf_slots);
  SCM_SET_CLASS_FLAGS (scm_class_generic, SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_generic_with_setter, "<generic-with-setter>",
	       scm_class_entity_class,
	       SCM_LIST2 (scm_class_generic, scm_class_entity_with_setter),
	       SCM_EOL);
#if 0
  /* Patch cpl since compute_cpl doesn't support multiple inheritance. */
  SCM_SLOT (scm_class_generic_with_setter, scm_si_cpl) =
    scm_append (SCM_LIST3 (SCM_LIST2 (scm_class_generic_with_setter,
				      scm_class_generic),
			   SCM_SLOT (scm_class_entity_with_setter,
				     scm_si_cpl),
			   SCM_EOL));
#endif
  SCM_SET_CLASS_FLAGS (scm_class_generic_with_setter, SCM_CLASSF_PURE_GENERIC);

  /* Primitive types classes */
  make_stdcls (&scm_class_boolean, 	   "<boolean>",
	       scm_class_class, scm_class_top, 	    	   SCM_EOL);
  make_stdcls (&scm_class_char,		   "<char>",
	       scm_class_class, scm_class_top,	    	   SCM_EOL);
  make_stdcls (&scm_class_list,	   	   "<list>",
	       scm_class_class, scm_class_top,	    	   SCM_EOL);
  make_stdcls (&scm_class_pair,		   "<pair>",
	       scm_class_class, scm_class_list,		   SCM_EOL);
  make_stdcls (&scm_class_null,		   "<null>",
	       scm_class_class, scm_class_list,		   SCM_EOL);
  make_stdcls (&scm_class_string,	   "<string>",
	       scm_class_class, scm_class_top,	    	   SCM_EOL);
  make_stdcls (&scm_class_symbol,	   "<symbol>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_vector,	   "<vector>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_number,	   "<number>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_complex,	   "<complex>",
	       scm_class_class, scm_class_number, 	   SCM_EOL);
  make_stdcls (&scm_class_real,		   "<real>",
	       scm_class_class, scm_class_complex,	   SCM_EOL);
  make_stdcls (&scm_class_integer,	   "<integer>",
	       scm_class_class, scm_class_real,		   SCM_EOL);
  make_stdcls (&scm_class_keyword,	   "<keyword>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_unknown,	   "<unknown>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_procedure,	   "<procedure>",
	       scm_class_procedure_class, scm_class_top,   SCM_EOL);
  make_stdcls (&scm_class_procedure_with_setter, "<procedure-with-setter>",
	       scm_class_procedure_class, scm_class_procedure, SCM_EOL);
  make_stdcls (&scm_class_primitive_generic, "<primitive-generic>",
	       scm_class_procedure_class, scm_class_procedure, SCM_EOL);
  make_stdcls (&scm_class_port,		   "<port>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_input_port,	   "<input-port>",
	       scm_class_class, scm_class_port,		   SCM_EOL);
  make_stdcls (&scm_class_output_port,	   "<output-port>",
	       scm_class_class, scm_class_port,		   SCM_EOL);
  make_stdcls (&scm_class_input_output_port, "<input-output-port>",
	       scm_class_class,
	       SCM_LIST2 (scm_class_input_port, scm_class_output_port),
	       SCM_EOL);
}

/**********************************************************************
 *
 * Smob classes
 *
 **********************************************************************/

static SCM
make_class_from_template (char *template, char *type_name, SCM supers)
{
  SCM class, name;
  if (type_name)
    {
      char buffer[100];
      sprintf (buffer, template, type_name);
      name = Intern (buffer);
    }
  else
    name = SCM_GOOPS_UNBOUND;

  class = scm_permanent_object (scm_basic_make_class (scm_class_class,
						      name,
						      supers,
						      SCM_EOL));

  /* Only define name if doesn't already exist. */
  if (!SCM_GOOPS_UNBOUNDP (name)
      && SCM_FALSEP (scm_apply (scm_goops_lookup_closure,
				SCM_LIST2 (name, SCM_BOOL_F),
				SCM_EOL)))
    {
      /* Make sure we add the binding in the GOOPS module.
       * This kludge is needed until DEFVAR ceases to use `define-public'
       * or `define-public' ceases to use `current-module'.
       */
      SCM old_module = scm_select_module (scm_module_goops);
      DEFVAR (name, class);
      scm_select_module (old_module);
    }
  return class;
}

static SCM
make_extended_class (char *type_name)
{
  return make_class_from_template ("<%s>",
				   type_name,
				   SCM_LIST1 (scm_class_top));
}

static void
create_smob_classes (void)
{
  int i;

  scm_smob_class = (SCM *) malloc (255 * sizeof (SCM));
  for (i = 0; i < 255; ++i)
    scm_smob_class[i] = 0;

#ifdef scm_tc16_big
  scm_smob_class[SCM_TC2SMOBNUM (scm_tc16_big)] = scm_class_integer;
  scm_smob_class[SCM_TC2SMOBNUM (scm_tc16_real)] = scm_class_real;
  scm_smob_class[SCM_TC2SMOBNUM (scm_tc16_complex)] = scm_class_complex;
#else
  scm_smob_class[SCM_TC2SMOBNUM (scm_tc16_bigpos)] = scm_class_integer;
  scm_smob_class[SCM_TC2SMOBNUM (scm_tc16_bigneg)] = scm_class_integer;
#endif
  scm_smob_class[SCM_TC2SMOBNUM (scm_tc16_keyword)] = scm_class_keyword;
  
  for (i = 0; i < scm_numsmob; ++i)
    if (!scm_smob_class[i])
      scm_smob_class[i] = scm_make_extended_class (SCM_SMOBNAME (i));
}

static void
make_port_classes (int ptobnum, char *type_name)
{
  SCM c, class = make_class_from_template ("<%s-port>",
					   type_name,
					   SCM_LIST1 (scm_class_port));
  scm_port_class[SCM_IN_PCLASS_INDEX + ptobnum]
    = make_class_from_template ("<%s-input-port>",
				type_name,
				SCM_LIST2 (class, scm_class_input_port));
  scm_port_class[SCM_OUT_PCLASS_INDEX + ptobnum]
    = make_class_from_template ("<%s-output-port>",
				type_name,
				SCM_LIST2 (class, scm_class_output_port));
  scm_port_class[SCM_INOUT_PCLASS_INDEX + ptobnum]
    = c
    = make_class_from_template ("<%s-input-output-port>",
				type_name,
				SCM_LIST2 (class,
					   scm_class_input_output_port));
  /* Patch cpl (since this tree is too complex for the C level compute-cpl) */
  SCM_SLOT (c, scm_si_cpl)
    = scm_cons2 (c, class, SCM_SLOT (scm_class_input_output_port, scm_si_cpl));
}

static void
create_port_classes (void)
{
  int i;

  scm_port_class = (SCM *) malloc (3 * 256 * sizeof (SCM));
  for (i = 0; i < 3 * 256; ++i)
    scm_port_class[i] = 0;

  for (i = 0; i < scm_numptob; ++i)
    make_port_classes (i, SCM_PTOBNAME (i));
}

static SCM
make_struct_class (void *closure, SCM key, SCM data, SCM prev)
{
  if (SCM_NFALSEP (SCM_STRUCT_TABLE_NAME (data)))
    SCM_SET_STRUCT_TABLE_CLASS (data,
				scm_make_extended_class
				(SCM_ROCHARS (SCM_STRUCT_TABLE_NAME (data))));
  return SCM_UNSPECIFIED;
}

static void
create_struct_classes (void)
{
  scm_internal_hash_fold (make_struct_class, 0, SCM_BOOL_F, scm_struct_table);
}

/**********************************************************************
 *
 * C interface
 *
 **********************************************************************/

void
scm_load_goops ()
{
  if (!goops_loaded_p)
    scm_resolve_module (scm_read_0str ("(oop goops)"));
}

SCM
scm_make_foreign_object (SCM class, SCM initargs)
{
  void * (*constructor) (SCM)
    = (void * (*) (SCM)) SCM_SLOT (class, scm_si_constructor);
  SCM_ASSERT (constructor != 0, class, "Can't make instances of this class",
	      s_make);
  return scm_wrap_object (class, constructor (initargs));
}

static size_t
scm_free_foreign_object (SCM *class, SCM *data)
{
  size_t (*destructor) (void *)
    = (size_t (*) (void *)) class[scm_si_destructor];
  return destructor (data);
}

SCM
scm_make_class (SCM meta, char *s_name, SCM supers, size_t size,
		void * (*constructor) (SCM initargs),
		size_t (*destructor) (void *))
{
  SCM name, class;
  name = Intern (s_name);
  if (SCM_IMP (supers))
    supers = SCM_LIST1 (scm_class_foreign_object);
  class = scm_basic_basic_make_class (meta, name, supers, SCM_EOL);
  scm_sys_inherit_magic_x (class, supers);

  if (destructor != 0)
    {
      SCM_SLOT (class, scm_si_destructor) = (SCM) destructor;
      SCM_SET_CLASS_DESTRUCTOR (class, scm_free_foreign_object);
    }
  else if (size > 0)
    {
      SCM_SET_CLASS_DESTRUCTOR (class, scm_struct_free_light);
      SCM_SET_CLASS_INSTANCE_SIZE (class, size);
    }
  
  SCM_SLOT (class, scm_si_layout) = SCM_CAR (scm_intern ("", 0));
  SCM_SLOT (class, scm_si_constructor) = (SCM) constructor;

  return class;
}

SCM_SYMBOL (sym_o, "o");
SCM_SYMBOL (sym_x, "x");

SCM_KEYWORD (k_accessor, "accessor");
SCM_KEYWORD (k_getter, "getter");

static SCM
default_setter (SCM obj, SCM c)
{
  scm_misc_error ("slot-set!", "read-only slot", SCM_EOL);
  return 0;
}

void
scm_add_slot (SCM class, char *slot_name, SCM slot_class,
	      SCM (*getter) (SCM obj),
	      SCM (*setter) (SCM obj, SCM x),
	      char *accessor_name)
{
  {
    SCM get = scm_make_subr_opt ("goops:get", scm_tc7_subr_1, getter, 0);
    SCM set = scm_make_subr_opt ("goops:set", scm_tc7_subr_2,
				 setter ? setter : default_setter, 0);
    SCM getm = scm_closure (SCM_LIST2 (SCM_LIST1 (sym_o),
				       SCM_LIST2 (get, sym_o)),
			    SCM_EOL);
    SCM setm = scm_closure (SCM_LIST2 (SCM_LIST2 (sym_o, sym_x),
				       SCM_LIST3 (set, sym_o, sym_x)),
			    SCM_EOL);
    {
      SCM name = SCM_CAR (scm_intern0 (slot_name));
      SCM aname = SCM_CAR (scm_intern0 (accessor_name));
      SCM gf = scm_ensure_accessor (aname);
      SCM slot = SCM_LIST5 (name,
			    k_class, slot_class,
			    setter ? k_accessor : k_getter,
			    gf);
      SCM gns = SCM_LIST4 (name, SCM_BOOL_F, get, set);

      scm_add_method (gf, scm_make (SCM_LIST5 (scm_class_accessor,
					       k_specializers,
					       SCM_LIST1 (class),
					       k_procedure, getm)));
      scm_add_method (scm_setter (gf),
		      scm_make (SCM_LIST5 (scm_class_accessor,
					   k_specializers,
					   SCM_LIST2 (class,
						      scm_class_top),
					   k_procedure, setm)));
      DEFVAR (aname, gf);
      
      SCM_SLOT (class, scm_si_slots)
	= scm_append_x (SCM_LIST2 (SCM_SLOT (class, scm_si_slots),
				   SCM_LIST1 (slot)));
      SCM_SLOT (class, scm_si_getters_n_setters)
	= scm_append_x (SCM_LIST2 (SCM_SLOT (class, scm_si_getters_n_setters),
				   SCM_LIST1 (gns)));
    }
  }
  {  
    int n = SCM_INUM (SCM_SLOT (class, scm_si_nfields));

    SCM_SLOT (class, scm_si_nfields)
      = SCM_MAKINUM (n + 1);
  }
}

SCM
scm_wrap_object (SCM class, void *data)
{
  SCM z;
#ifdef SCM_STRUCT_GC_CHAIN
  SCM_NEWCELL2 (z);
#else
  SCM_NEWCELL (z);
#endif
  SCM_SETCDR (z, (SCM) data);
#ifdef SCM_STRUCT_GC_CHAIN
  SCM_SET_STRUCT_GC_CHAIN (z, 0);
#endif
  SCM_SETCAR (z, SCM_UNPACK (SCM_CDR (class)) | scm_tc3_cons_gloc);
  return z;
}

SCM scm_components;

SCM
scm_wrap_component (SCM class, SCM container, void *data)
{
  SCM obj = scm_wrap_object (class, data);
  SCM handle = scm_hash_fn_create_handle_x (scm_components,
					    obj,
					    SCM_BOOL_F,
					    scm_struct_ihashq,
					    scm_sloppy_assq,
					    0);
  SCM_SETCDR (handle, container);
  return obj;
}

SCM
scm_ensure_accessor (SCM name)
{
  SCM gf = scm_apply (SCM_TOP_LEVEL_LOOKUP_CLOSURE,
		      SCM_LIST2 (name, SCM_BOOL_F),
		      SCM_EOL);
  if (!SCM_IS_A_P (gf, scm_class_generic_with_setter))
    {
      gf = scm_make (SCM_LIST3 (scm_class_generic, k_name, name));
      gf = scm_make (SCM_LIST5 (scm_class_generic_with_setter,
				k_name, name,
				k_setter, gf));
    }
  return gf;
}

SCM_SYMBOL (sym_internal_add_method_x, "internal-add-method!");

void
scm_add_method (SCM gf, SCM m)
{
  scm_eval2 (SCM_LIST3 (sym_internal_add_method_x, gf, m),
	     scm_goops_lookup_closure);
}

#ifdef GUILE_DEBUG
/*
 * Debugging utilities
 */

SCM_PROC (s_pure_generic_p, "pure-generic?", 1, 0, 0, scm_pure_generic_p);

SCM
scm_pure_generic_p (SCM obj)
{
  return (SCM_NIMP (obj) && SCM_STRUCTP (obj) && SCM_PUREGENERICP (obj)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

#endif /* GUILE_DEBUG */

/*
 * Initialization
 */

SCM_PROC (scm_sys_goops_loaded, "%goops-loaded", 0, 0, 0, sys_goops_loaded);

static SCM
sys_goops_loaded ()
{
  goops_loaded_p = 1;
  var_compute_applicable_methods
    = SCM_CDR (scm_apply (scm_goops_lookup_closure,
			  SCM_LIST2 (SCM_CAR (var_compute_applicable_methods),
				     SCM_BOOL_F),
			  SCM_EOL));
  return SCM_UNSPECIFIED;
}

SCM scm_module_goops;

void
scm_init_goops (void)
{
  SCM old_module;
#ifndef SCM_UNBOUND
  SCM the_unbound_value = SCM_CAR (scm_intern0 ("the-unbound-value"));
  scm_goops_the_unbound_value
    = scm_permanent_object (scm_cons (the_unbound_value, SCM_EOL));
#endif
  scm_module_goops = scm_make_module (scm_read_0str ("(oop goops)"));
  old_module = scm_select_module (scm_module_goops);
  
  scm_goops_lookup_closure = scm_module_lookup_closure (scm_module_goops);

  scm_components = scm_permanent_object (scm_make_weak_key_hash_table
					 (SCM_MAKINUM (37)));

  goops_rstate = scm_c_make_rstate ("GOOPS", 5);

#include "goops.x"

  list_of_no_method = scm_permanent_object (SCM_LIST1 (sym_no_method));

  hell = scm_must_malloc (hell_size, "hell");
#ifdef USE_THREADS
#ifdef SCM_MUTEX_INIT_TWO_ARGS
  scm_mutex_init (&hell_mutex, NULL);
#else
  scm_mutex_init (&hell_mutex);
#endif
#endif

  scm_make_extended_class = make_extended_class;
  scm_make_port_classes = make_port_classes;
  scm_change_object_class = change_class;
  scm_memoize_method = memoize_method;

  create_basic_classes ();
  create_standard_classes ();
  create_smob_classes ();
  create_struct_classes ();
  create_port_classes ();

  {
    SCM name = SCM_CAR (scm_intern0 ("no-applicable-method"));
    scm_no_applicable_method
      = scm_permanent_object (scm_make (SCM_LIST3 (scm_class_generic,
						   k_name,
						   name)));
    DEFVAR (name, scm_no_applicable_method);
  }
  
  scm_select_module (old_module);
}

void
scm_init_oop_goops_goopscore_module ()
{
  scm_register_module_xxx ("oop goops goopscore", (void *) scm_init_goops);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
