/*	Copyright (C) 1998 Free Software Foundation, Inc.
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

#define CLASSP(x)   (SCM_STRUCTP (x) \
		     && SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_METACLASS)
#define GENERICP(x) (SCM_INSTANCEP (x) \
		     && SCM_SUBCLASSP (SCM_CLASS_OF (x), Generic))
#define METHODP(x)  (SCM_INSTANCEP (x) \
		     && SCM_SUBCLASSP(SCM_CLASS_OF(x), Method))
#define SPEC_OF(x)  SCM_SLOT (x, scm_si_specializers)


#define DEFVAR(v,val) \
{ scm_eval2 (SCM_LIST3 (scm_sym_define_public, (v), (val)), \
	     scm_goops_lookup_closure); }
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

#define CLASS_REDEF(c) SCM_SLOT(c, scm_si_redefined)
#define TEST_CHANGE_CLASS(obj, class) 						\
	{ 									\
	  class = SCM_CLASS_OF(obj);						\
          if (CLASS_REDEF(class) != SCM_BOOL_F) 					\
	    CALL_GF3("change-object-class", obj, class, CLASS_REDEF(class));	\
	}

#define NXT_MTHD_METHODS(m)	(SCM_VELTS (m)[1])
#define NXT_MTHD_ARGS(m)	(SCM_VELTS (m)[2])

#define SCM_GOOPS_UNBOUND scm_goops_the_unbound_value
#define SCM_GOOPS_UNBOUNDP(x) ((x) == scm_goops_the_unbound_value)

static SCM scm_goops_the_unbound_value;

#if 0 /* Used by extended classes */
static char initialized	     = 0;
#endif

static SCM scm_goops_lookup_closure;
static SCM Top, Object, Class, Generic, Method, Simple_method, Accessor, 
  	   Procedure_class, Operator_class, Entity_class;
static SCM Boolean, Char, Pair, Procedure, String, Symbol, Vector, Number, 
	   Liste, Null, Real, Complex, Integer, Keyword, Unknown;
#ifdef USE_TK
static SCM Widget;
#endif

SCM_SYMBOL (scm_sym_define_public, "define-public");

static void set_slot_value_if_unbound (SCM class, SCM obj, SCM slot_name, SCM form);
static SCM scm_make_unbound (void);
static SCM scm_unbound_p (SCM obj);

/******************************************************************************
 *
 * Compute-cpl
 *
 *   This version doesn't handle multiple-inheritance. It serves only for
 * booting classes and will be overaloaded in Scheme
 *
 ******************************************************************************/

static SCM
compute_cpl (SCM supers, SCM res)
{
  return (SCM_NULLP (supers)
	  ? scm_reverse (res)
	  : compute_cpl (SCM_SLOT (SCM_CAR (supers), scm_si_direct_supers),
			 scm_cons (SCM_CAR (supers), res)));
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

  tmp = (SCM_NIMP (SCM_CAR (l)) && SCM_CONSP (SCM_CAR (l))
	 ? SCM_CAR (SCM_CAR (l))
	 : SCM_CAR (l));
  if (!(SCM_NIMP (tmp) && SCM_SYMBOLP (tmp)))
    scm_misc_error ("%compute-slots", "bad slot name %S", SCM_LIST1 (tmp));
  
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


SCM_PROC (s_sys_compute_slots, "%compute-slots", 1, 0, 0, scm_sys_compute_slots);

SCM
scm_sys_compute_slots (SCM class)
{
  SCM_ASSERT (SCM_NIMP (class) && CLASSP (class),
	      class, SCM_ARG1, s_sys_compute_slots);
  return build_slots_list (SCM_SLOT(class, scm_si_direct_slots),
			  SCM_SLOT(class, scm_si_cpl));
}

/******************************************************************************
 *
 * compute-getters-n-setters
 *  
 *   This version doesn't handle slot options. It serves only for booting 
 * classes and will be overaloaded in Scheme.
 *
 ******************************************************************************/

static SCM
compute_getters_n_setters (SCM slots)
{
  SCM  res = SCM_EOL;
  long i   = 0;

  for (  ; SCM_NNULLP(slots); slots = SCM_CDR(slots)) 
    res = scm_cons (scm_cons (SCM_CAR (slots),
			      scm_cons (SCM_BOOL_F, 
					SCM_MAKINUM (i++))),
		    res);
  return res;
}

/******************************************************************************
 *
 * initialize-object
 *
 ******************************************************************************/

/*fixme* Manufacture keywords in advance */
static SCM
scm_makekey (char *s)
{
  SCM vcell = scm_sysintern0 (s);
  return scm_make_keyword_from_dash_symbol (SCM_CAR (vcell));
}

SCM
scm_i_get_keyword (SCM key, SCM l, int len, SCM default_value, char *subr)
{
  int i;
  for (i = 0; i < len; i += 2)
    {
      if (!(SCM_NIMP (SCM_CAR (l)) && SCM_KEYWORDP (SCM_CAR (l))))
	scm_misc_error (subr, "bad keyword: %S", SCM_LIST1 (SCM_CAR (l)));
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
	      key, SCM_ARG1, s_get_keyword);
  len = scm_ilength (l);
  SCM_ASSERT (len >= 0 && (len & 1) == 0, l, SCM_ARG2, s_get_keyword);
  return scm_i_get_keyword (key, l, len, default_value, s_get_keyword);
}

SCM_PROC (s_sys_initialize_object, "%initialize-object", 2, 0, 0, scm_sys_initialize_object);

SCM
scm_sys_initialize_object (SCM obj, SCM initargs)
{
  static char k_init_keyword[] = "-init-keyword";
  SCM tmp, get_n_set, slots;
  SCM init_keyword = scm_makekey (k_init_keyword);
  SCM class       = SCM_CLASS_OF (obj);
  int n_initargs;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_sys_initialize_object);
  n_initargs = scm_ilength (initargs);
  SCM_ASSERT ((n_initargs & 1) == 0,
	      initargs, SCM_ARG2, s_sys_initialize_object);
  
  get_n_set = SCM_SLOT(class, scm_si_getters_n_setters);
  slots     = SCM_SLOT(class, scm_si_slots);
  
  /* See for each slot how it must be initialized */
  for ( ; SCM_NNULLP(slots); get_n_set=SCM_CDR(get_n_set), slots=SCM_CDR(slots)) {
    SCM slot_name  = SCM_CAR(slots);
    SCM slot_value = 0;
    
    if (SCM_NIMP (slot_name) && SCM_CONSP (slot_name)) {
      /* This slot admits (perhaps) to be initialized at creation time */
      int n = scm_ilength (SCM_CDR (slot_name));
      if (n & 1) /* odd or -1 */
	scm_misc_error (s_sys_initialize_object,
			"class contains bogus slot definition: %S",
			SCM_LIST1 (slot_name));
      tmp 	= scm_i_get_keyword (init_keyword,
				   SCM_CDR (slot_name),
				   n,
				   0,
				   s_sys_initialize_object);
      slot_name = SCM_CAR(slot_name);
      if (tmp) {
	/* an initarg was provided for this slot */
	if (!(SCM_NIMP (tmp) && SCM_KEYWORDP (tmp)))
	  scm_misc_error (s_sys_initialize_object,
			  "initarg must be a keyword. It was %S",
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
      scm_slot_set_x(obj, slot_name, slot_value);
    else {
      /* set slot to its :initform if it exists */
      tmp = SCM_CAR(SCM_CDR(SCM_CAR(get_n_set)));
      if (tmp != SCM_BOOL_F)
	set_slot_value_if_unbound(class, obj, slot_name, tmp);
    }
  }
  
  return obj;
}

SCM_PROC (s_sys_prep_layout_x, "%prep-layout!", 1, 0, 0, scm_sys_prep_layout_x);

SCM
scm_sys_prep_layout_x (SCM class)
{
  int i, n;
  char* s;
  SCM nfields;

  SCM_ASSERT (SCM_NIMP (class) && SCM_INSTANCEP (class),
	      class,
	      SCM_ARG1,
	      s_sys_prep_layout_x);
  nfields = scm_slot_ref (class, Intern ("nfields"));
  if (!SCM_INUMP (nfields) || SCM_INUM (nfields) < 0)
    scm_misc_error (s_sys_prep_layout_x,
		    "bad value in nfields slot: %S",
		    SCM_LIST1 (nfields));
  n = 2 * SCM_INUM (nfields);
  s  = scm_must_malloc (n, s_sys_prep_layout_x);
  for (i = 0; i < n; i += 2)
    {
      s[i] = 'p';
      s[i + 1] = 'w';
    }
  if (SCM_SUBCLASSP (class, Class))
    {
      if (n < 16)
	{
	  scm_must_free (s);
	  scm_misc_error (s_sys_prep_layout_x,
			  "class object doesn't have enough fields: %S",
			  SCM_LIST1 (nfields));
	}
      strncpy (s, "pruosrpwpopopopo", 16);
    }
  SCM_SLOT (class, scm_si_layout) = SCM_CAR (scm_intern (s, n));
  scm_must_free (s);
  return SCM_UNSPECIFIED;
}

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
  SCM_SET_CLASS_FLAGS (class, flags & SCM_CLASSF_MASK);
  return SCM_UNSPECIFIED;
}

/******************************************************************************/

SCM
scm_basic_make_class (SCM class, SCM name, SCM dsupers, SCM dslots)
{
  SCM z, cpl, slots, nfields, g_n_s;

  /* Allocate one instance */
  z = scm_make_struct (class, SCM_INUM0, SCM_EOL);

  /* Initialize its slots */
  cpl   = compute_cpl (dsupers, SCM_LIST1(z));
  slots = build_slots_list (dslots, cpl);
  nfields = SCM_MAKINUM (scm_ilength (slots));
  g_n_s = compute_getters_n_setters (slots);

  SCM_SLOT(z, scm_si_name)	        = name;
  SCM_SLOT(z, scm_si_direct_supers)     = dsupers;
  SCM_SLOT(z, scm_si_direct_slots)      = dslots;
  SCM_SLOT(z, scm_si_direct_subclasses) = SCM_EOL;
  SCM_SLOT(z, scm_si_direct_methods)    = SCM_EOL;
  SCM_SLOT(z, scm_si_cpl)               = cpl;
  SCM_SLOT(z, scm_si_slots)	        = slots;
  SCM_SLOT(z, scm_si_nfields)	        = nfields;
  SCM_SLOT(z, scm_si_getters_n_setters) = g_n_s;
  SCM_SLOT(z, scm_si_redefined)         = SCM_BOOL_F;
  SCM_SLOT(z, scm_si_environment)
    = scm_top_level_env (SCM_CDR (scm_top_level_lookup_closure_var));

  /* Add this class in the direct-subclasses slot of dsupers */
  {
    SCM tmp;
    for (tmp = dsupers; SCM_NNULLP(tmp); tmp = SCM_CDR(tmp))
      SCM_SLOT(SCM_CAR(tmp), scm_si_direct_subclasses)
	= scm_cons(z, SCM_SLOT(SCM_CAR(tmp), scm_si_direct_subclasses));
  }

  /* Support for the underlying structs: */
  SCM_SET_CLASS_FLAGS (z, (class == Entity_class
			   ? (SCM_OBJF_GOOPS
			      | SCM_CLASSF_OPERATOR
			      | SCM_CLASSF_ENTITY)
			   : class == Operator_class
			   ? SCM_OBJF_GOOPS | SCM_CLASSF_OPERATOR
			   : SCM_OBJF_GOOPS));
  scm_sys_inherit_magic_x (z, dsupers);
  scm_sys_prep_layout_x (z);
  return z;
}

/******************************************************************************/

static void
create_Top_Object_Class(void)
{
  SCM slots_of_class = scm_cons (Intern ("layout"),
		       scm_cons (Intern ("vcell"),
		       scm_cons (Intern ("vtable"),
		       scm_cons (Intern ("print"),
		       scm_cons (Intern ("procedure0"),
		       scm_cons (Intern ("procedure1"),
		       scm_cons (Intern ("procedure2"),
		       scm_cons (Intern ("procedure3"),
		       scm_cons (Intern ("name"),
		       scm_cons (Intern ("direct-supers"),
		       scm_cons (Intern ("direct-slots"),
		       scm_cons (Intern ("direct-subclasses"),
		       scm_cons (Intern ("direct-methods"),
		       scm_cons (Intern ("cpl"),
		       scm_cons (Intern ("slots"),
		       scm_cons (Intern ("nfields"),
		       scm_cons (Intern ("getters-n-setters"),
		       scm_cons (Intern ("redefined"),
		       scm_cons (Intern ("environment"),
				 SCM_EOL)))))))))))))))))));

  /**** <Class> ****/
  SCM cs = scm_makfrom0str (SCM_METACLASS_GOOPS_LAYOUT);
  SCM cl = scm_make_struct_layout (cs);
  SCM name = Intern ("<class>");
  Class = scm_permanent_object (scm_make_vtable_vtable (cl,
							SCM_INUM0,
							SCM_EOL));
  SCM_SET_CLASS_FLAGS (Class, (SCM_OBJF_GOOPS
			       | SCM_OBJF_INSTANCE
			       | SCM_CLASSF_METACLASS));

  SCM_SLOT(Class, scm_si_name) 		 = name;
  SCM_SLOT(Class, scm_si_direct_supers)	 = SCM_EOL;  /* will be changed */
  SCM_SLOT(Class, scm_si_direct_slots)	 = slots_of_class;
  SCM_SLOT(Class, scm_si_direct_subclasses)= SCM_EOL;
  SCM_SLOT(Class, scm_si_direct_methods) = SCM_EOL;  
  SCM_SLOT(Class, scm_si_cpl)		 = SCM_EOL;  /* will be changed */
  SCM_SLOT(Class, scm_si_slots)		 = slots_of_class;
  SCM_SLOT(Class, scm_si_nfields)	 = SCM_MAKINUM (SCM_N_CLASS_SLOTS);
  SCM_SLOT(Class, scm_si_getters_n_setters)
    = compute_getters_n_setters (slots_of_class);
  SCM_SLOT(Class, scm_si_redefined) 	 = SCM_BOOL_F;
  SCM_SLOT(Class, scm_si_environment) = scm_top_level_env (SCM_CDR (scm_top_level_lookup_closure_var));

  DEFVAR(name, Class);

  /**** <Top> ****/
  name = Intern ("<top>");
  Top = scm_permanent_object (scm_basic_make_class (Class,
						    name,
						    SCM_EOL,
						    SCM_EOL));

  DEFVAR(name, Top);
  
  /**** <Object> ****/
  name	 = Intern("<object>");
  Object = scm_permanent_object (scm_basic_make_class (Class,
						       name,
						       SCM_LIST1 (Top),
						       SCM_EOL));

  DEFVAR (name, Object);

  /* <top> <object> and <class> were partially initialized. Correct them here */
  SCM_SLOT (Object, scm_si_direct_subclasses) = SCM_LIST1 (Class);

  SCM_SLOT (Class, scm_si_direct_supers)      = SCM_LIST1 (Object);
  SCM_SLOT (Class, scm_si_cpl)		    = SCM_LIST3 (Class, Object, Top);
}

/******************************************************************************/

SCM_PROC (s_instance_p, "instance?", 1, 0, 0, scm_instance_p);

SCM
scm_instance_p (SCM obj)
{
  return SCM_NIMP (obj) && SCM_INSTANCEP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC (s_class_of, "class-of", 1, 0, 0, scm_class_of);

SCM
scm_class_of (SCM x)
{
  switch (SCM_ITAG3 (x))
    {
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      return Integer;

    case scm_tc3_imm24:
      if (SCM_ICHRP (x))
	return Char;
      else
	{
	  switch (SCM_ISYMNUM (x))
	    {
	    case SCM_ISYMNUM (SCM_BOOL_F):
	    case SCM_ISYMNUM (SCM_BOOL_T):
	      return Boolean;
	    case SCM_ISYMNUM (SCM_EOL):
	      return Null;
	    default:
	      return Unknown;
	    }
	}

    case scm_tc3_cons:
      switch (SCM_TYP7 (x))
	{
	case scm_tcs_cons_nimcar:
	  return Pair;
	case scm_tcs_closures:
	  return Procedure;
	case scm_tcs_symbols:
	  return Symbol;
	case scm_tc7_vector:
	case scm_tc7_wvect:
	case scm_tc7_bvect:
	case scm_tc7_byvect:
	case scm_tc7_svect:
	case scm_tc7_ivect:
	case scm_tc7_uvect:
	case scm_tc7_fvect:
	case scm_tc7_dvect:
	case scm_tc7_cvect:
	  return Vector;
	case scm_tc7_string:
	case scm_tc7_substring:
	  return String;
	case scm_tc7_asubr:
	case scm_tc7_subr_0:
	case scm_tc7_subr_1:
	case scm_tc7_cxr:
	case scm_tc7_subr_3:
	case scm_tc7_subr_2:
	case scm_tc7_rpsubr:
	case scm_tc7_subr_1o:
	case scm_tc7_subr_2o:
	case scm_tc7_lsubr_2:
	case scm_tc7_lsubr:
	  return Procedure;

	case scm_tc7_port:
	  return Unknown;
	case scm_tc7_smob:
	  {
	    SCM type = SCM_TYP16 (x);
	    if (type == scm_tc16_flo)
	      {
		if (SCM_CAR (x) & SCM_IMAG_PART)
		  return Complex;
		else
		  return Real;
	      }
	    else if (type == scm_tc16_bigpos || type == scm_tc16_bigneg)
	      return Integer;
	    else if (type == scm_tc16_kw)
	      return Keyword;
	    else
	      return Unknown;
	  }
	case scm_tcs_cons_gloc:
	  /* must be a struct */
	  if (SCM_OBJ_FLAGS (x) & SCM_OBJF_GOOPS)
	  {
	    SCM c;
	    TEST_CHANGE_CLASS (x, c);
	    return SCM_CLASS_OF (x);
	  }
	default:
	  if (SCM_CONSP (x))
	    return Pair;
	  else
	    return Unknown;
	}

    case scm_tc3_cons_gloc:
    case scm_tc3_tc7_1:
    case scm_tc3_tc7_2:
    case scm_tc3_closure:
      /* Never reached */
      break;
    }
  return Unknown;
}

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
  return scm_slot_ref(obj, Intern ("cpl"));
}

SCM_PROC (s_class_slots, "class-slots", 1, 0, 0, scm_class_slots);

SCM
scm_class_slots (SCM obj)
{
  SCM_ASSERT (SCM_NIMP (obj) && CLASSP (obj),
	      obj, SCM_ARG1, s_class_slots);
  return scm_slot_ref(obj, Intern ("slots"));
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
  return scm_slot_ref(obj, Intern ("name"));
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

  return SCM_SLOT (obj, i);
}

SCM_PROC (s_sys_fast_slot_set_x, "%fast-slot-set!", 3, 0, 0, scm_sys_fast_slot_set_x);

SCM
scm_sys_fast_slot_set_x (SCM obj, SCM index, SCM value)
{
  register long i;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_sys_fast_slot_ref);
  SCM_ASSERT (SCM_INUMP (index), index, SCM_ARG2, s_sys_fast_slot_ref);
  i = SCM_INUM (index);
  SCM_ASSERT (i >= 0 && i < SCM_NUMBER_OF_SLOTS (obj),
	      index, SCM_OUTOFRANGE, s_sys_fast_slot_ref);

  SCM_SLOT(obj, i) = value;
  return SCM_UNSPECIFIED;
}

/** Utilities **/

static SCM
get_slot_value (SCM class, SCM obj, SCM slot_name)
{
  register SCM l;

  for (l = SCM_ACCESSORS_OF (obj); SCM_NNULLP (l); l = SCM_CDR (l))
    {
      if (SCM_CAAR (l) == slot_name)
	{
	  l = SCM_CDDAR (l);
	  /* Two cases here:
	   *	- l is an integer (the offset of this slot in the slots vector)
	   *	- otherwise (car l) is the getter function to apply
	   */
	  if (SCM_INUMP (l))
	    return SCM_SLOT (obj, SCM_INUM (l));
	  else
	    {
	      /* We must evaluate (apply (car l) (list obj)) 
	       * where (car l) is known to be a closure of arity 1  */
	      register SCM code, next, env;

	      code = SCM_CAR (l);
	      env  = SCM_EXTEND_ENV (SCM_CAR (SCM_CODE (code)),
				     SCM_LIST1 (obj),
				     SCM_ENV (code));
	      /* Evaluate the closure body */
	      code = SCM_CDR (SCM_CODE (code));
	      next = code;
	      while (SCM_NNULLP (next = SCM_CDR (next)))
		{
		  if (SCM_NIMP (SCM_CAR (code)))
		    l = SCM_XEVAL (SCM_CAR (code), env);
		  code = next;
		}
	      return SCM_XEVALCAR (code, env);
	    }
	}
    }
  return CALL_GF3 ("slot-missing", class, obj, slot_name);
}

static SCM
set_slot_value (SCM class, SCM obj, SCM slot_name, SCM value)
{
  register SCM l;

  for (l = SCM_ACCESSORS_OF (obj); SCM_NNULLP (l); l = SCM_CDR (l))
    {
      if (SCM_CAAR (l) == slot_name)
	{
	  l = SCM_CDDAR (l);
	  /* Two cases here:
	   *	- l is an integer (the offset of this slot in the slots vector)
	   *	- otherwise (cadr l) is the setter function to apply
	   */
	  if (SCM_INUMP (l))
	    SCM_SLOT (obj, SCM_INUM (l)) = value;
	  else
	    {
	      /* We must evaluate (apply (cadr l) (list obj value))
	       * where (cadr l) is known to be a closure of arity 2  */
	      register SCM code, env;

	      code = SCM_CADR (l);
	      env  = SCM_EXTEND_ENV (SCM_CAR (SCM_CODE (code)),
				     SCM_LIST2 (obj, value),
				     SCM_ENV (code));
	      /* Evaluate the closure body */
	      code = SCM_CDR (SCM_CODE (code));
	      while (SCM_NNULLP (code))
		{
		  if (SCM_NIMP (SCM_CAR (code)))
		    SCM_XEVAL (SCM_CAR (code), env);
		  code = SCM_CDR (code);
		}
	    }
	  return SCM_UNSPECIFIED;
	}
    }
  return CALL_GF4 ("slot-missing", class, obj, slot_name, value);
}

static void
set_slot_value_if_unbound (SCM class, SCM obj, SCM slot_name, SCM form)
{
  SCM old_val = get_slot_value(class, obj, slot_name);
  
  if (SCM_GOOPS_UNBOUNDP (old_val))
    set_slot_value (class, obj,
		    slot_name,
		    scm_apply (form, SCM_EOL, SCM_EOL));
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

  res = get_slot_value (class, obj, slot_name);
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
  return set_slot_value (class, obj, slot_name, value);
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

  return (SCM_GOOPS_UNBOUNDP (get_slot_value (class, obj, slot_name))
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

  res = get_slot_value (class, obj, slot_name);
  if (SCM_GOOPS_UNBOUNDP (res))
    return CALL_GF3 ("slot-unbound", class, obj, slot_name);
  return res;
}

SCM_PROC (s_slot_set_x, "slot-set!", 3, 0, 0, scm_slot_set_x);

SCM
scm_slot_set_x (SCM obj, SCM slot_name, SCM value)
{
  SCM class;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_slot_set_x);
  TEST_CHANGE_CLASS(obj, class);

  return set_slot_value (class, obj, slot_name, value);
}

SCM_PROC (s_slot_bound_p, "slot-bound?", 2, 0, 0, scm_slot_bound_p);

SCM
scm_slot_bound_p (SCM obj, SCM slot_name)
{
  SCM class;

  SCM_ASSERT (SCM_NIMP (obj) && SCM_INSTANCEP (obj),
	      obj, SCM_ARG1, s_slot_bound_p);
  TEST_CHANGE_CLASS(obj, class);

  return (SCM_GOOPS_UNBOUNDP (get_slot_value (class, obj, slot_name))
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

SCM_PROC (s_sys_allocate_instance, "%allocate-instance", 1, 0, 0, scm_sys_allocate_instance);

SCM
scm_sys_allocate_instance (SCM class)
{
  int type, n, i = 0;
  SCM z;

  SCM_ASSERT (SCM_NIMP (class) && CLASSP (class),
	      class, SCM_ARG1, s_sys_allocate_instance);
 
  z = scm_make_struct (class, SCM_INUM0, SCM_EOL);
  if (class == Generic)
    type = SCM_OBJF_GOOPS | SCM_OBJF_PURE_GENERIC;
  else if (class == Accessor)
    type = SCM_OBJF_GOOPS | SCM_OBJF_ACCESSOR;
  else if (class == Simple_method)
    type = SCM_OBJF_GOOPS | SCM_OBJF_SIMPLE_METHOD;
  else
    {
      type = SCM_OBJF_GOOPS | SCM_OBJF_INSTANCE;
      if (SCM_CLASS_FLAGS (class) & SCM_CLASSF_METACLASS)
	{
	  SCM_SLOT (z, scm_si_print) = SCM_GOOPS_UNBOUND;
	  i = 8;
	  if (SCM_SUBCLASSP (class, Entity_class))
	    SCM_SET_CLASS_FLAGS (z, SCM_CLASSF_OPERATOR | SCM_CLASSF_ENTITY);
	  else if (SCM_SUBCLASSP (class, Operator_class))
	    SCM_SET_CLASS_FLAGS (z, SCM_CLASSF_OPERATOR);
	}
    }

  SCM_SET_OBJ_FLAGS (z, type);
  /* Set all the slots except the first to unbound */
  n = SCM_INUM (SCM_SLOT (class, scm_si_nfields));
  for (; i < n; i++)
    SCM_SLOT (z, i) = SCM_GOOPS_UNBOUND;
  return z;
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
  SCM tmp;

  SCM_ASSERT (SCM_NIMP (old) && SCM_INSTANCEP (old),
	      old, SCM_ARG1, s_sys_modify_instance);
  SCM_ASSERT (SCM_NIMP (new) && SCM_INSTANCEP (new),
	      new, SCM_ARG2, s_sys_modify_instance);

  /* Exchange the data contained in old and new. We exchange rather than 
   * scratch the old value with new to be correct with GC
   */
  tmp = SCM_CAR (old);
  SCM_SETCAR (old, SCM_CAR (new));
  SCM_SETCAR (new, tmp);
  tmp = SCM_CDR (old);
  SCM_SETCDR (old, SCM_CDR (new));
  SCM_SETCDR (new, tmp);
  return SCM_UNSPECIFIED;
}

/* Kept for compatibility with STKlos. */

SCM_PROC (s_stklos_version, "stklos-version", 0, 0, 0, stklos_version);

SCM
stklos_version (void)
{
  return scm_makfrom0str (STKLOS_VERSION);
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

static char s_apply_next_method[] = "apply-next-method";
static SCM scm_f_apply_next_method;

/*fixme*: next-methods should be a subclass of <method>.  They
  shouldn't need this kind of special treatment but should be handled
  as entities directly in the evaluator.  */

SCM
scm_make_next_method (SCM methods, SCM args, SCM gf)
{
  register SCM z, l;

  /* if gf is SCM_EOL, args already contains the GF in front of args.
   * This saves the cost of a a cons. This situation is very frequent since
   * most of the time next-method is called without parameter and we just have 
   * to propagate the previous args.
   */
  l = (SCM_NULLP(gf))? args : scm_cons(gf, args);

  SCM_NEWCELL (z);
  SCM_DEFER_INTS;
  SCM_SETCHARS (z, scm_must_malloc (3 * sizeof (SCM), "compiled-closure"));
  SCM_SETLENGTH (z, 3, scm_tc7_cclo);
  SCM_VELTS (z)[0] = scm_f_apply_next_method;
  SCM_VELTS (z)[1] = methods;
  SCM_VELTS (z)[2] = l;
  SCM_ALLOW_INTS;
  return z;
}

SCM
scm_apply_next_method (SCM args)
{
  SCM next = SCM_CAR (args);
  SCM methods = NXT_MTHD_METHODS (next);
  SCM tmp;
  args = SCM_CDR (args);
  
  if (SCM_NULLP (args))
    {
      tmp   = NXT_MTHD_ARGS (next);
      args  = SCM_CDR (tmp);
    }
  else
    tmp  = scm_cons (SCM_CAR (NXT_MTHD_ARGS (next)), args);
  
  if (SCM_NULLP (methods))
    {
      SCM gf = SCM_CAR (NXT_MTHD_ARGS (next));
    
      return CALL_GF2 ("no-next-method", gf, args);    
    }
  else
    {
      SCM m        = SCM_CAR (methods);
      SCM new_next = scm_make_next_method (SCM_CDR(methods),tmp, SCM_EOL);
    
      /* m is the function to call with args. */
      return scm_apply (SCM_METHOD (m)->procedure,
			scm_cons (new_next, args),
			SCM_EOL);
    }
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
		      "Internal error in applicable: bad list %S",
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
sort_applicable_methods(SCM method_list, int size, SCM *targs)
{
  int i, j, incr;
  SCM *v, vector = SCM_EOL;
  SCM buffer[BUFFSIZE];
  SCM save = method_list;

  /* For reasonably sized method_lists we can try to avoid all the
   * consing and reorder the list in place...
   * This idea is due to David McClain <Dave_McClain@msn.com>
   */
  if (size <= BUFFSIZE) {
    for(i=0;  i < size; i++) {
      buffer[i]   = SCM_CAR (method_list);
      method_list = SCM_CDR (method_list);
    }
    v = buffer;
  } 
  else {
    /* Too many elements in method_list to keep everything locally */
    vector = scm_i_vector2list (save, size);
    v      = SCM_VELTS (vector);
  }

  /* Use a simple shell sort since it is generally faster than qsort on 
   * small vectors (which is probably mostly the case when we have to
   * sort a list of applicable methods).
   */
  for (incr = size / 2; incr; incr /= 2) {
    for (i = incr; i < size; i++) {
      for (j = i-incr ;j >= 0; j -= incr) {
	if (more_specificp(v[j], v[j+incr], targs)) break;
	else {
	  SCM tmp   = v[j+incr];
	  v[j+incr] = v[j];
	  v[j]	    = tmp;
	}
      }
    }
  }

  if (size <= BUFFSIZE) {
    /* We did it in locally, so restore the original list (reordered) in-place */
    for(i=0, method_list=save; i < size; i++, v++) {
      SCM_CAR(method_list) = *v;
      method_list      = SCM_CDR(method_list);
    }
    return save;
  }
  /* If we are here, that's that we did it the hard way... */ 
  return scm_vector_to_list (vector);
}

SCM
scm_compute_applicable_methods (SCM gf, SCM args, int len, int scm_find_method)
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
      for (i = 0, fl = SPEC_OF (SCM_CAR (l)); ; i++, fl = SCM_CDR (fl))
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
      if (scm_find_method)
	return SCM_BOOL_F;
      CALL_GF2("no-applicable-method", gf, save);
      /* if we are here, it's because no-applicable-method hasn't signaled an error */
      return SCM_EOL;
    }
  return (count == 1
	  ? applicable
	  : sort_applicable_methods (applicable, count, types));
}

#if 0
/* Not used */
static SCM apply_methods(SCM gf, SCM methods, SCM args)
{
  SCM m, next;
  
  if (SCM_NULLP(methods)) {
    /* 
     * methods can be SCM_EOL if we have a no-applicable-method handler which 
     * doesn't signal an error (or dont ends with a call to next-method)
     * In this case return an undefined value
     */
    return SCM_UNSPECIFIED;
  }
  
  m    = SCM_CAR(methods);
  next = SCM_FASTMETHODP(m) ? SCM_UNDEFINED : scm_make_next_method (SCM_CDR(methods), args, gf);

  /* Next-method is set to UNBOUND for simple_method and accessors */
  return scm_apply (SCM_METHOD (m)->procedure,
		    scm_cons (next, args),
		    SCM_EOL);
}

/* Not used */
SCM STk_apply_generic(SCM gf, SCM args)
{
  if (NGENERICP(gf)) 			 Err("apply: bad generic function", gf);
  if (SCM_NULLP(SCM_SLOT(gf, scm_si_methods))) CALL_GF2("no-method", gf, args);

  return 
    apply_methods(gf,
		  STk_compute_applicable_methods(gf,args,scm_length(args),0),
		  args);
}

/* Not used */
SCM STk_apply_user_generic(SCM gf, SCM args)
{
  if (NGENERICP(gf)) Err("apply: bad generic function", gf);
  return CALL_GF2("apply-generic", gf, args);
}
#endif

static char s_apply_generic_0[] = "apply-generic-0";
static SCM scm_f_apply_generic_0;
static SCM
apply_generic_0 (SCM gf)
{
  SCM methods;
  if (SCM_NULLP (SCM_SLOT (gf, scm_si_methods)))
    scm_apply (GETVAR (Intern ("no-method")),
	       SCM_LIST2 (gf, SCM_EOL),
	       SCM_EOL);
  methods = scm_compute_applicable_methods (gf, SCM_EOL, 0, 0);
  /* methods is the list of applicable methods. Apply the
   * first one with the tail of the list as first
   * parameter (next-method). If fct is NIL, that's because
   * the no-applicable-method triggered didn't call error.
   */
  if (SCM_NULLP (methods))
    return SCM_UNSPECIFIED;
  /*fixme* Should be made tail-recursive!  Methods are closures in goops. */
  return scm_apply (SCM_METHOD (SCM_CAR (methods))->procedure,
		    SCM_LIST1 (SCM_FASTMETHODP (SCM_CAR (methods))
			       ? SCM_BOOL_F
			       : scm_make_next_method (SCM_CDR (methods),
						       SCM_EOL,
						       gf)),
		    SCM_EOL);
}

static char s_apply_generic_1[] = "apply-generic-1";
static SCM scm_f_apply_generic_1;
static SCM
apply_generic_1 (SCM gf, SCM arg1)
{
  SCM methods, args = SCM_LIST1 (arg1);
  if (SCM_NULLP (SCM_SLOT (gf, scm_si_methods)))
    scm_apply (GETVAR (Intern ("no-method")),
	       SCM_LIST2 (gf, args),
	       SCM_EOL);
  methods = scm_compute_applicable_methods (gf, args, 1, 0);
  if (SCM_NULLP (methods))
    return SCM_UNSPECIFIED;
  return scm_apply (SCM_METHOD (SCM_CAR (methods))->procedure,
		    SCM_LIST2 (SCM_FASTMETHODP (SCM_CAR (methods))
			       ? SCM_BOOL_F
			       : scm_make_next_method (SCM_CDR (methods),
						       args,
						       gf),
			       arg1),
		    SCM_EOL);
}

static char s_apply_generic_2[] = "apply-generic-2";
static SCM scm_f_apply_generic_2;
static SCM
apply_generic_2 (SCM gf, SCM arg1, SCM arg2)
{
  SCM methods, args = SCM_LIST2 (arg1, arg2);
  if (SCM_NULLP (SCM_SLOT (gf, scm_si_methods)))
    scm_apply (GETVAR (Intern ("no-method")),
	       SCM_LIST2 (gf, args),
	       SCM_EOL);
  methods = scm_compute_applicable_methods (gf, args, 2, 0);
  if (SCM_NULLP (methods))
    return SCM_UNSPECIFIED;
  return scm_apply (SCM_METHOD (SCM_CAR (methods))->procedure,
		    SCM_LIST3 (SCM_FASTMETHODP (SCM_CAR (methods))
			       ? SCM_BOOL_F
			       : scm_make_next_method (SCM_CDR (methods),
						       args,
						       gf),
			       arg1,
			       arg2),
		    SCM_EOL);
}

static char s_apply_generic_3[] = "apply-generic-3";
static SCM scm_f_apply_generic_3;
static SCM
apply_generic_3 (SCM gf, SCM arg1, SCM rest)
{
  SCM methods, args = scm_cons (arg1, rest);
  if (SCM_NULLP (SCM_SLOT (gf, scm_si_methods)))
    scm_apply (GETVAR (Intern ("no-method")),
	       SCM_LIST2 (gf, args),
	       SCM_EOL);
  methods = scm_compute_applicable_methods (gf, args, scm_ilength (args), 0);
  if (SCM_NULLP (methods))
    return SCM_UNSPECIFIED;
  return scm_apply (SCM_METHOD (SCM_CAR (methods))->procedure,
		    scm_cons (SCM_FASTMETHODP (SCM_CAR (methods))
			      ? SCM_BOOL_F
			      : scm_make_next_method (SCM_CDR (methods),
						      args,
						      gf),
			       args),
		    SCM_EOL);
}

#if 0
static SCM
internal_apply_generic ()
{
	        tmp = eval_args(tmp, env);
	        if (SCM_PUREGENERICP(fct)) {
		  /* Do it in C */
		  SCM methods;
		  
		  if (NULLP(SCM_SLOT(fct, scm_si_methods)))
		    Apply(STk_STklos_value(Intern("no-method")), LIST2(fct, tmp));

		  methods = STk_compute_applicable_methods(fct, tmp, len, 0);
		  /* methods is the list of applicable methods. Apply the
		   * first one with the tail of the list as first
		   * parameter (next-method). If fct is NIL, that's because
		   * the no-applicable-method triggered didn't call error.
		   */
		  if (NULLP(methods)) RETURN(UNDEFINED);
		  tmp = Cons(SCM_FASTMETHODP(CAR(methods))? 
			     		UNBOUND: 
			     		STk_make_next_method(CDR(methods),tmp,fct),
			     tmp);
		  fct = SCM_SLOT(CAR(methods), scm_si_procedure);
		  env = extend_env(fct, tmp, x, len+1);
		  tmp = CLOSBODY(fct);
		  goto Begin;
		}
		else
		  /* Do it in Scheme */
		  RETURN(STk_apply_user_generic(fct, tmp));
}
#endif
		
/******************************************************************************
 *
 * A simple make (which will be redefined later in Scheme)
 * This version handles only creation of gf, methods and classes (no instances)
 *
 * Since this code will disappear when Goops will be fully booted, 
 * no precaution is taken to be efficient.
 *
 ******************************************************************************/

static char k_name[] 	     = "-name";		/* Use vars since makekey patches */
static char k_specializers[] = "-specializers"; /* its argument. This avoids the */
static char k_procedure[]    = "-procedure";	/* -fwritable_string */
static char k_dsupers[]	     = "-dsupers";
static char k_slots[]	     = "-slots";
static char k_gf[]	     = "-generic-function";

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
  
  if (class == Generic)
    {
      z = scm_make_struct (class,
			   SCM_INUM0,
			   SCM_LIST2 (scm_i_get_keyword (scm_makekey (k_name),
							 args,
							 len - 1,
							 Intern ("???"),
							 s_make),
				      SCM_EOL));
      scm_set_object_procedure_x (z, SCM_LIST4 (scm_f_apply_generic_0,
						scm_f_apply_generic_1,
						scm_f_apply_generic_2,
						scm_f_apply_generic_3));
      SCM_SET_OBJ_FLAGS (z, SCM_OBJF_GOOPS | SCM_OBJF_PURE_GENERIC);
    }
  else
    {
      z = scm_sys_allocate_instance (class);

      if (class == Method || class == Simple_method || class == Accessor)
	{
	  SCM_SLOT (z, scm_si_generic_function) =  
	    scm_i_get_keyword (scm_makekey (k_gf),
			     args,
			     len - 1,
			     SCM_BOOL_F,
			     s_make);
	  SCM_SLOT (z, scm_si_specializers) =  
	    scm_i_get_keyword (scm_makekey (k_specializers),
			     args,
			     len - 1,
			     SCM_EOL,
			     s_make);
	  SCM_SLOT (z, scm_si_procedure) =
	    scm_i_get_keyword (scm_makekey (k_procedure),
			     args,
			     len - 1,
			     SCM_EOL,
			     s_make);
	}
      else
	{
	  /* In all the others case, make a new class .... No instance here */
	  SCM_SLOT (z, scm_si_name) = 
	    scm_i_get_keyword (scm_makekey(k_name),
			       args,
			       len - 1,
			       Intern ("???"),
			       s_make);
	  SCM_SLOT (z, scm_si_direct_supers) = 
	    scm_i_get_keyword (scm_makekey(k_dsupers),
			       args,
			       len - 1,
			       SCM_EOL,
			       s_make);
	  SCM_SLOT (z, scm_si_direct_slots)  = 
	    scm_i_get_keyword (scm_makekey(k_slots),
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
		    "no methods for generic %S",
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
   
   *var = scm_permanent_object (scm_basic_make_class(meta,
						     tmp,
						     SCM_LIST1 (super),
						     slots));
   DEFVAR(tmp, *var);
}

static void
make_standard_classes (void)
{
  SCM tmp1 = SCM_LIST3 (Intern("generic-function"), 
			Intern("specializers"), 
			Intern("procedure"));
  SCM tmp2 = SCM_LIST2 (Intern("name"),
			Intern("methods"));

  /* Generic functions classes */
  make_stdcls(&Procedure_class, "<procedure-class>", Class, Class, 	     SCM_EOL);
  make_stdcls(&Entity_class,    "<entity-class>",    Class, Procedure_class, SCM_EOL);
  make_stdcls(&Operator_class,  "<operator-class>",  Class, Procedure_class, SCM_EOL);
  make_stdcls(&Method,		"<method>",	     Class, Object,	     tmp1);
  make_stdcls(&Simple_method,	"<simple-method>",   Class, Method,	     SCM_EOL);
  make_stdcls(&Accessor,	"<accessor-method>", Class, Simple_method,   SCM_EOL);
  make_stdcls(&Generic,		"<generic>",	     Entity_class, Object,   tmp2);

  /* Primitive types classes */
  make_stdcls(&Boolean, 	"<boolean>",	Class, 		 Top, 	    SCM_EOL);
  make_stdcls(&Char,		"<char>",	Class, 		 Top,	    SCM_EOL);
  make_stdcls(&Liste,		"<list>",	Class, 		 Top,	    SCM_EOL);
  make_stdcls(&Pair,		"<pair>",	Class, 		 Liste,	    SCM_EOL);
  make_stdcls(&Null,		"<null>", 	Class, 		 Liste,	    SCM_EOL);
  make_stdcls(&String,		"<string>",	Class, 		 Top,	    SCM_EOL);
  make_stdcls(&Symbol,		"<symbol>",	Class, 		 Top,	    SCM_EOL);
  make_stdcls(&Vector,		"<vector>",	Class, 		 Top,	    SCM_EOL);
  make_stdcls(&Number,		"<number>",	Class,		 Top,	    SCM_EOL);
  make_stdcls(&Complex,		"<complex>",	Class, 		 Number,    SCM_EOL);
  make_stdcls(&Real,		"<real>",	Class, 		 Complex,   SCM_EOL);
  make_stdcls(&Integer,		"<integer>",	Class, 		 Real,	    SCM_EOL);
  make_stdcls(&Keyword,		"<keyword>",	Class, 		 Top,	    SCM_EOL);
  make_stdcls(&Unknown,		"<unknown>",	Class, 		 Top,	    SCM_EOL);
  make_stdcls(&Procedure,	"<procedure>",	Procedure_class, Top, 	    SCM_EOL);
#ifdef USE_TK
  make_stdcls(&Widget,		"<widget>",	Procedure_class, Procedure, SCM_EOL);
#endif
}  

#if 0
/**
 *
 * Definition of Extended type classes
 *
 **/

SCM STk_make_extended_class(char *name)
{
  /* 
   * This function is called when a new extended type is defined and returns 
   * a new class for it. Furthermore, it defines a global variable for this class.
   * If Goops is not initialized, it returns SCM_EOL. 
   * Extended types defined before initialization of Goops will be done
   * when Goops is inited by "define_extended_type_classes"
   */
  if (initialized) {
    SCM tmp1, tmp2;
    char buffer[200];

    sprintf(buffer, "<%s>", name);
    tmp1 	= Intern(buffer);
    tmp2 	= STk_basic_make_class(Class, tmp1, SCM_LIST1(Top), SCM_EOL);
    DEFVAR(tmp1, tmp2);
    return tmp2;
  }
  return SCM_EOL;
}

static void define_extended_type_classes(void)
{
  /* 
   * This function is called when Goops is initialized. It performs
   * the definition of classes for extended types defined before Goops
   * loading
   */
  int i;

  initialized = 1;
  for (i = tc_start_extd; ; i++) {
    char *name = STk_get_extended_name(i);
    
    if (!name) return;
    STk_register_extended_class(STk_make_extended_class(name), i);
  }
}

static void add_primitive(char *name, int type, void *fct_ptr)
{
  STk_add_new_primitive(name, type, fct_ptr);
  STk_export_symbol(Intern(name), STklos);
}
#endif

/*===========================================================================*/

void
scm_init_goops (void)
{
  SCM the_unbound_value = SCM_CAR (scm_intern0 ("the-unbound-value"));
  SCM goops = SCM_CAR (scm_intern0 ("goops"));
  SCM goops_module = scm_make_module (SCM_LIST2 (goops, goops));
  SCM old_module = scm_select_module (goops_module);
  scm_goops_lookup_closure = scm_module_lookup_closure (goops_module);

#include "goops.x"

  scm_goops_the_unbound_value
    = scm_permanent_object (scm_cons (the_unbound_value, SCM_EOL));
  
  scm_f_apply_next_method
    = scm_permanent_object (scm_make_subr (s_apply_next_method,
					   scm_tc7_lsubr,
					   scm_apply_next_method));
  scm_f_apply_generic_0
    = scm_permanent_object (scm_make_subr (s_apply_generic_0,
					   scm_tc7_subr_1,
					   apply_generic_0));
  scm_f_apply_generic_1
    = scm_permanent_object (scm_make_subr (s_apply_generic_1,
					   scm_tc7_subr_2,
					   apply_generic_1));
  scm_f_apply_generic_2
    = scm_permanent_object (scm_make_subr (s_apply_generic_2,
					   scm_tc7_subr_3,
					   apply_generic_2));
  scm_f_apply_generic_3
    = scm_permanent_object (scm_make_subr (s_apply_generic_3,
					   scm_tc7_lsubr_2,
					   apply_generic_3));

  create_Top_Object_Class ();
  make_standard_classes ();

  scm_select_module (old_module);
}

void
scm_init_goops_goopscore_module ()
{
  scm_register_module_xxx ("goops goopscore", scm_init_goops);
}
