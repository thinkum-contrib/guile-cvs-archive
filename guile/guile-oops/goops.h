/* classes: h_files */

#ifndef GOOPSH
#define GOOPSH
/*	Copyright (C) 1998, 1999 Free Software Foundation, Inc.
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
 * This file is based upon stklos.h from the STk distribution by
 * Erick Gallesio <eg@unice.fr>.
 */

#include <libguile.h>
#include <goops-snarf.h>

/*
 * scm_class_class
 */

#define SCM_CLASS_CLASS_LAYOUT "pruosrpwpopopopopopwururururururururpwpwpwpwpwpwpwpwpwpwpwpw"

#define scm_si_layout		  0 /* the struct layout */
#define scm_si_vcell		  1 
#define scm_si_vtable		  2 
#define scm_si_print		  3 /* the struct print closure */
#define scm_si_procedure0	  4 
#define scm_si_procedure1	  5 
#define scm_si_procedure2	  6 
#define scm_si_procedure3	  7 
#define scm_si_setter		  8 

#define scm_si_goops_fields	  9

/* Defined in libguile/objects.c:
#define scm_si_redefined	  9    The class to which class was redefined.
#define scm_si_hashsets	 	 10
*/
#define scm_si_name 		 18 /* a symbol */
#define scm_si_direct_supers 	 19 /* (class ...) */
#define scm_si_direct_slots	 20 /* ((name . options) ...) */
#define scm_si_direct_subclasses 21 /* (class ...) */
#define scm_si_direct_methods	 22 /* (methods ...) */
#define scm_si_cpl		 23 /* (class ...) */
#define scm_si_slotdef_class	 24 
#define scm_si_slots		 25 /* ((name . options) ...) */
#define scm_si_name_access	 26 
#define scm_si_keyword_access	 27 
#define scm_si_nfields		 28 /* an integer */
#define scm_si_environment	 29 /* The environment in which class is built  */
#define SCM_N_CLASS_SLOTS	 30

typedef struct scm_method_t {
  SCM generic_function;
  SCM specializers;
  SCM procedure;
} scm_method_t;

#define SCM_METHOD(obj) ((scm_method_t *) SCM_STRUCT_DATA (obj))

#define SCM_CLASSF_SIMPLE_METHOD    (0x004 << 20)
#define SCM_CLASSF_ACCESSOR_METHOD  (0x008 << 20)

/* Defined in libguile/objects.c */
/* #define SCM_CLASSF_PURE_GENERIC  (0x010 << 20) */

#define SCM_CLASSF_FOREIGN	    (0x020 << 20)
#define SCM_CLASSF_METACLASS        (0x040 << 20)

/* Defined in libguile/objects.c */
/* #define SCM_CLASSF_GOOPS_VALID   (0x080 << 20) */
/* #define SCM_CLASSF_GOOPS         (0x100 << 20) */
#define SCM_CLASSF_GOOPS_OR_VALID (SCM_CLASSF_GOOPS | SCM_CLASSF_GOOPS_VALID)

#define SCM_CLASSF_INHERIT	 (~(SCM_CLASSF_PURE_GENERIC \
				    | SCM_CLASSF_SIMPLE_METHOD \
				    | SCM_CLASSF_ACCESSOR_METHOD \
				    | SCM_STRUCTF_LIGHT) \
				  & SCM_CLASSF_MASK)

#define SCM_INST(x)	       SCM_STRUCT_DATA (x)
#define SCM_INST_TYPE(x)       SCM_OBJ_CLASS_FLAGS (x)
/* Also defined in libguuile/objects.c */
#define SCM_CLASS_OF(x)        SCM_STRUCT_VTABLE (x)
#define SCM_ACCESSORS_OF(x)    (SCM_STRUCT_VTABLE_DATA (x)[scm_si_getters_n_setters])
#define SCM_NUMBER_OF_SLOTS(x) (SCM_STRUCT_DATA (x)[scm_struct_i_n_words] \
			       - scm_struct_n_extra_words) \

#define SCM_INSTANCEP(x)       (SCM_STRUCTP (x) \
			       && (SCM_INST_TYPE (x) & SCM_CLASSF_GOOPS))

#define SCM_PUREGENERICP(x)    (SCM_INST_TYPE(x) & SCM_CLASSF_PURE_GENERIC)
#define SCM_SIMPLEMETHODP(x)   (SCM_INST_TYPE(x) & SCM_CLASSF_SIMPLE_METHOD)
#define SCM_ACCESSORP(x)       (SCM_INST_TYPE(x) & SCM_CLASSF_ACCESSOR_METHOD)
#define SCM_FASTMETHODP(x)     (SCM_INST_TYPE(x) \
				& (SCM_CLASSF_ACCESSOR_METHOD \
				   | SCM_CLASSF_SIMPLE_METHOD))

#define SCM_SLOT(x, i)         (SCM_INST(x)[i])
#define SCM_SUBCLASSP(c1, c2)  SCM_NNULLP (scm_sloppy_memq (c2, SCM_SLOT (c1, scm_si_cpl)))
#define SCM_IS_A_P(x, c)       (SCM_NIMP (x) \
				&& SCM_INSTANCEP (x) \
				&& SCM_SUBCLASSP (SCM_CLASS_OF (x), c))

#define SCM_INITIAL_HASH_SIZE	  4
#define SCM_CMETHOD_CODE(cmethod) SCM_CDR (cmethod)
#define SCM_CMETHOD_ENV(cmethod)  SCM_CAR (cmethod)

#define scm_si_getters_n_setters scm_si_name_access

#define scm_si_constructor	 SCM_N_CLASS_SLOTS
#define scm_si_destructor	 SCM_N_CLASS_SLOTS + 1

#define scm_si_methods		 0  /* offset of methods slot in a <generic> */
#define scm_si_n_specialized	 1
#define scm_si_used_by		 2

#define scm_si_generic_function	 0  /* offset of gf    slot in a <method> */
#define scm_si_specializers	 1  /* offset of spec. slot in a <method> */

#define scm_si_procedure 	 2  /* offset of proc. slot in a <method> */
#define scm_si_code_table	 3  /* offset of code. slot in a <method> */

/* C interface */
extern SCM scm_class_foreign_class, scm_class_foreign_object;
extern SCM scm_class_foreign_slot;
extern SCM scm_class_self, scm_class_protected;
extern SCM scm_class_opaque, scm_class_read_only;
extern SCM scm_class_protected_opaque, scm_class_protected_read_only;
extern SCM scm_class_scm;
extern SCM scm_class_int, scm_class_float, scm_class_double;
extern const char *scm_s_slot_set_x;
void scm_load_goops (void);
SCM scm_make_foreign_object (SCM class, SCM initargs);
SCM scm_make_class (SCM meta, char *s_name, SCM supers, size_t size,
		    void * (*constructor) (SCM initargs),
		    size_t (*destructor) (void *));
void scm_add_slot (SCM c, char *slot, SCM slot_class,
		   SCM (*getter) (SCM obj),
		   SCM (*setter) (SCM obj, SCM x),
		   char *accessor_name);
SCM scm_wrap_object (SCM c, void *);
SCM scm_wrap_component (SCM c, SCM obj, void *);
SCM scm_ensure_accessor (SCM name);
void scm_add_method (SCM gf, SCM m);

/* Low level functions exported */
SCM scm_make_next_method (SCM methods, SCM args, SCM gf);
SCM scm_basic_basic_make_class (SCM c, SCM name, SCM dsupers, SCM dslots);
SCM scm_basic_make_class (SCM c, SCM name, SCM dsupers, SCM dslots);

/* Primitives exported */
SCM scm_sys_allocate_instance (SCM c, SCM initargs);
SCM scm_sys_set_object_setter_x (SCM obj, SCM setter);
SCM scm_slot_ref (SCM obj, SCM slot_name);
SCM scm_slot_set_x (SCM obj, SCM slot_name, SCM value);

SCM scm_compute_applicable_methods (SCM gf, SCM args, int len, int scm_find_method);
SCM scm_sys_compute_applicable_methods (SCM gf, SCM args);
SCM scm_m_atslot_ref (SCM xorig, SCM env);
SCM scm_m_atslot_set_x (SCM xorig, SCM env);
SCM scm_m_dispatch (SCM xorig, SCM env);
SCM scm_m_hash_dispatch (SCM xorig, SCM env);
#ifdef GUILE_DEBUG
SCM scm_pure_generic_p (SCM obj);
#endif
extern void scm_init_oop_goops_goopscore_module (void);

SCM scm_sys_compute_slots (SCM c); 
SCM scm_i_get_keyword (SCM key, SCM l, int len, SCM default_value, const char *subr); 
SCM scm_get_keyword (SCM key, SCM l, SCM default_value); 
SCM scm_sys_initialize_object (SCM obj, SCM initargs); 
SCM scm_sys_prep_layout_x (SCM c); 
SCM scm_sys_inherit_magic_x (SCM c, SCM dsupers); 
SCM scm_instance_p (SCM obj); 
SCM scm_class_name (SCM obj); 
SCM scm_class_direct_supers (SCM obj); 
SCM scm_class_direct_slots (SCM obj); 
SCM scm_class_direct_subclasses (SCM obj); 
SCM scm_class_direct_methods (SCM obj); 
SCM scm_class_precedence_list (SCM obj); 
SCM scm_class_slots (SCM obj); 
SCM scm_class_environment (SCM obj); 
SCM scm_generic_function_name (SCM obj); 
SCM scm_generic_function_methods (SCM obj); 
SCM scm_method_generic_function (SCM obj); 
SCM scm_method_specializers (SCM obj); 
SCM scm_method_procedure (SCM obj); 
SCM scm_sys_fast_slot_ref (SCM obj, SCM index); 
SCM scm_sys_fast_slot_set_x (SCM obj, SCM index, SCM value); 
SCM scm_slot_ref_using_class (SCM class, SCM obj, SCM slot_name); 
SCM scm_slot_set_using_class_x (SCM class, SCM obj, SCM slot_name, SCM value); 
SCM scm_slot_bound_using_class_p (SCM class, SCM obj, SCM slot_name); 
SCM scm_slot_exists_using_class_p (SCM class, SCM obj, SCM slot_name); 
SCM scm_slot_bound_p (SCM obj, SCM slot_name); 
SCM scm_slots_exists_p (SCM obj, SCM slot_name); 
SCM scm_sys_modify_instance (SCM old, SCM new); 
SCM scm_sys_modify_class (SCM old, SCM new); 
SCM scm_sys_invalidate_class (SCM class);
SCM scm_make_method_cache (SCM gf);
SCM scm_sys_invalidate_method_cache_x (SCM gf);
SCM stklos_version (void); 
SCM scm_make (SCM args); 
SCM scm_find_method (SCM args); 
SCM scm_sys_method_more_specific_p (SCM m1, SCM m2, SCM targs); 
void scm_init_goops (void); 

#endif /* GOOPSH */
