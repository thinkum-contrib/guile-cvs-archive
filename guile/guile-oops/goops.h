/* classes: h_files */

#ifndef GOOPSH
#define GOOPSH
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
 * This file is based upon stklos.h from the STk distribution by
 * Erick Gallesio <eg@unice.fr>.
 */

#define STKLOS_VERSION	"3.99.1"

#define SCM_METACLASS_GOOPS_LAYOUT "pwpwpopopopopwpwpwpwpwpwpwpwpw"
struct scm_metaclass_goops {
  SCM layout;
  SCM vcell;
  SCM vtable;
  SCM print;
  SCM direct_supers;
  SCM direct_slots;
  SCM proc0;
  SCM proc1;
  SCM proc2;
  SCM proc3;
  SCM name;
  SCM direct_subclasses;
  SCM direct_methods;
  SCM cpl;
  SCM slots;
  SCM nfields;
  SCM getters_n_setters;
  SCM redefined;
  SCM environment;
};

typedef struct scm_method_t {
  SCM generic_function;
  SCM specializers;
  SCM procedure;
} scm_method_t;

#define SCM_METHOD(obj) ((scm_method_t *) SCM_STRUCT_DATA (obj))

#define SCM_OBJF_INSTANCE      (0x00 << 24)
#define SCM_OBJF_GENERIC       (0x01 << 24)
#define SCM_OBJF_SIMPLE_METHOD (0x02 << 24)
#define SCM_OBJF_ACCESSOR      (0x04 << 24)

#define SCM_CLASSF_METACLASS   (0x08 << 24)

#define SCM_OBJF_GOOPS         (0x10 << 24)


#define SCM_INST(x)	       SCM_STRUCT_DATA (x)
#define SCM_INST_TYPE(x)       SCM_OBJ_CLASS_FLAGS (x)
#define SCM_CLASS_OF(x)        SCM_STRUCT_VTABLE (x)
#define SCM_ACCESSORS_OF(x)    (SCM_STRUCT_VTABLE_DATA (x)[scm_si_getters_n_setters])
#define SCM_NUMBER_OF_SLOTS(x) (SCM_STRUCT_DATA (x)[scm_struct_i_n_words] \
			       - scm_struct_n_extra_words) \

#define SCM_INSTANCEP(x)       (SCM_STRUCTP (x) \
			       && (SCM_INST_TYPE (x) & SCM_OBJF_GOOPS))

#define SCM_PUREGENERICP(x)    (SCM_INST_TYPE(x) & SCM_OBJF_GENERIC)
#define SCM_SIMPLEMETHODP(x)   (SCM_INST_TYPE(x) & SCM_OBJF_SIMPLE_METHOD)
#define SCM_ACCESSORP(x)       (SCM_INST_TYPE(x) & SCM_OBJF_ACCESSOR)
#define SCM_FASTMETHODP(x)     (SCM_INST_TYPE(x) & (SCM_OBJF_ACCESSOR|SCM_OBJF_SIMPLE_METHOD))

#define SCM_SLOT(x, i)         (SCM_INST(x)[i])
#define SCM_SUBCLASSP(c1, c2)  SCM_NNULLP (scm_sloppy_memq (c2, SCM_SLOT (c1, scm_si_cpl)))


#define scm_si_name 		 10 	/* a symbol */
#define scm_si_direct_supers 	  4 	/* (class ...) */
#define scm_si_direct_slots	  5 	/* ((name . options) ...) */
#define scm_si_direct_subclasses 11	/* (class ...) */
#define scm_si_direct_methods	 12	/* (methods ...) */
#define scm_si_cpl		 13 	/* (class ...) */
#define scm_si_slots		 14	/* ((name . options) ...) */
#define scm_si_nfields		 15	/* an integer */
#define scm_si_layout		  0	/* the struct layout */
#define scm_si_getters_n_setters 16	/* ((slot getter setter) ...) */
#define scm_si_redefined	 17	/* the class to which class was redefined */
#define scm_si_environment	 18	/* The environme in which class is built  */
#define SCM_N_CLASS_SLOTS	 19

#define scm_si_methods		  5	/* offset of methods slot in a <generic> */

#define scm_si_generic_function	  0	/* offset of gf    slot in a <method> */
#define scm_si_specializers	  1	/* offset of spec. slot in a <method> */

#define scm_si_procedure 	  2	/* offset of proc. slot in a <method> */

/* Low level functions exported */
SCM scm_make_next_method (SCM methods, SCM args, SCM gf);
SCM scm_basic_make_class (SCM classe, SCM name, SCM dsupers, SCM dslots);

/* Primitives exported */
SCM scm_sys_allocate_instance (SCM classe);
SCM scm_slot_ref (SCM obj, SCM slot_name);
SCM scm_slot_set_x (SCM obj, SCM slot_name, SCM value);
SCM scm_class_of (SCM obj);

SCM scm_compute_applicable_methods (SCM gf, SCM args, int len, int scm_find_method);
SCM scm_apply_next_method(SCM args);
extern void scm_init_goops_goops_module (void);

#endif /* GOOPSH */
