/* classes: h_files */

#ifndef GOOPSSNARFH
#define GOOPSSNARFH
/*	Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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


#include <libguile/snarf.h>

#ifndef SCM_SNARF_INIT
/* Old snarf.h */
#define SCM_SNARF_INIT(X) %%% X
#endif

#define SCM_I_CLASS(C_NAME, S_NAME, SUPERS, SIZE, CONSTR, DESTR)	\
SCM_SNARF_INIT(								\
  scm_load_goops ();							\
  C_NAME = scm_permanent_object (scm_intern0 (S_NAME));			\
  SCM_SETCDR (C_NAME,							\
  	    scm_make_class (scm_class_foreign_class,			\
  			    S_NAME, SUPERS, SIZE,			\
  			    (void * (*) (SCM)) CONSTR,			\
  			    (size_t (*) (void *)) DESTR));		\
  C_NAME = SCM_CDR (C_NAME)						\
)
     
#ifndef SCM_MAGIC_SNARFER
#define SCM_CLASS(C_NAME, S_NAME, SUPERS, SIZE, CONSTR, DESTR)		\
	static SCM C_NAME = SCM_BOOL_F
#else
#define SCM_CLASS(C_NAME, S_NAME, SUPERS, SIZE, CONSTR, DESTR)		\
SCM_I_CLASS(C_NAME, S_NAME, SUPERS, SIZE, CONSTR, DESTR)
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_GLOBAL_CLASS(C_NAME, S_NAME, SUPERS, SIZE, CONSTR, DESTR)	\
	SCM C_NAME = SCM_BOOL_F
#else
#define SCM_GLOBAL_CLASS(C_NAME, S_NAME, SUPERS, SIZE, CONSTR, DESTR)	\
SCM_I_CLASS(C_NAME, S_NAME, SUPERS, SIZE, CONSTR, DESTR)
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_VIRTUAL_SLOT(C_CLASS, C_TYPE, C_SLOT, S_SLOT, S_TYPE, GET, SET, A)
#else
#define SCM_VIRTUAL_SLOT(C_CLASS, C_TYPE, C_SLOT, S_SLOT, S_TYPE, GET, SET, A) \
SCM_SNARF_INIT (scm_add_slot (C_CLASS, S_SLOT, S_TYPE, GET, SET, A))
#endif

#define SCM_I_GETTER_NAME(C_CLASS, C_SLOT) scm_get_ ## C_CLASS ## _ ## C_SLOT

#define SCM_I_GETTER(C_CLASS, C_TYPE, C_SLOT, GET)			\
static SCM								\
SCM_I_GETTER_NAME (C_CLASS, C_SLOT) (SCM obj)				\
{									\
  C_TYPE *o = (C_TYPE *) SCM_CDR (obj);					\
  return GET;								\
}									\
static SCM								\
SCM_I_GETTER_NAME (C_CLASS, C_SLOT) (SCM obj)				\

#define SCM_I_SETTER_NAME(C_CLASS, C_SLOT) scm_set_ ## C_CLASS ## _ ## C_SLOT

#define SCM_I_SETTER(C_CLASS, C_TYPE, C_SLOT, PRED, SET)		\
static SCM								\
SCM_I_SETTER_NAME (C_CLASS, C_SLOT) (SCM obj, SCM x)			\
{									\
  C_TYPE *o = (C_TYPE *) SCM_CDR (obj);					\
  SCM_ASSERT (PRED, x, SCM_ARG3, scm_s_slot_set_x);			\
  SET;									\
  return SCM_UNSPECIFIED;						\
}									\

#ifndef SCM_MAGIC_SNARFER
#define SCM_I_SLOT(C_CLASS, C_TYPE, C_SLOT, S_SLOT, S_TYPE, PRED, GET, SET, A) \
SCM_I_SETTER (C_CLASS, C_TYPE, C_SLOT, PRED, SET)			\
SCM_I_GETTER (C_CLASS, C_TYPE, C_SLOT, GET)				\

#else
#define SCM_I_SLOT(C_CLASS, C_TYPE, C_SLOT, S_SLOT, S_TYPE, PRED, GET, SET, A) \
SCM_SNARF_INIT (scm_add_slot (C_CLASS, S_SLOT, S_TYPE,			\
			      SCM_I_GETTER_NAME (C_CLASS, C_SLOT),	\
			      SCM_I_SETTER_NAME (C_CLASS, C_SLOT),	\
			      A))
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_I_ROSLOT(C_CLASS, C_TYPE, C_SLOT, S_SLOT, S_TYPE, GET, A)	\
SCM_I_GETTER (C_CLASS, C_TYPE, C_SLOT, GET)				\

#else
#define SCM_I_ROSLOT(C_CLASS, C_TYPE, C_SLOT, S_SLOT, S_TYPE, GET, A)	\
SCM_SNARF_INIT (scm_add_slot (C_CLASS, S_SLOT, S_TYPE,			\
			      SCM_I_GETTER_NAME (C_CLASS, C_SLOT),	\
			      0,					\
			      A))
#endif

#define SCM_SLOT_PTR(C_CLASS, C_TYPE, C_SLOT, S_SLOT, S_TYPE, A)	\
SCM_I_SLOT (C_CLASS, C_TYPE, C_SLOT, S_SLOT, S_TYPE,			\
	    SCM_IS_A_P (x, S_TYPE),					\
	    scm_wrap_component (S_TYPE, obj, o->C_SLOT),		\
	    o->C_SLOT = (void *) SCM_CDR (obj),				\
	    A)

#define SCM_ROSLOT_PTR(C_CLASS, C_TYPE, C_SLOT, S_SLOT, S_TYPE, A)	\
SCM_I_ROSLOT (C_CLASS, C_TYPE, C_SLOT, S_SLOT, S_TYPE,			\
	      scm_wrap_component (S_TYPE, obj, o->C_SLOT), A)

#define SCM_SLOT_INT(C_CLASS, C_TYPE, C_SLOT, SCM_SLOT, A)		\
SCM_I_SLOT (C_CLASS, C_TYPE, C_SLOT, SCM_SLOT, scm_class_int,		\
	    SCM_INUMP (x),						\
	    SCM_MAKINUM (o->C_SLOT),					\
	    o->C_SLOT = SCM_INUM (x),					\
	    A)

#define SCM_SLOT_FLOAT(C_CLASS, C_TYPE, C_SLOT, SCM_SLOT, A)		\
SCM_I_SLOT (C_CLASS, C_TYPE, C_SLOT, SCM_SLOT, scm_class_float,		\
	    SCM_NUMBERP (x),						\
	    scm_makflo (o->C_SLOT),					\
	    o->C_SLOT = SCM_NUM2DBL (x),				\
	    A)

#define SCM_SLOT_DOUBLE(C_CLASS, C_TYPE, C_SLOT, SCM_SLOT, A)		\
SCM_I_SLOT (C_CLASS, C_TYPE, C_SLOT, SCM_SLOT, scm_class_double,	\
	    SCM_NUMBERP (x),						\
	    scm_makdbl (o->C_SLOT, 0.0),				\
	    o->C_SLOT = SCM_NUM2DBL (x),				\
	    A)

#define SCM_SLOT_SCM(C_CLASS, C_TYPE, C_SLOT, SCM_SLOT, A)		\
SCM_I_SLOT (C_CLASS, C_TYPE, C_SLOT, SCM_SLOT, scm_class_scm,		\
	    1,								\
	    o->C_SLOT,							\
	    o->C_SLOT = x,						\
	    A)

#endif /* GOOPSSNARFH */
