/* classes: h_files */

#ifndef ROOTH
#define ROOTH

/*	Copyright (C) 1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 */




#include "libguile/__scm.h"



#define scm_flo0 scm_sys_protects[0]
#define scm_listofnull scm_sys_protects[1]
#define scm_undefineds scm_sys_protects[2]
#define scm_nullvect scm_sys_protects[3]
#define scm_nullstr scm_sys_protects[4]
#define scm_symhash scm_sys_protects[5]
#define scm_weak_symhash scm_sys_protects[6]
#define scm_symhash_vars scm_sys_protects[7]
#define scm_kw_obarray scm_sys_protects[8]
#define scm_type_obj_list scm_sys_protects[9]
#define scm_first_type scm_sys_protects[10]
#define scm_stand_in_procs scm_sys_protects[11]
#define scm_object_whash scm_sys_protects[12]
#define scm_permobjs scm_sys_protects[13]
#define scm_asyncs scm_sys_protects[14]
#ifdef DEBUG_EXTENSIONS
#define scm_source_whash scm_sys_protects[15]
#define SCM_NUM_PROTECTS 16
#else
#define SCM_NUM_PROTECTS 15
#endif

extern SCM scm_sys_protects[];



struct scm_root_state
{
  SCM_STACKITEM * stack_base;
  jmp_buf save_regs_gc_mark;
  int errjmp_bad;

  SCM rootcont;
  SCM dynwinds;
  SCM continuation_stack;
  SCM continuation_stack_ptr;

  SCM progargs;			/* vestigial */
  SCM exitval;			/* vestigial */

  SCM cur_inp;
  SCM cur_outp;
  SCM cur_errp;
  SCM def_inp;
  SCM def_outp;
  SCM def_errp;

  SCM system_transformer;
  SCM top_level_lookup_thunk_var;
};

#define scm_stack_base			(scm_root->stack_base)
#define scm_save_regs_gc_mark		(scm_root->save_regs_gc_mark)
#define scm_errjmp_bad			(scm_root->errjmp_bad)

#define scm_rootcont			(scm_root->rootcont)
#define scm_dynwinds			(scm_root->dynwinds)
#define scm_continuation_stack		(scm_root->continuation_stack)
#define scm_continuation_stack_ptr	(scm_root->continuation_stack_ptr)
#define scm_progargs			(scm_root->progargs)
#define scm_exitval 			(scm_root->exitval)
#define scm_cur_inp			(scm_root->cur_inp)
#define scm_cur_outp			(scm_root->cur_outp)
#define scm_cur_errp			(scm_root->cur_errp)
#define scm_def_inp			(scm_root->def_inp)
#define scm_def_outp			(scm_root->def_outp)
#define scm_def_errp			(scm_root->def_errp)
#define scm_top_level_lookup_thunk_var	(scm_root->top_level_lookup_thunk_var)
#define scm_system_transformer		(scm_root->system_transformer)


extern struct scm_root_state * scm_root;




#ifdef __STDC__
extern SCM scm_call_catching_errors (SCM (*thunk)(), SCM (*err_filter)(), void * closure);

#else /* STDC */
extern SCM scm_call_catching_errors ();

#endif /* STDC */






#endif  /* ROOTH */
