/* classes: h_files */

#ifndef TKCMDH
#define TKCMDH

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


#include "libguile/__scm.h"

#include "libguile/objects.h"

#include "tcl.h"



/* The argument conversion function needs to be able to efficiently
 * identify instances of the tk-command class or its subclasses.
 */
#define SCM_CLASSF_TKCMD (1L << 28)

#define SCM_TKCMDP(obj)\
((SCM_OBJ_CLASS_FLAGS (obj) & SCM_CLASSF_TKCMD) != 0)
#define SCM_TKCMD(obj) ((scm_tkcmd*) SCM_STRUCT_DATA (obj))

#define SCM_TKCMD_LAYOUT "uououououruR"
typedef struct Tk_command {
  ClientData ptr;		/* pointer associated to the widget command */
  Tcl_CmdProc *fct;		/* Tk lib function associated to widget */    
  Tcl_CmdDeleteProc *delproc; /* procedure to call when command is destroyed */
  ClientData deldata;		/* value to pass to delproc */                
  int deleted;			/* 1 if command has already been deleted */
  char *Id;
  char Id_chars[1];		/* must be last field */
} scm_tkcmd;

extern SCM scm_class_tk_command;

extern SCM scm_make_tkcmd (char *name,
			   Tcl_CmdProc *proc,
			   ClientData clientData,
			   Tcl_CmdDeleteProc *deleteProc);
extern void scm_init_tkcmd SCM_P ((void));

#endif /* TKCMDH */
