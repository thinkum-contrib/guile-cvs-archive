/* classes: h_files */

#ifndef SCM_NULL_DEFS_H
#define SCM_NULL_DEFS_H

/* Copyright (C) 2002 Free Software Foundation, Inc.
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



/* The null-threads implementation.  We provide the standard API, but
   no new threads can be created.
*/

#define SCM_CRITICAL_SECTION_START 
#define SCM_CRITICAL_SECTION_END 
#define SCM_THREAD_SWITCHING_CODE

typedef struct scm_null_mutex {
  int locked;
} scm_null_mutex;

SCM_API int scm_null_mutex_init (scm_null_mutex *);
SCM_API int scm_null_mutex_lock (scm_null_mutex *);
SCM_API int scm_null_mutex_unlock (scm_null_mutex *);
SCM_API int scm_null_mutex_destroy (scm_null_mutex *);

typedef scm_null_mutex scm_t_mutex;
#define scm_mutex_init scm_null_mutex_init
#define scm_mutex_lock scm_null_mutex_lock
#define scm_mutex_unlock scm_null_mutex_unlock

typedef struct scm_null_condvar {
  int signalled;
} scm_null_condvar;

SCM_API int scm_null_condvar_init (scm_null_condvar *);
SCM_API int scm_null_condvar_wait (scm_null_condvar *, scm_null_mutex *);
SCM_API int scm_null_condvar_signal (scm_null_condvar *);
SCM_API int scm_null_condvar_destroy (scm_null_condvar *);

typedef scm_null_condvar scm_t_cond;
#define scm_cond_init scm_null_condvar_init
#define scm_cond_wait scm_null_condvar_wait
#define scm_cond_signal scm_null_condvar_signal
#define scm_cond_broadcast scm_null_condvar_signal /* yes */
#define scm_cond_destroy scm_null_condvar_destroy

SCM_API void *scm_null_threads_data;

#define SCM_THREAD_LOCAL_DATA          (scm_null_threads_data)
#define SCM_SET_THREAD_LOCAL_DATA(ptr) (scm_null_threads_data = (ptr))

#endif  /* SCM_NULL_DEFS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
