/* classes: h_files */

#ifndef THREADSH
#define THREADSH

/* Copyright (C) 1996, 1997, 1998, 2000 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/__scm.h"
#include "libguile/procs.h"
#include "libguile/throw.h"


/* Dirk:FIXME:: Where should this go?  All the definitions below (up to
 * FD_ZERO are needed only for scm_thread.thread_select.  They are repeated
 * for all headers that provide select-functionality. */

/* Needed for FD_SET on some systems.  */
#include <sys/types.h>

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  ifdef HAVE_TIME_H
#   include <time.h>
#  endif
# endif
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifdef FD_SET

#define SELECT_TYPE fd_set
#define SELECT_SET_SIZE FD_SETSIZE

#else /* no FD_SET */

/* Define the macros to access a single-int bitmap of descriptors.  */
#define SELECT_SET_SIZE 32
#define SELECT_TYPE int
#define FD_SET(n, p) (*(p) |= (1 << (n)))
#define FD_CLR(n, p) (*(p) &= ~(1 << (n)))
#define FD_ISSET(n, p) (*(p) & (1 << (n)))
#define FD_ZERO(p) (*(p) = 0)

#endif /* no FD_SET */



/* The definition of the thread related data types is targeted towards
 * compatibility with pthreads.  This will allow to plug in most of the
 * pthreads functions without need for extra intermediate glue code.  For
 * other thread implementations this may require additional effort in the glue
 * code, though.
 */
typedef unsigned long int scm_thread_t;
typedef struct scm_threadattr scm_threadattr_t;
typedef struct scm_mutex scm_mutex_t;
typedef struct scm_mutexattr scm_mutexattr_t;
typedef struct scm_cond scm_cond_t;
typedef struct scm_condattr scm_condattr_t;
typedef unsigned int scm_key_t;

/* Dirk:FIXME:: Where should this go?  It doesn't seem good to have it here. */
#ifndef HAVE_STRUCT_TIMESPEC
/* POSIX.4 structure for a time value.  This is like a `struct timeval' but
   has nanoseconds instead of microseconds.  */
struct timespec
{
  long int tv_sec;		/* Seconds.  */
  long int tv_nsec;		/* Nanoseconds.  */
};
#endif


/* Dirk:FIXME:: As we are calling glue code anyway, wouldn't it make sense to
 * use guile specific data types, like an alist with (symbol . value) pairs
 * for attributes?  Or, scm_bits_t instead of void* data?  Or SCM values for
 * return data?  The glue code can perform the transformation.  Especially for
 * creation and destruction of threads, mutexes etc. this would be feasible,
 * since these functions are probably not too time critical.
 *
 * Also, instead of a dedicated function to set the stack size, there should
 * be two functions:  
 * 1) set_default_thread_parameters and 
 * 2) set_thread_parameters, which both should be alists IMO.
 *
 * Further, threads_mark_stacks should IMO be renamed to something more
 * general, since it is not just stacks that are to be marked.
 */
struct scm_threads {
  /* Threads */
  /* Dirk:FIXME:: What is the start_routine expected to return? */
  scm_thread_t (*make_thread) (scm_threadattr_t * attr, void * (*start_routine) (void *), void * arg);
  void (*thread_exit) (void * retval);
  int (*thread_cancel) (scm_thread_t thread);
  /* Dirk:FIXME:: What is the retval supposed to be? */
  int (*thread_join) (scm_thread_t thread, void ** retval);
  size_t (*thread_free) (scm_thread_t thread); /* returns freed amount of memory */

  /* Cooperative threads (optional) */
  void (*thread_yield) (void);
  /* Dirk:FIXME:: Should the name be changed to select? */
  /* Dirk:FIXME:: Is this interface OK? */
  int (*thread_select) 
    (int fds, SELECT_TYPE *rfds, SELECT_TYPE *wfds, SELECT_TYPE *efds, struct timeval *timeout);

  /* Mutecis */
  scm_mutex_t * (*make_mutex) (const scm_mutexattr_t * mutexattr);
  /* Dirk:FIXME:: What is the return value needed for? */
  int (*mutex_lock) (scm_mutex_t * mutex);
  int (*mutex_trylock) (scm_mutex_t * mutex);
  /* Dirk:FIXME:: What is the return value needed for? */
  int (*mutex_unlock) (scm_mutex_t * mutex);
  size_t (*mutex_free) (scm_mutex_t * mutex); /* returns freed amount of memory */

  /* Condition variables */
  scm_cond_t * (*make_cond) (const scm_condattr_t * cond_attr);
  /* Dirk:FIXME:: What is the return value needed for? */
  int (*cond_signal) (scm_cond_t * cond);
  /* Dirk:FIXME:: What is the return value needed for? */
  int (*cond_broadcast) (scm_cond_t * cond);
  /* Dirk:FIXME:: What is the return value needed for? */
  int (*cond_wait) (scm_cond_t * cond, scm_mutex_t * mutex);
  /* Dirk:FIXME:: What is the return value needed for? */
  int (*cond_timedwait) (scm_cond_t * cond, scm_mutex_t * mutex, const struct timespec * abstime);
  size_t (*cond_free) (scm_cond_t * cond); /* returns freed amount of memory */

  /* Keys */
  /* Dirk:FIXME:: What is the return value needed for? */
  /* Dirk:FIXME:: What does the parameter destr_function do? */
  /* Dirk:FIXME:: Shouldn't there rather be functions make-key, key_delete, key_free? */
  int (*key_create) (scm_key_t * key, void (*destr_function) (void *));
  /* Dirk:FIXME:: What is the return value needed for? */
  int (*key_delete) (scm_key_t key);

  /* Thread specific data */
  /* Dirk:FIXME:: Neither implemented nor used yet: */
  /* Dirk:FIXME:: What is the return value needed for? */
  int (*set_thread_specific_data) (scm_key_t key, const void *);
  /* Dirk:FIXME:: Neither implemented nor used yet: */
  void * (*thread_specific_data) (scm_key_t key);
  /* Dirk:FIXME:: shouldn't the parameter be a scm_root_state* ? */
  void (*set_thread_local_data) (void * data);
  void * (*thread_local_data) (void);

  /* Garbage collection */
  void (*threads_mark_stacks) (void);

  /* Parameters */
  void (*set_threads_stack_size) (size_t size);
};

extern struct scm_threads scm_thread;

extern size_t scm_thread_count;    /* counts between context switches */
extern size_t scm_switch_counter;  /* count-down until next switch.  0 = no switching */



/* smob tags for the thread datatypes */
extern long scm_tc16_thread;
extern long scm_tc16_mutex;
extern long scm_tc16_cond;

#define SCM_THREADP(x) (!SCM_IMP (x) && (SCM_TYP16 (x) == scm_tc16_thread))
#define SCM_THREAD_DATA(x) (SCM_CELL_WORD_1 (x))
#define SCM_SET_THREAD_DATA(x, v) (SCM_SET_CELL_WORD_1 ((x), (v)))

#define SCM_MUTEXP(x) (!SCM_IMP (x) && (SCM_TYP16 (x) == scm_tc16_mutex))
#define SCM_MUTEX_DATA(x) ((scm_mutex_t *) SCM_CELL_WORD_1 (x))

#define SCM_CONDP(x) (!SCM_IMP (x) && (SCM_TYP16 (x) == scm_tc16_cond))
#define SCM_COND_DATA(x) ((scm_cond_t *) SCM_CELL_WORD_1 (x))

/* 
 * C interface
 */

/* Dirk:Fixme:: Should these macros be eliminated? */
#define SCM_THREAD_LOCAL_DATA ((* scm_thread.thread_local_data) ())
#define SCM_SET_THREAD_LOCAL_DATA(v) ((* scm_thread.set_thread_local_data) (v))
/* Dirk:FIXME:: This has to be fixed.  For cooperative threads it's OK though. */
#define SCM_THREAD_CRITICAL_SECTION_START 
#define SCM_THREAD_CRITICAL_SECTION_END 

SCM scm_spawn_thread (scm_catch_body_t, void *, scm_catch_handler_t, void *);

#define scm_c_thread_yield() ((* scm_thread.thread_yield) ())
#define scm_c_thread_select(f,r,w,e,t) ((* scm_thread.thread_select) (f, r, w, e, t))

#define scm_c_make_mutex(a) ((* scm_thread.make_mutex) (a))
#define scm_c_mutex_lock(m) ((* scm_thread.mutex_lock) (m))
#define scm_c_mutex_trylock(m) ((* scm_thread.mutex_trylock) (m))
#define scm_c_mutex_unlock(m) ((* scm_thread.mutex_unlock) (m))

#define scm_c_make_cond(a) ((* scm_thread.make_cond) (a))
#define scm_c_cond_signal(c) ((* scm_thread.cond_signal) (c))
#define scm_c_cond_broadcast(c) ((* scm_thread.cond_broadcast) (c))
#define scm_c_cond_wait(c, m) ((* scm_thread.cond_wait) (c, m))
#define scm_c_cond_timedwait(c, m, t) ((* scm_thread.cond_timedwait) (c, m, t))

#define scm_c_key_create(k, f) ((* scm_thread.key_create) (k, f))
#define scm_c_key_delete(k) ((* scm_thread.key_delete) (k))

/* Dirk:FIXME:: How to name these?
#define scm_c_setspecific scm_thread.setspecific
#define scm_c_getspecific scm_thread.getspecific
*/

#define scm_threads_mark_stacks() ((* scm_thread.threads_mark_stacks) ())

/* These are versions of the ordinary sleep and usleep functions,
   that play nicely with the thread system.  */
unsigned long scm_thread_sleep (unsigned long);
unsigned long scm_thread_usleep (unsigned long);

/* Initialization.  */
void scm_init_threads (SCM_STACKITEM *);


/* 
 * Scheme interface. 
 */

/* Dirk:FIXME:: The name call_with_new_thread does not fit very nicely with
 * with rest of the functions.  I'd rather use make_thread or spawn_thread,
 * but both in sync with the C level function names.  make_thread, however, is
 * defined as a macro in ice-9/threads.scm. */
extern SCM scm_call_with_new_thread (SCM thunk, SCM error_thunk);
/* Dirk:FIXME:: Neither implemented nor used yet.  What shall it do? */
extern SCM scm_m_begin_thread (SCM exp, SCM env);
extern SCM scm_thread_p (SCM obj);
extern SCM scm_thread_exit (SCM retval);
extern SCM scm_thread_cancel (SCM thread);
extern SCM scm_thread_join (SCM t);

extern SCM scm_thread_yield (void);
/* Dirk:FIXME:: Should the common select and the thread based select function
 * be merged?  The following definition is from filesys.h */
/* extern SCM scm_select (SCM reads, SCM writes, SCM excepts, SCM secs, SCM msecs); */

extern SCM scm_make_mutex (void);
extern SCM scm_mutex_p (SCM obj);
extern SCM scm_mutex_lock (SCM m);
extern SCM scm_mutex_trylock (SCM m);
extern SCM scm_mutex_unlock (SCM m);

extern SCM scm_make_cond (void);
extern SCM scm_cond_p (SCM obj);
extern SCM scm_cond_signal (SCM cond);
extern SCM scm_cond_broadcast (SCM cond);
extern SCM scm_cond_wait (SCM cond, SCM mutex);
extern SCM scm_cond_timedwait (SCM cond, SCM mutex, SCM seconds, SCM nanoseconds);



#if (SCM_DEBUG_DEPRECATED == 0)

#define scm_join_thread scm_thread_join
#define scm_lock_mutex scm_mutex_lock
#define scm_unlock_mutex scm_mutex_unlock
#define scm_wait_condition_variable scm_cond_wait
#define scm_signal_condition_variable scm_cond_signal

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

#endif  /* THREADSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
