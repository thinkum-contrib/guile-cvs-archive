/* Copyright (C) 1995, 1996, 1997, 1998, 2000 Free Software Foundation, Inc.
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



#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/dynwind.h"
#include "libguile/root.h"
#include "libguile/smob.h"

#include "libguile/validate.h"
#include "libguile/threads.h"

extern SCM scm_apply (SCM, SCM, SCM);
extern void scm_threads_init (SCM_STACKITEM *);



struct scm_threads scm_thread;
/* A counter of the current number of threads */
size_t scm_thread_count = 0;
/* A count-down counter used to determine when to switch contexts */
size_t scm_switch_counter = SCM_THREAD_SWITCH_COUNT;


long scm_tc16_thread;
long scm_tc16_mutex;
long scm_tc16_cond;



/* NOTE: There are TWO mechanisms for starting a thread: The first one
   is used when spawning a thread from Scheme, while the second one is
   used from C.

   It might be argued that the first should be implemented in terms of
   the second.  The reason it isn't is that that would require an
   extra unnecessary malloc (the thread_args structure).  By providing
   one pair of extra functions (c_launch_thread, scm_spawn_thread) the
   Scheme threads are started more efficiently.  */

/* This is the mechanism to spawn threads from C */

typedef struct c_launch_data {
  union {
    SCM thread;
    SCM rootcont;
  } u;
  SCM root_data;
  scm_catch_body_t body;
  void *body_data;
  scm_catch_handler_t handler;
  void *handler_data;
} c_launch_data;

static SCM
c_body_bootstrip (c_launch_data* data)
{
  /* First save the new root continuation */
  data->u.rootcont = scm_root->rootcont;
  return (data->body) (data->body_data);
}

static SCM
c_handler_bootstrip (c_launch_data* data, SCM tag, SCM throw_args)
{
  scm_root->rootcont = data->u.rootcont;
  return (data->handler) (data->handler_data, tag, throw_args);
}

static void *
c_launch_thread (void *p)
{
  register c_launch_data *data = (c_launch_data *) p;
  /* The thread object will be GC protected by being on this stack */
  /* Dirk:FIXME:: But, what is for the time between scm_spawn_thread and the
   * invocation of c_launch_thread?  During that time, the only reference to
   * the thread and root objects is on the non-gc-controlled heap. */
  SCM thread = data->u.thread;
  SCM root = data->root_data;
  (* scm_thread.set_thread_local_data) (SCM_ROOT_STATE (root));
  /* We must use the address of `thread', otherwise the compiler will
     optimize it away.  This is OK since the longest SCM_STACKITEM
     also is a long.  */
  scm_internal_cwdr ((scm_catch_body_t) c_body_bootstrip,
		     data,
		     (scm_catch_handler_t) c_handler_bootstrip,
		     data,
		     (SCM_STACKITEM *) &thread);
  /* Dirk:FIXME:: In contrast to the scheme level launch function, the SCM
   * thread object is not invalidated here.  Thus, the thread data can still
   * be accessed via the SCM thread object even after the thread has died. */
  scm_thread_count--;
  scm_must_free ((char *) data);
  return NULL;
}



/* This is the mechanism to spawn threads from Scheme */

typedef struct scheme_launch_data {
  SCM rootcont;
  SCM body;
  SCM handler;
} scheme_launch_data;

static SCM
scheme_body_bootstrip (scheme_launch_data* data)
{
  /* First save the new root continuation */
  data->rootcont = scm_root->rootcont;
  return scm_apply (data->body, SCM_EOL, SCM_EOL);
}

static SCM
scheme_handler_bootstrip (scheme_launch_data* data, SCM tag, SCM throw_args)
{
  scm_root->rootcont = data->rootcont;
  return scm_apply (data->handler, scm_cons (tag, throw_args), SCM_EOL);
}

static void *
scheme_launch_thread (void * p)
{
  /* The thread object will be GC protected by being a member of the
     list given as argument to launch_thread.  It will be marked
     during the conservative sweep of the stack. */
  register SCM argl = SCM_PACK (p);
  SCM thread = SCM_CAR (argl);
  SCM root = SCM_CADR (argl);
  scheme_launch_data data;
  (* scm_thread.set_thread_local_data) (SCM_ROOT_STATE (root));
  data.rootcont = SCM_BOOL_F;
  data.body = SCM_CADDR (argl);
  data.handler = SCM_CADDDR (argl);
  scm_internal_cwdr ((scm_catch_body_t) scheme_body_bootstrip,
		     &data,
		     (scm_catch_handler_t) scheme_handler_bootstrip,
		     &data,
		     (SCM_STACKITEM *) &thread);
  /* Dirk:FIXME:: This code is probably necessary in order not to be able to
   * access threads from scheme that have died.  However, since 0 may be a
   * valid value for some existing thread, this is insecure.  Further, after
   * this value has been set to 0, the function thread_free can not free the
   * thread any more, since the thread id is not available.
   * It would be better to have a status bit in the cell type that tells about
   * whether the thread is still alive.  The problem is, that the two accesses
   * to the status bit and to the thread id could be interrupted.  Thus, a
   * mutex would be required to change and read the read status bit and thread
   * data.  However, since this is an essential mutex used by the threads
   * implementation, it must be created at guile's startup. */
  SCM_SET_THREAD_DATA (thread, 0);
  scm_thread_count--;
  SCM_DEFER_INTS;
  return NULL;
}



SCM
scm_spawn_thread (scm_catch_body_t body, void *body_data,
		  scm_catch_handler_t handler, void *handler_data)
{
  SCM old_winds;
  SCM root;
  SCM thread;
  scm_thread_t thread_data;
  c_launch_data *data = (c_launch_data *) scm_must_malloc (sizeof (*data), "scm_spawn_thread");
  
  /* Unwind wind chain. */
  old_winds = scm_dynwinds;
  scm_dowinds (SCM_EOL, scm_ilength (scm_root->dynwinds));

  /* Allocate thread locals. */
  root = scm_make_root (scm_root->handle);

  /* Prepare parameters for c_launch_thread */
  SCM_NEWCELL (thread);
  SCM_DEFER_INTS;
  SCM_SET_CELL_TYPE (thread, scm_tc16_thread);
  data->u.thread = thread;
  /* Dirk:Note:: We have to set the root data from within the new thread.
   * This is different from the way it was done before.
   */
  data->root_data = root;
  data->body = body;
  data->body_data = body_data;
  data->handler = handler;
  data->handler_data = handler_data;
  /* Dirk:FIXME:: What about the stack size settings?  Note that if this is
   * done, the data has to be passed also, since the setting has to be done
   * from within the new thread.
   */
  
  /* Create thread */
  thread_data = (* scm_thread.make_thread) (NULL, c_launch_thread, (void *) data);
  SCM_SET_THREAD_DATA (thread, thread_data);
  
  scm_thread_count++;
  /* Note that the following statement also could cause coop_yield.*/
  SCM_ALLOW_INTS;

  /* We're now ready for the thread to begin. */
  (*scm_thread.thread_yield) ();

  /* Return to old dynamic context. */
  scm_dowinds (old_winds, - scm_ilength (old_winds));
  
  return thread;
}


SCM_DEFINE(scm_call_with_new_thread, "call-with-new-thread", 2, 0, 0,
	   (SCM thunk, SCM error_thunk),
	   "Evaluate @var{(thunk)} in a new thread, and new dynamic\n"
	   "context, returning a new thread object representing the thread.\n"
	   "If an error occurs during evaluation, call error-thunk, passing\n"
	   "it an error code describing the condition.  [Error codes are\n"
	   "currently meaningless integers.  In the future, real values\n"
	   "will be specified.]  If this happens, the error-thunk is called\n"
	   "outside the scope of the new root -- it is called in the same\n"
	   "dynamic context in which call-with-new-thread was evaluated, but\n"
	   "not in the callers thread.  All the evaluation rules for dynamic\n"
	   "roots apply to threads.")
#define FUNC_NAME s_scm_call_with_new_thread
{
  SCM old_winds;
  SCM root;
  SCM thread;
  scm_thread_t thread_data;
  SCM args;

  SCM_VALIDATE_THUNK (1, thunk);
  SCM_VALIDATE_THUNK (2, error_thunk);

  /* Unwind wind chain. */
  old_winds = scm_dynwinds;
  scm_dowinds (SCM_EOL, scm_ilength (scm_root->dynwinds));

  /* Allocate thread locals. */
  root = scm_make_root (scm_root->handle);

  /* Prepare parameters for scheme_launch_thread */
  SCM_NEWCELL (thread);
  SCM_DEFER_INTS;
  SCM_SET_CELL_TYPE (thread, scm_tc16_thread);
  args = scm_cons (error_thunk, SCM_EOL);
  args = scm_cons (thunk, args);
  /* Dirk:Note:: We have to set the root data from within the new thread.
   * This is different from the way it was done before.
   */
  args = scm_cons (root, args);
  args = scm_cons (thread, args);
  /* Dirk:FIXME:: What about the stack size settings?  Note that if this is
   * done, the data has to be passed also, since the setting has to be done
   * from within the new thread.
   */

  /* Create thread */
  thread_data = (* scm_thread.make_thread) (NULL, scheme_launch_thread, (void *) SCM_UNPACK (args));
  SCM_SET_THREAD_DATA (thread, thread_data);

  scm_thread_count++;
  /* Note that the following statement also could cause coop_yield.*/
  SCM_ALLOW_INTS;
  
  /* We're now ready for the thread to begin. */
  (*scm_thread.thread_yield) ();

  /* Return to old dynamic context. */
  scm_dowinds (old_winds, - scm_ilength (old_winds));

  return thread;
}
#undef FUNC_NAME


SCM_DEFINE(scm_thread_p, "thread?", 1, 0, 0,
	   (SCM obj),
	   "Return #t iff OBJ is a thread object.")
#define FUNC_NAME s_scm_thread_p
{
  return SCM_BOOL (SCM_THREADP (obj));
}
#undef FUNC_NAME


SCM_DEFINE(scm_thread_exit, "thread-exit", 1, 0, 0,
	   (SCM result),
	   "Finish the currently running thread and return RESULT.")
#define FUNC_NAME s_scm_thread_exit
{
  /* Dirk:FIXME:: Guile hangs when doing thread-exit in the repl. */
  /* Dirk:FIXME:: Guile hangs when doing thread-exit without params. */

  (* scm_thread.thread_exit) ((void *) SCM_UNPACK (result));
  return SCM_BOOL_T; /* not reached */
}
#undef FUNC_NAME


SCM_DEFINE(scm_thread_cancel, "thread-cancel", 1, 0, 0,
	   (SCM thread),
	   "Abort the execution of THREAD.")
#define FUNC_NAME s_scm_thread_cancel
{
  scm_thread_t thread_data;
  SCM_VALIDATE_THREAD (1, thread);
  thread_data = SCM_THREAD_DATA (thread);
  if (thread_data)
    /* The thread is still alive */
    (* scm_thread.thread_cancel) (thread_data);
  return SCM_BOOL_T;
}
#undef FUNC_NAME


static scm_sizet
thread_free (SCM thread)
{
  scm_thread_t thread_data = SCM_THREAD_DATA (thread);
  if (thread_data)
    /* The thread is still alive */
    return (* scm_thread.thread_free) (thread_data);
  else
    /* Dirk:FIXME:: If the thread has exited, then the thread data is 0.
     * This has to be solved in a better way. */
    return 0;
}


#if (SCM_DEBUG_DEPRECATED == 0)

SCM_REGISTER_PROC(s_join_thread, "join-thread", 1, 0, 0, scm_thread_join);

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

SCM_DEFINE(scm_thread_join, "thread-join", 1, 0, 0,
	   (SCM thread),
	   "Suspend execution of the calling thread until the target\n"
	   "@var{thread} terminates, unless the target @var{thread} has\n"
	   "already terminated.")
#define FUNC_NAME s_scm_thread_join
{
  scm_thread_t thread_data;
  SCM_VALIDATE_THREAD (1, thread);
  /* Dirk:FIXME:: SCM_THREAD_DATA is a handle for a thread.  It may be that a
   * certain thread implementation uses a value of 0 as a valid thread handle.
   * With the following code, this thread would always be considered finished.
   */
  /* Dirk:FIXME:: With preemptive threading, a thread may finish immediately
   * after SCM_THREAD_DATA is read.  Thus, it must be guaranteed that the
   * handle remains valid until the thread-object is garbage collected, or
   * a mutex has to be used for reading and modifying SCM_THREAD_DATA.
   */
  thread_data = SCM_THREAD_DATA (thread);
  if (thread_data)
    /* The thread is still alive */
    (* scm_thread.thread_join) (thread_data, NULL);
  return SCM_BOOL_T;
}
#undef FUNC_NAME



#if (SCM_DEBUG_DEPRECATED == 0)

SCM_REGISTER_PROC(s_scm_yield, "yield", 0, 0, 0, scm_thread_yield);

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

SCM_DEFINE(scm_thread_yield, "thread-yield", 0, 0, 0,
	   (),
	   "If one or more threads are waiting to execute, calling\n"
	   "thread-yield forces an immediate context switch to one of them.\n"
	   "Otherwise, thread-yield has no effect.")
#define FUNC_NAME s_scm_thread_yield
{
  scm_switch_counter = SCM_THREAD_SWITCH_COUNT;
  (* scm_thread.thread_yield) ();
  return SCM_BOOL_T;
}
#undef FUNC_NAME



SCM_DEFINE(scm_make_mutex, "make-mutex", 0, 0, 0,
	   (),
	   "Create a new mutex object.")
#define FUNC_NAME s_scm_make_mutex
{
  SCM mutex;
  scm_mutex_t * mutex_data = (* scm_thread.make_mutex) (NULL);
  SCM_NEWSMOB (mutex, scm_tc16_mutex, mutex_data);
  return mutex;
}
#undef FUNC_NAME


SCM_DEFINE(scm_mutex_p, "mutex?", 1, 0, 0,
	   (SCM obj),
	   "Return #t iff OBJ is a mutex object.")
#define FUNC_NAME s_scm_mutex_p
{
  return SCM_BOOL (SCM_MUTEXP (obj));
}
#undef FUNC_NAME


#if (SCM_DEBUG_DEPRECATED == 0)

SCM_REGISTER_PROC(s_lock_mutex, "lock-mutex", 1, 0, 0, scm_mutex_lock);

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

SCM_DEFINE(scm_mutex_lock, "mutex-lock", 1, 0, 0, 
	   (SCM mutex),
	   "Lock @var{mutex}. If the mutex is already locked, the calling\n"
	   "thread blocks until the mutex becomes available. The function\n"
	   "returns when the calling thread owns the lock on @var{mutex}.")
#define FUNC_NAME s_scm_mutex_lock
{
  SCM_ASSERT (SCM_MUTEXP (mutex), mutex, SCM_ARG1, s_scm_mutex_lock);
  (* scm_thread.mutex_lock) (SCM_MUTEX_DATA (mutex));
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE(scm_mutex_trylock, "mutex-trylock", 1, 0, 0, 
	   (SCM mutex),
	   "If the @var{mutex} is not locked yet, lock it and return #t.\n"
	   "Otherwise, if the mutex is already locked, return #f.  The\n"
	   "calling thread is never blocked.")
#define FUNC_NAME s_scm_mutex_trylock
{
  SCM_ASSERT (SCM_MUTEXP (mutex), mutex, SCM_ARG1, s_scm_mutex_lock);
  return SCM_BOOL ((* scm_thread.mutex_trylock) (SCM_MUTEX_DATA (mutex)) == 0);
}
#undef FUNC_NAME


#if (SCM_DEBUG_DEPRECATED == 0)

SCM_REGISTER_PROC(s_unlock_mutex, "unlock-mutex", 1, 0, 0, scm_mutex_unlock);

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

SCM_DEFINE(scm_mutex_unlock, "mutex-unlock", 1, 0, 0, 
	   (SCM mutex),
	   "Unlocks @var{mutex} if the calling thread owns the lock on\n"
	   "@var{mutex}.  Calling unlock-mutex on a mutex not owned by the\n"
	   "current thread results in undefined behaviour.  Once a mutex\n"
	   "has been unlocked, one thread blocked on @var{mutex} is\n"
	   "awakened and grabs the mutex lock.")
#define FUNC_NAME s_scm_mutex_unlock
{
  /* Dirk:FIXME:: What if the mutex was not locked before? */

  SCM_ASSERT (SCM_MUTEXP (mutex), mutex, SCM_ARG1, s_scm_mutex_unlock);
  (* scm_thread.mutex_unlock) (SCM_MUTEX_DATA (mutex));

  /* Yield early */
  scm_switch_counter = SCM_THREAD_SWITCH_COUNT;
  (* scm_thread.thread_yield) ();

  return SCM_BOOL_T;
}
#undef FUNC_NAME


static scm_sizet
mutex_free (SCM mutex)
{
  /* Dirk:FIXME:: What happens to the threads that are blocked because of this
   * mutex?  They will never wake up again.  */

  return (* scm_thread.mutex_free) (SCM_MUTEX_DATA (mutex));  
}



SCM_DEFINE(scm_make_cond, "make-cond", 0, 0, 0, 
	   (),
	   "")
#define FUNC_NAME s_scm_make_cond
{
  SCM cond;
  scm_cond_t * cond_data = (* scm_thread.make_cond) (NULL);
  SCM_NEWSMOB (cond, scm_tc16_cond, cond_data);
  return cond;
}
#undef FUNC_NAME


SCM_DEFINE(scm_cond_p, "cond?", 1, 0, 0,
	   (SCM obj),
	   "Return #t iff OBJ is a condition variable object.")
#define FUNC_NAME s_scm_cond_p
{
  return SCM_BOOL (SCM_CONDP (obj));
}
#undef FUNC_NAME


#if (SCM_DEBUG_DEPRECATED == 0)

SCM_REGISTER_PROC(s_signal_condition_variable, "signal-condition-variable", 1, 0, 0, scm_cond_signal);

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

SCM_DEFINE(scm_cond_signal, "cond-signal", 1, 0, 0, 
	   (SCM cond),
	   "")
#define FUNC_NAME s_scm_cond_signal
{
  SCM_ASSERT (SCM_CONDP (cond), cond, SCM_ARG1, FUNC_NAME);
  (* scm_thread.cond_signal) (SCM_COND_DATA (cond));
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE(scm_cond_broadcast, "cond-broadcast", 1, 0, 0, 
	   (SCM cond),
	   "")
#define FUNC_NAME s_scm_cond_broadcast
{
  SCM_ASSERT (SCM_CONDP (cond), cond, SCM_ARG1, FUNC_NAME);
  (* scm_thread.cond_broadcast) (SCM_COND_DATA (cond));
  return SCM_BOOL_T;
}
#undef FUNC_NAME


#if (SCM_DEBUG_DEPRECATED == 0)

SCM_REGISTER_PROC(s_wait_condition_variable, "wait-condition-variable", 2, 0, 0, scm_cond_wait);

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

SCM_DEFINE(scm_cond_wait, "cond-wait", 2, 0, 0, 
	   (SCM cond, SCM mutex),
	   "")
#define FUNC_NAME s_scm_cond_wait
{
  SCM_ASSERT (SCM_CONDP (cond), cond, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_MUTEXP (mutex), mutex, SCM_ARG1, FUNC_NAME);
  (* scm_thread.cond_wait) (SCM_COND_DATA (cond), SCM_MUTEX_DATA (mutex));
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE(scm_cond_timedwait, "cond-timedwait", 4, 0, 0, 
	   (SCM cond, SCM mutex, SCM seconds, SCM nanoseconds),
	   "")
#define FUNC_NAME s_scm_cond_timedwait
{
  struct timespec t;
  SCM_ASSERT (SCM_CONDP (cond), cond, 1, FUNC_NAME);
  SCM_ASSERT (SCM_MUTEXP (mutex), mutex, 2, FUNC_NAME);
  SCM_VALIDATE_INUM (3, seconds);
  SCM_VALIDATE_INUM (4, nanoseconds);
  t.tv_sec = SCM_INUM (seconds);
  t.tv_nsec = SCM_INUM (nanoseconds);
  (* scm_thread.cond_timedwait) (SCM_COND_DATA (cond), SCM_MUTEX_DATA (mutex), &t);
  return SCM_BOOL_T;
}
#undef FUNC_NAME


static scm_sizet
cond_free (SCM cond)
{
  return (* scm_thread.cond_free) (SCM_COND_DATA (cond));  
}



unsigned long 
scm_thread_usleep (unsigned long usec)
{
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = usec;
  (* scm_thread.thread_select) (0, NULL, NULL, NULL, &timeout);
  return 0;  /* Maybe we should calculate actual time slept,
		but this is faster... :) */
}


unsigned long
scm_thread_sleep (unsigned long sec)
{
  time_t now = time (NULL);
  struct timeval timeout;
  unsigned long slept;
  timeout.tv_sec = sec;
  timeout.tv_usec = 0;
  (* scm_thread.thread_select) (0, NULL, NULL, NULL, &timeout);
  slept = time (NULL) - now;
  return slept > sec ? 0 : sec - slept;
}



void
scm_init_threads (SCM_STACKITEM *i)
{
  scm_tc16_thread = scm_make_smob_type ("thread", 0);
  scm_set_smob_free (scm_tc16_thread, thread_free);
  scm_tc16_mutex = scm_make_smob_type ("mutex", 0);
  scm_set_smob_free (scm_tc16_mutex, mutex_free);
  scm_tc16_cond = scm_make_smob_type ("condition-variable", 0);
  scm_set_smob_free (scm_tc16_cond, cond_free);

#include "libguile/threads.x"
  /* Initialize implementation specific details of the threads support */
  /* Dirk:FIXME:: We should initialize the functions with some default, and
   * have the thread library dynamically loaded later.  However, what if some
   * features are needed from the start, like a mutex that is needed by the
   * wrapper functions in this file?  Further, how to deal with thread, mutex
   * etc. objects that were created by threading system a) when system b) is
   * dynamically loaded? */
  scm_threads_init (i);

  scm_add_feature ("threads");
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
