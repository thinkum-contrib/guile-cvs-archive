/* Copyright (C) 2000 Free Software Foundation, Inc.
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


#include "libguile/coop.h"
#include "libguile/_scm.h"
#include "libguile/root.h"
#include "libguile/threads.h"
#include "libguile/guile-coop.h"



/* Thread functions */

static scm_thread_t
make_thread (scm_threadattr_t * attr, void * (*start_routine) (void *), void * arg)
{
  /* FIXME: bad cast to coop_userf_t */
  return (scm_thread_t) coop_create ((coop_userf_t *) start_routine, arg);
}


static void 
thread_exit (void * retval)
{
  coop_abort ();
}


static int
thread_cancel (scm_thread_t thread)
{
  scm_misc_error ("guile-coop.c:thread_cancel", "Not implemented yet.", SCM_EOL);
}


static int
thread_join (scm_thread_t thread, void ** retval)
{
  coop_join ((coop_t *) thread);
  return 0;
}


static size_t
thread_free (scm_thread_t thread)
{
  return 0;
}



/* Cooperative functions */

static void
thread_yield (void)
{
  coop_yield ();
}


static int
thread_select (int f, SELECT_TYPE * r, SELECT_TYPE * w, SELECT_TYPE * e, struct timeval *t)
{
  return coop_select (f, r, w, e, t);
}



/* Mutex functions */

static scm_mutex_t *
make_mutex (const scm_mutexattr_t * mutexattr)
{
  coop_m * data = (coop_m *) scm_must_malloc (sizeof (coop_m), "mutex");
  coop_mutex_init (data);
  return (scm_mutex_t *) data;
}


static int
mutex_lock (scm_mutex_t * mutex)
{
  return coop_mutex_lock ((coop_m *) mutex);
}


static int
mutex_trylock (scm_mutex_t * mutex)
{
  return coop_mutex_trylock ((coop_m *) mutex);
}


static int
mutex_unlock (scm_mutex_t * mutex)
{
  return coop_mutex_unlock ((coop_m *) mutex);
}


static size_t
mutex_free (scm_mutex_t * mutex)
{
  coop_mutex_destroy ((coop_m *) mutex);
  scm_must_free ((coop_m *) mutex);
  return sizeof (coop_m);
}



/* Condition variable functions */

static scm_cond_t *
make_cond (const scm_condattr_t * cond_attr)
{
  coop_c * data = (coop_c *) scm_must_malloc (sizeof (coop_c), "condvar");
  coop_condition_variable_init (data);
  return (scm_cond_t *) data;
}


static int
cond_signal (scm_cond_t * cond)
{
  return coop_condition_variable_signal ((coop_c *) cond);
}


static int
cond_broadcast (scm_cond_t * cond)
{
  scm_misc_error ("guile-coop.c:cond_broadcast", "Not implemented yet.", SCM_EOL);
}


static int
cond_wait (scm_cond_t * cond, scm_mutex_t * mutex)
{
  return coop_condition_variable_wait_mutex ((coop_c *) cond, (coop_m *) mutex);
}


static int
cond_timedwait (scm_cond_t * cond, scm_mutex_t * mutex, const struct timespec * abstime)
{
  return coop_condition_variable_timed_wait_mutex ((coop_c *) cond, (coop_m *) mutex, abstime);
}


static size_t
cond_free (scm_cond_t * cond)
{
  coop_condition_variable_destroy ((coop_c *) cond);
  scm_must_free ((coop_c *) cond);
  return sizeof (coop_c);
}



/* Key functions */

static int
key_create (scm_key_t * key, void (*destr_function) (void *))
{
  return coop_key_create ((coop_k *) key, destr_function);
}


static int
key_delete (scm_key_t key)
{
  return coop_key_delete ((coop_k) key);
}



/* Functions for thread specific data */

static int
set_thread_specific_data (scm_key_t key, const void * data)
{
  return coop_setspecific ((coop_k) key, data);
}


static void *
thread_specific_data (scm_key_t key)
{
  return coop_getspecific ((coop_k) key);
}


static void
set_thread_local_data (void * data)
{
  coop_global_curr->data = data;
}


static void *
thread_local_data (void)
{
  return coop_global_curr->data;
}



/* Garbage collection functions */

static void
threads_mark_stacks (void)
{
  coop_t *thread;
  
  for (thread = coop_global_allq.t.all_next; 
       thread != NULL; thread = thread->all_next)
    {
      if (thread == coop_global_curr)
	{
	  /* Active thread */
	  /* stack_len is long rather than sizet in order to guarantee
	     that &stack_len is long aligned */
#ifdef STACK_GROWS_UP
	  long stack_len = ((SCM_STACKITEM *) (&thread) -
			    (SCM_STACKITEM *) thread->base);
	  
	  /* Protect from the C stack.  This must be the first marking
	   * done because it provides information about what objects
	   * are "in-use" by the C code.   "in-use" objects are  those
	   * for which the values from SCM_LENGTH and SCM_CHARS must remain
	   * usable.   This requirement is stricter than a liveness
	   * requirement -- in particular, it constrains the implementation
	   * of scm_resizuve.
	   */
	  SCM_FLUSH_REGISTER_WINDOWS;
	  /* This assumes that all registers are saved into the jmp_buf */
	  setjmp (scm_save_regs_gc_mark);
	  scm_mark_locations ((SCM_STACKITEM *) scm_save_regs_gc_mark,
			      ((scm_sizet) sizeof scm_save_regs_gc_mark
			       / sizeof (SCM_STACKITEM)));
	  
	  scm_mark_locations (((size_t) thread->base,
			       (sizet) stack_len));
#else
	  long stack_len = ((SCM_STACKITEM *) thread->base -
			    (SCM_STACKITEM *) (&thread));
	  
	  /* Protect from the C stack.  This must be the first marking
	   * done because it provides information about what objects
	   * are "in-use" by the C code.   "in-use" objects are  those
	   * for which the values from SCM_LENGTH and SCM_CHARS must remain
	   * usable.   This requirement is stricter than a liveness
	   * requirement -- in particular, it constrains the implementation
	   * of scm_resizuve.
	   */
	  SCM_FLUSH_REGISTER_WINDOWS;
	  /* This assumes that all registers are saved into the jmp_buf */
	  setjmp (scm_save_regs_gc_mark);
	  scm_mark_locations ((SCM_STACKITEM *) scm_save_regs_gc_mark,
			      ((scm_sizet) sizeof scm_save_regs_gc_mark
			       / sizeof (SCM_STACKITEM)));
	  
	  scm_mark_locations ((SCM_STACKITEM *) &thread,
			      stack_len);
#endif
	}
      else
	{
	  /* Suspended thread */
#ifdef STACK_GROWS_UP
	  long stack_len = ((SCM_STACKITEM *) (thread->sp) -
			    (SCM_STACKITEM *) thread->base);

	  scm_mark_locations ((size_t)thread->base,
			      (sizet) stack_len);
#else
	  long stack_len = ((SCM_STACKITEM *) thread->base -
			    (SCM_STACKITEM *) (thread->sp));
	  
	  /* Registers are already on the stack. No need to mark. */
	  
	  scm_mark_locations ((SCM_STACKITEM *) (size_t)thread->sp,
			      stack_len);
#endif
	}

      /* Mark this thread's root */
      scm_gc_mark (((scm_root_state *) thread->data) -> handle);
    }
}



/* Functions to set parameters */

static void
set_threads_stack_size (size_t size)
{
  scm_misc_error ("guile-coop.c:set_threads_stack_size", "Not implemented yet.", SCM_EOL);
}



/* Initialization */

void
scm_threads_init (SCM_STACKITEM *i)
{
  coop_init();

  scm_thread_count = 1;

#ifndef GUILE_PTHREAD_COMPAT
  coop_global_main.sto = i;
#endif
  coop_global_main.base = i;
  coop_global_curr = &coop_global_main;
  coop_all_qput (&coop_global_allq, coop_global_curr);
  coop_global_main.data = 0; /* Initialized in init.c */


  scm_thread.make_thread = make_thread;
  scm_thread.thread_exit = thread_exit;
  scm_thread.thread_cancel = thread_cancel;
  scm_thread.thread_join = thread_join;
  scm_thread.thread_free = thread_free;

  scm_thread.thread_yield = thread_yield;
  scm_thread.thread_select = thread_select;

  scm_thread.make_mutex = make_mutex;
  scm_thread.mutex_lock = mutex_lock;
  scm_thread.mutex_trylock = mutex_trylock;
  scm_thread.mutex_unlock = mutex_unlock;
  scm_thread.mutex_free = mutex_free;

  scm_thread.make_cond = make_cond;
  scm_thread.cond_signal = cond_signal;
  scm_thread.cond_broadcast = cond_broadcast;
  scm_thread.cond_wait = cond_wait;
  scm_thread.cond_timedwait = cond_timedwait;
  scm_thread.cond_free = cond_free;

  scm_thread.key_create = key_create;
  scm_thread.key_delete = key_delete;

  scm_thread.set_thread_specific_data = set_thread_specific_data;
  scm_thread.thread_specific_data = thread_specific_data;
  scm_thread.set_thread_local_data = set_thread_local_data;
  scm_thread.thread_local_data = thread_local_data;

  scm_thread.threads_mark_stacks = threads_mark_stacks;

  scm_thread.set_threads_stack_size = set_threads_stack_size;
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
