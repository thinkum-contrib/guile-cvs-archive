/*	Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
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


typedef struct scm_pthread_info {
  queue q;		     /* the dequeue on which this structure exists */
  			     /* reqired to be the first element */
  pthread_t thread;	     /* the corresponding thread structure */
  void *stack_top;	     /* the highest address in this thread's stack */
  scm_root_state *root;      /* root for this thread */
} scm_pthread_info;

pthread_mutex_t scm_critical_section_mutex;
pthread_t scm_critical_section_owner;

static queue infos = { &infos, &infos };  /* the dequeue of info structures */

/* Key to thread specific data */
pthread_key_t info_key;

size_t
scm_threads_free_thread (SCM t)
{
  scm_must_free (SCM_THREAD_DATA (t));
  return sizeof (pthread_t);
}

size_t
scm_threads_free_mutex (SCM m)
{
  pthread_mutex_destroy (SCM_MUTEX_DATA (m));
  scm_must_free (SCM_MUTEX_DATA (m));
  return sizeof (pthread_mutex_t);
}

size_t
scm_threads_free_condvar (SCM c)
{
  pthread_cond_destroy (SCM_CONDVAR_DATA (c));
  scm_must_free (SCM_CONDVAR_DATA (c));
  return sizeof (pthread_cond_t);
}

/* cleanup for info structure
 */
static void
scm_pthread_delete_info (void *ptr)
{
  scm_pthread_info *info = (scm_pthread_info *) ptr;
  info->q.blink->flink = info->q.flink;
  info->q.flink->blink = info->q.blink;
  scm_must_free ((char *) info);
}

void
scm_threads_init (SCM_STACKITEM *i)
{
  /*
   * each info structure is made thread-specific, so that the cleanup
   * mechanism can be used to reclaim the space in a timely fashion.
   */
  pthread_key_create (&info_key, scm_pthread_delete_info);

  /* initialize various mutex variables */
  pthread_mutex_init (&scm_critical_section_mutex, NULL);

  /*
   * create an info structure for the initial thread and push it onto
   * the info dequeue
   */
  {
    scm_pthread_info *info;
    info = (scm_pthread_info *) scm_must_malloc (sizeof (scm_pthread_info),
						 "threads_init");
    infos.flink = infos.blink = &info->q;
    info->q.flink = info->q.blink = &infos;
    info->thread = pthread_initial;
    info->stack_top = (void *) i;
    pthread_setspecific(info_key, info);
  }
  /* The root state pointer gets initialized in init.c. */
}

/* given some thread, find the corresponding info
 */
static scm_pthread_info *pthreads_find_info (pthread_t target)
{
  queue *ptr = infos.flink;

  while (ptr != &infos)
    {
      scm_pthread_info *info = (scm_pthread_info *) ptr;

      if (info->thread == target)
        {
          return (info);
        }
      ptr = ptr->flink;
    }
}

void
scm_threads_mark_stacks ()
{
  scm_pthread_info *info;
  pthread_t thread;
  int j;
  
  for (info = (scm_pthread_info *) infos.flink;
       info != (scm_pthread_info *) &infos;
       info = (scm_pthread_info *) info->q.flink)
    {
      thread = info->thread;
      if (thread == pthread_run)
	{
	  /* Active thread */
	  /* stack_len is long rather than sizet in order to guarantee
	     that &stack_len is long aligned */
#ifdef STACK_GROWS_UP
	  long stack_len = ((SCM_STACKITEM *) (&thread) -
			    (SCM_STACKITEM *) info->stack_top);
	  
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
	  
	  scm_mark_locations (((size_t) info->stack_top,
			       (sizet) stack_len));
#else
	  long stack_len = ((SCM_STACKITEM *) info->stack_top -
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
	  long stack_len = ((SCM_STACKITEM *) (thread->THREAD_SP) -
			    (SCM_STACKITEM *) info->stack_top);
	      
	  scm_mark_locations ((size_t)info->stack_top,
			      (sizet) stack_len);
#else
	  long stack_len = ((SCM_STACKITEM *) info->stack_top -
			    (SCM_STACKITEM *) (thread->THREAD_SP));
	      
	  scm_mark_locations ((SCM_STACKITEM *) thread->machdep_data.machdep_state,
			      ((scm_sizet) sizeof (*thread->machdep_data.machdep_state)
			       / sizeof (SCM_STACKITEM)));
	  scm_mark_locations ((SCM_STACKITEM *) (size_t) thread->THREAD_SP,
			      stack_len);
#endif
	}

      /* Mark this thread's root */
      scm_gc_mark (((scm_root_state *) info->root) -> handle);
    }
}

void *
launch_thread (void *p)
{
  /* The thread object will be GC protected by being a member of the
     list given as argument to launch_thread.  It will be marked
     during the conservative sweep of the stack. */
  SCM args = (SCM) p;
  pthread_attr_setcleanup (&pthread_self () -> attr,
			   NULL,
			   SCM_ROOT_STATE (SCM_CAR (args)));
  scm_call_with_dynamic_root (SCM_CADDR (args), SCM_CADDDR (args));
  return NULL;
}

SCM
scm_call_with_new_thread (SCM argl)
{
  SCM thread;

  /* Check arguments. */
  {
    register SCM args = argl;
    SCM thunk, handler;
    SCM_ASSERT (SCM_NIMP (args),
		scm_makfrom0str (s_call_with_new_thread),
		SCM_WNA, NULL);
    thunk = SCM_CAR (args);
    SCM_ASSERT (SCM_NFALSEP (scm_thunk_p (thunk)),
		thunk,
		SCM_ARG1,
		s_call_with_new_thread);
    args = SCM_CDR (args);
    SCM_ASSERT (SCM_NIMP (args),
		scm_makfrom0str (s_call_with_new_thread),
		SCM_WNA, NULL);
    handler = SCM_CAR (args);
    SCM_ASSERT (SCM_NFALSEP (scm_procedure_p (handler)),
		handler,
		SCM_ARG2,
		s_call_with_new_thread);
    SCM_ASSERT (SCM_NULLP (SCM_CDR (args)),
		scm_makfrom0str (s_call_with_new_thread),
		SCM_WNA, NULL);
  }

  /* Make new thread. */
  {
    pthread_attr_t attr;
    pthread_t t;
    scm_pthread_info *info = 
      (scm_pthread_info *) scm_must_malloc (sizeof (scm_pthread_info),
					    "pthread_info");
    SCM root, old_winds;

    /* Unwind wind chain. */
    old_winds = scm_dynwinds;
    scm_dowinds (SCM_EOL, scm_ilength (scm_root->dynwinds));

    /* Allocate thread locals. */
    root = scm_make_root (scm_root->handle);
    /* Make thread. */
    SCM_NEWCELL (thread);
    SCM_DEFER_INTS;
    SCM_SETCAR (thread, scm_tc16_thread);
    argl = scm_cons2 (root, thread, argl);
  
    /* thread mustn't start until we've built the info struct */
    pthread_kernel_lock++;

    /* initialize and create the thread. */
    pthread_attr_init (&attr);
    pthread_attr_setschedpolicy (&attr, SCHED_RR);
  
    pthread_create (&t, &attr, launch_thread, (void *) argl);
    pthread_attr_destroy (&attr);

    /* push the info onto the dequeue */
    info->q.flink = infos.flink;
    info->q.blink = &infos;
    infos.flink->blink = &info->q;
    infos.flink = &info->q;
    /* pthread_create filled in the initial SP -- profitons-en ! */
    info->stack_top = (void *) (t->THREAD_SP);
    info->thread = t;
    info->root = SCM_ROOT_STATE (root);
    SCM_SETCDR (thread, t);
    SCM_ALLOW_INTS;
    
    /* we're now ready for the thread to begin */
    pthread_kernel_lock--;

    /* Return to old dynamic context. */
    scm_dowinds (old_winds, - scm_ilength (old_winds));
  }

  return thread;
}

SCM
scm_join_thread (SCM t)
{
  void *value;
  pthread_join (SCM_THREAD_DATA (t), &value);
  return SCM_BOOL_T;
}

SCM
scm_yield ()
{
  pthread_yield ();
  return SCM_BOOL_T;
}

SCM
scm_make_mutex ()
{
  SCM m;
  pthread_mutex_t *data = (pthread_mutex_t *) scm_must_malloc (sizeof (pthread_mutex_t), "mutex");
  SCM_NEWSMOB (m,scm_tc16_mutex, data);
  pthread_mutex_init (SCM_MUTEX_DATA (m), NULL);
  return m;
}

SCM
scm_lock_mutex (SCM m)
{
  SCM_ASSERT (SCM_MUTEXP (m), m, SCM_ARG1, s_lock_mutex);
  pthread_mutex_lock (SCM_MUTEX_DATA (m));
  return SCM_BOOL_T;
}

SCM
scm_unlock_mutex (SCM m)
{
  SCM_ASSERT (SCM_MUTEXP (m), m, SCM_ARG1, s_unlock_mutex);
  pthread_mutex_unlock (SCM_MUTEX_DATA (m));
  return SCM_BOOL_T;
}

SCM
scm_make_condition_variable ()
{
  SCM c;
  pthread_cond_t *data = (pthread_cond_t *) scm_must_malloc (sizeof (pthread_cond_t), "condvar");
  SCM_NEWSMOB (c, scm_tc16_condvar, data);
  pthread_cond_init (SCM_CONDVAR_DATA (c), NULL);
  return c;
}

SCM
scm_wait_condition_variable (SCM c, SCM m)
{
  SCM_ASSERT (SCM_CONDVARP (c),
	      c,
	      SCM_ARG1,
	      s_wait_condition_variable);
  SCM_ASSERT (SCM_MUTEXP (m),
	      m,
	      SCM_ARG2,
	      s_wait_condition_variable);
  pthread_cond_wait (SCM_CONDVAR_DATA (m), SCM_MUTEX_DATA (c));
  return SCM_BOOL_T;
}

SCM
scm_signal_condition_variable (SCM c)
{
  SCM_ASSERT (SCM_CONDVARP (c),
	      c,
	      SCM_ARG1,
	      s_signal_condition_variable);
  pthread_cond_signal (SCM_CONDVAR_DATA (c));
  return SCM_BOOL_T;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
