/*	Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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


/* $Id$ */

/* Cooperative thread library, based on QuickThreads */

#include <stdio.h>
#include "qt/qt.h"
#include "libguile/coop.h"

#ifdef HAVE_UNISTD_H 
#include <unistd.h>
#endif

#include <errno.h>

static void init_iselect (void);
static coop_t * coop_next_runnable_thread (void);
static coop_t * coop_wait_for_runnable_thread (void);



/* Dirk:Note:: had to change this in order not to use references to guile */
#define COOP_STKSIZE (0x10000)
/* #define COOP_STKSIZE (scm_eval_stack) */

/* `alignment' must be a power of 2. */
#define COOP_STKALIGN(sp, alignment) \
((void *)((((qt_word_t)(sp)) + (alignment) - 1) & ~((alignment)-1)))



/* Queue access functions. */

static void
coop_qinit (coop_q_t *q)
{
  q->t.next = q->tail = &q->t;

  q->t.all_prev = NULL;
  q->t.all_next = NULL;
#ifdef GUILE_ISELECT
  q->t.nfds = 0;
  q->t.readfds = NULL;
  q->t.writefds = NULL;
  q->t.exceptfds = NULL;
  q->t.timeoutp = 0;
#endif
}


/* Dirk:Note:: This was externally visible, but only for iselect.c */
static coop_t *
coop_qget (coop_q_t *q)
{
  coop_t *t;

  t = q->t.next;
  q->t.next = t->next;
  if (t->next == &q->t)
    {
      if (t == &q->t)
	{			/* If it was already empty .. */
	  return NULL;		/* .. say so. */
	}
      q->tail = &q->t;		/* Else now it is empty. */
    }
  return (t);
}


/* Dirk:Note:: This was externally visible, but only for iselect.c */
static void
coop_qput (coop_q_t *q, coop_t *t)
{
  q->tail->next = t;
  t->next = &q->t;
  q->tail = t;
}

/* Dirk:Note:: This was static, but is needed(?) for guile-coop.c:scm_threads_init */
void
coop_all_qput (coop_q_t *q, coop_t *t)
{
  if (q->t.all_next)
    q->t.all_next->all_prev = t;
  t->all_prev = NULL;
  t->all_next = q->t.all_next;
  q->t.all_next = t;
}

static void
coop_all_qremove (coop_q_t *q, coop_t *t)
{
  if (t->all_prev)
    t->all_prev->all_next = t->all_next;
  else
    q->t.all_next = t->all_next;
  if (t->all_next)
      t->all_next->all_prev = t->all_prev;
}

#ifdef GUILE_ISELECT
/* Insert thread t into the ordered queue q.
   q is ordered after wakeup_time.  Threads which aren't sleeping but
   waiting for I/O go last into the queue. */
/* Dirk:Note:: This was externally visible, but only for iselect.c */
static void
coop_timeout_qinsert (coop_q_t *q, coop_t *t)
{
  coop_t *pred = &q->t;
  int sec = t->wakeup_time.tv_sec;
  int usec = t->wakeup_time.tv_usec;
  while (pred->next != &q->t
	 && pred->next->timeoutp
	 && (pred->next->wakeup_time.tv_sec < sec
	     || (pred->next->wakeup_time.tv_sec == sec
		 && pred->next->wakeup_time.tv_usec < usec)))
    pred = pred->next;
  t->next = pred->next;
  pred->next = t;
  if (t->next == &q->t)
    q->tail = t;
}
#endif


/* Thread routines. */

coop_q_t coop_global_runq;	/* A queue of runable threads. */
/* Dirk:Note:: This was externally visible, but only for iselect.c */
static coop_q_t coop_global_sleepq;	/* A queue of sleeping threads. */
/* Dirk:Note:: This was externally visible, but only for iselect.c */
static coop_q_t coop_tmp_queue;        /* A temp working queue */
coop_q_t coop_global_allq;      /* A queue of all threads. */
/* Dirk:Note:: This was static, but is needed(?) for guile-coop.c:scm_threads_init */
coop_t coop_global_main;        /* Thread for the process. */
coop_t *coop_global_curr;	/* Currently-executing thread. */

#ifdef GUILE_PTHREAD_COMPAT
static coop_q_t coop_deadq;
static int coop_quitting_p = -1;
static pthread_cond_t coop_cond_quit;
static pthread_cond_t coop_cond_create;
static pthread_mutex_t coop_mutex_create;
static pthread_t coop_mother;
static coop_t *coop_child;
#endif

static void *coop_starthelp (qt_t *old, void *ignore0, void *ignore1);
static void coop_only (void *pu, void *pt, qt_userf_t *f);
static void *coop_aborthelp (qt_t *sp, void *old, void *null);
static void *coop_yieldhelp (qt_t *sp, void *old, void *blockq);

static int I_am_dead;


/* called on process termination.  */
#ifdef HAVE_ATEXIT
static void
coop_finish (void)
#else
#ifdef HAVE_ON_EXIT
extern int on_exit (void (*procp) (), int arg);

static void
coop_finish (int status, void *arg)
#else
#error Dont know how to setup a cleanup handler on your system.
#endif
#endif
{
#ifdef GUILE_PTHREAD_COMPAT
  coop_quitting_p = 1;
  pthread_cond_signal (&coop_cond_create);
  pthread_cond_broadcast (&coop_cond_quit);
#endif
}

void
coop_init ()
{
  /* Dirk:FIXME:: This is a temporary solution, in order not to change the
   * code too much at the moment. 
   */
  init_iselect ();

  coop_qinit (&coop_global_runq);
  coop_qinit (&coop_global_sleepq);
  coop_qinit (&coop_tmp_queue);
  coop_qinit (&coop_global_allq);
  coop_global_curr = &coop_global_main;
#ifdef GUILE_PTHREAD_COMPAT
  coop_qinit (&coop_deadq);
  pthread_cond_init (&coop_cond_quit, NULL);
  pthread_cond_init (&coop_cond_create, NULL);
  pthread_mutex_init (&coop_mutex_create, NULL);
#endif
#ifdef HAVE_ATEXIT
  atexit (coop_finish);
#else
#ifdef HAVE_ON_EXIT
  on_exit (coop_finish, 0);
#endif
#endif
}

/* Return the next runnable thread. If no threads are currently runnable,
   and there are sleeping threads - wait until one wakes up. Otherwise,
   return NULL. */

#ifndef GUILE_ISELECT
/* Dirk:Note:: This was externally visible, but only for iselect.c */
static coop_t *
coop_next_runnable_thread()
{
  int sleepers;
  coop_t *t;
  time_t now;

  do {
    sleepers = 0;
    now = time(NULL);

    /* Check the sleeping queue */
    while ((t = coop_qget(&coop_global_sleepq)) != NULL)
      {
	sleepers++;
	if (t->wakeup_time <= now)
	  coop_qput(&coop_global_runq, t);
	else
	  coop_qput(&coop_tmp_queue, t);
      }
    while ((t = coop_qget(&coop_tmp_queue)) != NULL)
      coop_qput(&coop_global_sleepq, t);
    
    t = coop_qget (&coop_global_runq);

  } while ((t == NULL) && (sleepers > 0));

  return t;
}
#endif

void
coop_start()
{
  coop_t *next;

  while ((next = coop_qget (&coop_global_runq)) != NULL) {
    coop_global_curr = next;
    QT_BLOCK (coop_starthelp, 0, 0, next->sp);
  }
}


static void *
coop_starthelp (qt_t *old, void *ignore0, void *ignore1)
{
  coop_global_main.sp = old;
  coop_global_main.joining = NULL;
  coop_qput (&coop_global_runq, &coop_global_main);
  return NULL; /* not used, but keeps compiler happy */
}

int
coop_mutex_init (coop_m *m)
{
  return coop_new_mutex_init (m, NULL);
}

int
coop_new_mutex_init (coop_m *m, coop_mattr *attr)
{
  m->owner = NULL;
  coop_qinit(&(m->waiting));
  return 0;
}

int
coop_mutex_trylock (coop_m *m)
{
  if (m->owner == NULL)
    {
      m->owner = coop_global_curr;
      return 0;
    }
  else
    return EBUSY;
}

int
coop_mutex_lock (coop_m *m)
{
  if (m->owner == NULL)
    {
      m->owner = coop_global_curr;
    }
  else
    {
      coop_t *old, *newthread;

      /* Record the current top-of-stack before going to sleep */
      coop_global_curr->top = &old;

#ifdef GUILE_ISELECT
      newthread = coop_wait_for_runnable_thread();
      if (newthread == coop_global_curr)
	coop_abort ();
#else
      newthread = coop_next_runnable_thread();
#endif
      old = coop_global_curr;
      coop_global_curr = newthread;
      QT_BLOCK (coop_yieldhelp, old, &(m->waiting), newthread->sp);
    }
  return 0;
}


int 
coop_mutex_unlock (coop_m *m)
{
  coop_t *old, *newthread;
  
  newthread = coop_qget (&(m->waiting));
  if (newthread != NULL)
    {
      /* Record the current top-of-stack before going to sleep */
      coop_global_curr->top = &old;

      old = coop_global_curr;
      coop_global_curr = newthread;
      /* The new thread came into m->waiting through a lock operation.
	 It now owns this mutex. */
      m->owner = coop_global_curr;
      QT_BLOCK (coop_yieldhelp, old, &coop_global_runq, newthread->sp);
    }
  else
    {
      m->owner = NULL;
    }
  return 0;
}


int 
coop_mutex_destroy (coop_m *m)
{
  return 0;
}


int 
coop_condition_variable_init (coop_c *c)
{
  return coop_new_condition_variable_init (c, NULL);
}

int
coop_new_condition_variable_init (coop_c *c, coop_cattr *a)
{
  coop_qinit(&(c->waiting));
  return 0;
}

int 
coop_condition_variable_wait_mutex (coop_c *c, coop_m *m)
{
  coop_t *old, *newthread;

  /* coop_mutex_unlock (m); */
  newthread = coop_qget (&(m->waiting));
  if (newthread != NULL)
    {
      m->owner = newthread;
    }
  else
    {
      m->owner = NULL;
      /*fixme* Should we really wait here?  Isn't it OK just to proceed? */
#ifdef GUILE_ISELECT
      newthread = coop_wait_for_runnable_thread();
      if (newthread == coop_global_curr)
	coop_abort ();
#else
      newthread = coop_next_runnable_thread();
#endif
    }
  coop_global_curr->top = &old;
  old = coop_global_curr;
  coop_global_curr = newthread;
  QT_BLOCK (coop_yieldhelp, old, &(c->waiting), newthread->sp);

  coop_mutex_lock (m);
  return 0;
}

int 
coop_condition_variable_timed_wait_mutex (coop_c *c,
					  coop_m *m,
					  const struct timespec *abstime)
{
  coop_t *old, *t;
  int res = ETIMEDOUT;

  /* coop_mutex_unlock (m); */
  t = coop_qget (&(m->waiting));
  if (t != NULL)
    {
      m->owner = t;
    }
  else
    {
      m->owner = NULL;
#ifdef GUILE_ISELECT
      coop_global_curr->timeoutp = 1;
      coop_global_curr->wakeup_time.tv_sec = abstime->tv_sec;
      coop_global_curr->wakeup_time.tv_usec = abstime->tv_nsec / 1000;
      coop_timeout_qinsert (&coop_global_sleepq, coop_global_curr);
      t = coop_wait_for_runnable_thread();
#else
      /*fixme* Implement!*/
      t = coop_next_runnable_thread();
#endif
    }
  if (t != coop_global_curr)
    {
      coop_global_curr->top = &old;
      old = coop_global_curr;
      coop_global_curr = t;
      QT_BLOCK (coop_yieldhelp, old, &(c->waiting), t->sp);

      /* Are we still in the sleep queue? */
      old = &coop_global_sleepq.t;
      for (t = old->next; t != &coop_global_sleepq.t; old = t, t = t->next)
	if (t == coop_global_curr)
	  {
	    old->next = t->next; /* unlink */
	    res = 0;
	    break;
	  }
    }
  coop_mutex_lock (m);
  return res;
}

int 
coop_condition_variable_signal (coop_c *c)
{
  coop_t *newthread;

  while ((newthread = coop_qget (&(c->waiting))) != NULL)
    {
      coop_qput (&coop_global_runq, newthread);
    }
  return 0;
}

/* {Keys}
 */

static int n_keys = 0;
static int max_keys = 0;
static void (**destructors) (void *) = 0;

int
coop_key_create (coop_k *keyp, void (*destructor) (void *value))
{
  if (n_keys >= max_keys)
    {
      int i;
      max_keys = max_keys ? max_keys * 3 / 2 : 10;
      destructors = realloc (destructors, sizeof (void *) * max_keys);
      if (destructors == 0)
	{
	  fprintf (stderr, "Virtual memory exceeded in coop_key_create\n");
	  exit (1);
	}
      for (i = n_keys; i < max_keys; ++i)
	destructors[i] = NULL;
    }
  destructors[n_keys] = destructor;
  *keyp = n_keys++;
  return 0;
}

int
coop_setspecific (coop_k key, const void *value)
{
  int n_keys = coop_global_curr->n_keys;
  if (key >= n_keys)
    {
      int i;
      coop_global_curr->n_keys = max_keys;
      coop_global_curr->specific = realloc (n_keys
					    ? coop_global_curr->specific
					    : NULL,
					    sizeof (void *) * max_keys);
      if (coop_global_curr->specific == 0)
	{
	  fprintf (stderr, "Virtual memory exceeded in coop_setspecific\n");
	  exit (1);
	}
      for (i = n_keys; i < max_keys; ++i)
	coop_global_curr->specific[i] = NULL;
    }
  coop_global_curr->specific[key] = (void *) value;
  return 0;
}

void *
coop_getspecific (coop_k key)
{
  return (key < coop_global_curr->n_keys
	  ? coop_global_curr->specific[key]
	  : NULL);
}

int
coop_key_delete (coop_k key)
{
  return 0;
}


int 
coop_condition_variable_destroy (coop_c *c)
{
  return 0;
}

#ifdef GUILE_PTHREAD_COMPAT

/* 1K room for the cond wait routine */
#ifdef SCM_STACK_GROWS_UP
#define COOP_STACK_ROOM (256)
#else
#define COOP_STACK_ROOM (-256)
#endif

static void *
dummy_start (void *coop_thread)
{
  coop_t *t = (coop_t *) coop_thread;
  int res;
  t->sp = (qt_t *) (&t + COOP_STACK_ROOM);
  pthread_mutex_init (&t->dummy_mutex, NULL);
  pthread_mutex_lock (&t->dummy_mutex);
  coop_child = 0;
  do
    res = pthread_cond_wait (&coop_cond_quit, &t->dummy_mutex);
  while (res == EINTR);
  return 0;
}

static void *
mother (void *dummy)
{
  pthread_mutex_lock (&coop_mutex_create);
  while (!coop_quitting_p)
    {
      int res;
      pthread_create (&coop_child->dummy_thread,
		      NULL,
		      dummy_start,
		      coop_child);
      do
	res = pthread_cond_wait (&coop_cond_create, &coop_mutex_create);
      while (res == EINTR);
    }
  return 0;
}

#endif

coop_t *
coop_create (coop_userf_t *f, void *pu)
{
  coop_t *t;
#ifndef GUILE_PTHREAD_COMPAT
  void *sto;
#endif

#ifdef GUILE_PTHREAD_COMPAT
  t = coop_qget (&coop_deadq);
  if (t)
    {
      t->sp = t->base;
      t->specific = 0;
      t->n_keys = 0;
    }
  else
#endif
    {
      t = malloc (sizeof (coop_t));

      t->specific = NULL;
      t->n_keys = 0;
#ifdef GUILE_PTHREAD_COMPAT
      coop_child = t;
      if (coop_quitting_p < 0)
	{
	  coop_quitting_p = 0;
	  /* We can't create threads ourselves since the pthread
	   * corresponding to this stack might be sleeping.
	   */
	  pthread_create (&coop_mother, NULL, mother, NULL);
	}
      else
	{
	  pthread_cond_signal (&coop_cond_create);
	}
      /* We can't use a pthreads condition variable since "this"
       * pthread could already be asleep.  We can't use a COOP
       * condition variable because they are not safe against
       * pre-emptive switching.
       */
      while (coop_child)
	usleep (0);
#else
      t->sto = malloc (COOP_STKSIZE);
      sto = COOP_STKALIGN (t->sto, QT_STKALIGN);
      t->sp = QT_SP (sto, COOP_STKSIZE - QT_STKALIGN);
#endif
      t->base = t->sp;
    }
  t->sp = QT_ARGS (t->sp, pu, t, (qt_userf_t *)f, coop_only);
  t->joining = NULL;
  coop_qput (&coop_global_runq, t);
  coop_all_qput (&coop_global_allq, t);

  return t;
}


static void
coop_only (void *pu, void *pt, qt_userf_t *f)
{
  coop_global_curr = (coop_t *)pt;
  (*(coop_userf_t *)f)(pu);
  coop_abort();
  /* NOTREACHED */
}


void
coop_abort ()
{
  coop_t *old, *newthread;

  /* Wake up any threads that are waiting to join this one */
  if (coop_global_curr->joining)
    {
      while ((newthread = coop_qget ((coop_q_t *)(coop_global_curr->joining)))
	     != NULL)
	{
	  coop_qput (&coop_global_runq, newthread);
	}
      free (coop_global_curr->joining);
    }

#ifdef GUILE_ISELECT
  I_am_dead = 1;
  do {
    newthread = coop_wait_for_runnable_thread();
  } while (newthread == coop_global_curr);
  I_am_dead = 0;
#else
  newthread = coop_next_runnable_thread();
#endif
  coop_all_qremove (&coop_global_allq, coop_global_curr);
  old = coop_global_curr;
  coop_global_curr = newthread;
  QT_ABORT (coop_aborthelp, old, (void *) NULL, newthread->sp);
}


static void *
coop_aborthelp (qt_t *sp, void *old, void *null)
{
  coop_t *oldthread = (coop_t *) old;

  if (oldthread->specific)
    free (oldthread->specific);
#ifndef GUILE_PTHREAD_COMPAT
  free (oldthread->sto);
  free (oldthread);
#else
  coop_qput (&coop_deadq, oldthread);
#endif
  
  return NULL;
}


void 
coop_join(coop_t *t)
{
  coop_t *old, *newthread;
  
  /* Create a join list if necessary */
  if (t->joining == NULL)
    {
      t->joining = malloc(sizeof(coop_q_t));
      coop_qinit((coop_q_t *) t->joining);
    }

#ifdef GUILE_ISELECT
  newthread = coop_wait_for_runnable_thread();
  if (newthread == coop_global_curr)
    return;
#else
  newthread = coop_next_runnable_thread();
#endif
  old = coop_global_curr;
  coop_global_curr = newthread;
  QT_BLOCK (coop_yieldhelp, old, (coop_q_t *) t->joining, newthread->sp);
}

void
coop_yield()
{
  coop_t *old = NULL;
  coop_t *newthread;

  newthread = coop_next_runnable_thread();

  /* There may be no other runnable threads. Return if this is the 
     case. */
#if GUILE_ISELECT
  if (newthread == coop_global_curr)
      return;
#else
  if (newthread == NULL)
      return;
#endif

  old = coop_global_curr;

  coop_global_curr = newthread;
  QT_BLOCK (coop_yieldhelp, old, &coop_global_runq, newthread->sp);
}


static void *
coop_yieldhelp (qt_t *sp, void *old, void *blockq)
{
  ((coop_t *)old)->sp = sp;
  coop_qput ((coop_q_t *)blockq, (coop_t *)old);
  return NULL;
}

/* Replacement for the system's sleep() function. Does the right thing
   for the process - but not for the system (it busy-waits) */

/* Dirk:Note:: This was externally visible, but only for iselect.c */
static void *
coop_sleephelp (qt_t *sp, void *old, void *blockq)
{
  ((coop_t *)old)->sp = sp;
  /* old is already on the sleep queue - so there's no need to
     do anything extra here */
  return NULL;
}


/*****************************************************************************
Here starts the content of iselect.c:
*****************************************************************************/


#ifdef MISSING_BZERO_DECL
extern void bzero (void *, size_t);
#endif



/* COOP queue macros */
#define QEMPTYP(q) (q.t.next == &q.t)
#define QFIRST(q) (q.t.next)

/* These macros count the number of bits in a word.  */
#define SCM_BITS_PER_LONG (8 * sizeof (unsigned long))
/* Use LONG_MAX instead of ULONG_MAX here since not all systems define
   ULONG_MAX */
#if LONG_MAX >> 16 == 0
#define SCM_NLONGBITS(p) (bc[((unsigned char *)(p))[0]]\
			  + bc[((unsigned char *)(p))[1]])
#elif LONG_MAX >> 32 == 0
#define SCM_NLONGBITS(p) (bc[((unsigned char *)(p))[0]]\
			  + bc[((unsigned char *)(p))[1]]\
			  + bc[((unsigned char *)(p))[2]]\
			  + bc[((unsigned char *)(p))[3]])
#elif LONG_MAX >> 64 == 0
#define SCM_NLONGBITS(p) (bc[((unsigned char *)(p))[0]]\
			  + bc[((unsigned char *)(p))[1]]\
			  + bc[((unsigned char *)(p))[2]]\
			  + bc[((unsigned char *)(p))[3]]\
			  + bc[((unsigned char *)(p))[4]]\
			  + bc[((unsigned char *)(p))[5]]\
			  + bc[((unsigned char *)(p))[6]]\
			  + bc[((unsigned char *)(p))[7]])
#endif

#ifdef HAVE_BZERO
#define FD_ZERO_N(pos, n) bzero ((pos), (n))
#else
#define FD_ZERO_N(pos, n) memset ((void *) (pos), 0, (n))
#endif

typedef unsigned long *ulongptr;

static char bc[256]; /* Bit counting array.  bc[x] is the number of
			bits in x. */

/* This flag indicates that several threads are waiting on the same
   file descriptor.  When this is the case, the common fd sets are
   updated in a more inefficient way.  */
int collisionp;

/* These are the common fd sets.  When new select calls are made,
   those sets are merged into these.  */
int gnfds;
SELECT_TYPE greadfds;
SELECT_TYPE gwritefds;
SELECT_TYPE gexceptfds;

/* These are the result sets.  They are used when we call OS select.
   We couldn't use the common fd sets above, since that would destroy
   them.  */
SELECT_TYPE rreadfds;
SELECT_TYPE rwritefds;
SELECT_TYPE rexceptfds;

/* Constant timeval struct representing a zero timeout which we use
   when polling.  */
static struct timeval timeout0;

/* As select, but doesn't destroy the file descriptor sets passed as
   arguments.  The results are stored into the result sets.  */
static int
safe_select (int nfds,
	     SELECT_TYPE *readfds,
	     SELECT_TYPE *writefds,
	     SELECT_TYPE *exceptfds,
	     struct timeval *timeout)
{
  int n = (nfds + 7) / 8;
  /* Copy file descriptor sets to result area */
  if (readfds == NULL)
    FD_ZERO (&rreadfds);
  else
    {
      memcpy (&rreadfds, readfds, n);
      FD_ZERO_N ((char *) &rreadfds + n, SELECT_SET_SIZE / 8 - n);
    }
  if (writefds == NULL)
    FD_ZERO (&rwritefds);
  else
    {
      memcpy (&rwritefds, writefds, n);
      FD_ZERO_N ((char *) &rwritefds + n, SELECT_SET_SIZE / 8 - n);
    }
  if (exceptfds == NULL)
    FD_ZERO (&rexceptfds);
  else
    {
      memcpy (&rexceptfds, exceptfds, n);
      FD_ZERO_N ((char *) &rexceptfds + n, SELECT_SET_SIZE / 8 - n);
    }
  return select (nfds, &rreadfds, &rwritefds, &rexceptfds, timeout);
}

/* Merge new file descriptor sets into the common sets.  */
static void
add_fd_sets (coop_t *t)
{
  int n = (t->nfds + SCM_BITS_PER_LONG - 1) / SCM_BITS_PER_LONG;
  int i;

  /* Detect if the fd sets of the thread have any bits in common with
     the rest of the waiting threads.  If that is so, set the
     collision flag.  This causes a more time consuming handling of
     the common fd sets---they need to recalculated every time a
     thread wakes up.  */
  if (!collisionp)
    for (i = 0; i < n; ++i)
      if ((t->readfds != NULL
	   && (((ulongptr) t->readfds)[i] & ((ulongptr) &greadfds)[i]) != 0)
	  || (t->writefds != NULL
	      && ((((ulongptr) t->writefds)[i] & ((ulongptr) &gwritefds)[i])
		  != 0))
	  || (t->exceptfds != NULL
	      && ((((ulongptr) t->exceptfds)[i] & ((ulongptr) &gexceptfds)[i])
		  != 0)))
	{
	  collisionp = 1;
	  break;
	}
  
  /* We recalculate nfds below.  The cost for this can be paid back
     with a great bonus since many programs are lazy with the nfds
     arg.  Many even pass 1024 when using one of the lowest fd:s!

     We approach from above, checking for non-zero bits.  As soon as
     we have determined the value of nfds, we jump down to code below
     which concludes the updating of the common sets.  */
  t->nfds = 0;
  i = n;
  while (i > 0)
    {
      --i;
      if (t->readfds != NULL && ((ulongptr) t->readfds)[i] != 0)
	{
	  ((ulongptr) &greadfds)[i] |= ((ulongptr) t->readfds)[i];
	  n = (i + 1) * SCM_BITS_PER_LONG;
	  t->nfds = n;
	  if (n > gnfds)
	    gnfds = n;
	  goto cont_read;
	}
      if (t->writefds != NULL && ((ulongptr) t->writefds)[i] != 0)
	{
	  ((ulongptr) &gwritefds)[i] |= ((ulongptr) t->writefds)[i];
	  n = (i + 1) * SCM_BITS_PER_LONG;
	  t->nfds = n;
	  if (n > gnfds)
	    gnfds = n;
	  goto cont_write;
	}
      if (t->exceptfds != NULL && ((ulongptr) t->exceptfds)[i] != 0)
	{
	  ((ulongptr) &gexceptfds)[i] |= ((ulongptr) t->exceptfds)[i];
	  n = (i + 1) * SCM_BITS_PER_LONG;
	  t->nfds = n;
	  if (n > gnfds)
	    gnfds = n;
	  goto cont_except;
	}
    }
  return;

  /* nfds is now determined.  Just finish updating the common sets.  */
  while (i > 0)
    {
      --i;
      if (t->readfds != NULL && ((ulongptr) t->readfds)[i] != 0)
	((ulongptr) &greadfds)[i] |= ((ulongptr) t->readfds)[i];
    cont_read:
      if (t->writefds != NULL && ((ulongptr) t->writefds)[i] != 0)
	((ulongptr) &gwritefds)[i] |= ((ulongptr) t->writefds)[i];
    cont_write:
      if (t->exceptfds != NULL && ((ulongptr) t->exceptfds)[i] != 0)
	((ulongptr) &gexceptfds)[i] |= ((ulongptr) t->exceptfds)[i];
    cont_except:
      ;
    }
}

/* Update the fd sets pointed to by the thread so that they reflect
   the status of the file descriptors which the thread was interested
   in.  Also clear those bits in the common sets.  This function is
   only called when there are no bit collisions.  */
static void
finalize_fd_sets (coop_t *t)
{
  int i = (t->nfds + SCM_BITS_PER_LONG - 1) / SCM_BITS_PER_LONG;
  int n_ones = 0;
  register unsigned long s;

  if (t->nfds == gnfds)
    {
      /* This thread is the one responsible for the current high value
	 of gnfds.  First do our other jobs while at the same time
	 trying to decrease gnfds.  */
      while (i > 0)
	{
	  --i;
	  if (t->readfds != NULL && (s = ((ulongptr) t->readfds)[i]) != 0)
	    {
	      ((ulongptr) t->readfds)[i] &= ((ulongptr) &rreadfds)[i];
	      ((ulongptr) &greadfds)[i] &= ~s;
	      n_ones += SCM_NLONGBITS (&((ulongptr) t->readfds)[i]);
	    }
	  if (((ulongptr) &greadfds)[i] != 0)
	    {
	      gnfds = (i + 1) * SCM_BITS_PER_LONG;
	      goto cont_read;
	    }
	  if (t->writefds != NULL && (s = ((ulongptr) t->writefds)[i]) != 0)
	    {
	      ((ulongptr) t->writefds)[i] &= ((ulongptr) &rwritefds)[i];
	      ((ulongptr) &gwritefds)[i] &= ~s;
	      n_ones += SCM_NLONGBITS (&((ulongptr) t->writefds)[i]);
	    }
	  if (((ulongptr) &gwritefds)[i] != 0)
	    {
	      gnfds = (i + 1) * SCM_BITS_PER_LONG;
	      goto cont_write;
	    }
	  if (t->exceptfds != NULL && (s = ((ulongptr) t->exceptfds)[i]) != 0)
	    {
	      ((ulongptr) t->exceptfds)[i] &= ((ulongptr) &rexceptfds)[i];
	      ((ulongptr) &gexceptfds)[i] &= ~s;
	      n_ones += SCM_NLONGBITS (&((ulongptr) t->exceptfds)[i]);
	    }
	  if (((ulongptr) &gexceptfds)[i] != 0)
	    {
	      gnfds = (i + 1) * SCM_BITS_PER_LONG;
	      goto cont_except;
	    }
	}
      gnfds = 0;
      t->retval = n_ones;
      return;
    }

  /* Either this thread wasn't responsible for gnfds or gnfds has been
     determined.  */
  while (i > 0)
    {
      --i;
      if (t->readfds != NULL && (s = ((ulongptr) t->readfds)[i]) != 0)
	{
	  ((ulongptr) t->readfds)[i] &= ((ulongptr) &rreadfds)[i];
	  ((ulongptr) &greadfds)[i] &= ~s;
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->readfds)[i]);
	}
    cont_read:
      if (t->writefds != NULL && (s = ((ulongptr) t->writefds)[i]) != 0)
	{
	  ((ulongptr) t->writefds)[i] &= ((ulongptr) &rwritefds)[i];
	  ((ulongptr) &gwritefds)[i] &= ~s;
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->writefds)[i]);
	}
    cont_write:
      if (t->exceptfds != NULL && (s = ((ulongptr) t->exceptfds)[i]) != 0)
	{
	  ((ulongptr) t->exceptfds)[i] &= ((ulongptr) &rexceptfds)[i];
	  ((ulongptr) &gexceptfds)[i] &= ~s;
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->exceptfds)[i]);
	}
    cont_except:
      ;
    }
  t->retval = n_ones;
}

/* Just like finalize_fd_sets except that we don't have to update the
   global fd sets.  Those will be recalulated elsewhere.  */
static void
finalize_fd_sets_lazily (coop_t *t)
{
  int i = (t->nfds + SCM_BITS_PER_LONG - 1) / SCM_BITS_PER_LONG;
  int n_ones = 0;
  while (i > 0)
    {
      --i;
      if (t->readfds != NULL && ((ulongptr) t->readfds)[i] != 0)
	{
	  ((ulongptr) t->readfds)[i] &= ((ulongptr) &rreadfds)[i];
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->readfds)[i]);
	}
      if (t->writefds != NULL && ((ulongptr) t->writefds)[i] != 0)
	{
	  ((ulongptr) t->writefds)[i] &= ((ulongptr) &rwritefds)[i];
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->writefds)[i]);
	}
      if (t->exceptfds != NULL && ((ulongptr) t->exceptfds)[i] != 0)
	{
	  ((ulongptr) t->exceptfds)[i] &= ((ulongptr) &rexceptfds)[i];
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->exceptfds)[i]);
	}
    }
  t->retval = n_ones;
}

/* Return first fd with a non-zero bit in any of the result sets.  */
static int
first_interesting_fd (void)
{
  int i = 0;
  SELECT_TYPE *s;
  while (1)
    {
      if (((ulongptr) &rreadfds)[i] != 0)
	{
	  s = &rreadfds;
	  break;
	}
      if (((ulongptr) &rwritefds)[i] != 0)
	{
	  s = &rwritefds;
	  break;
	}
      if (((ulongptr) &rexceptfds)[i] != 0)
	{
	  s = &rexceptfds;
	  break;
	}
      ++i;
    }
  i *= SCM_BITS_PER_LONG;
  while (i < gnfds)
    {
      if (FD_ISSET (i, s))
	return i;
      ++i;
    }
  fprintf (stderr, "first_interesting_fd: internal error\n");
  exit (1);
}

/* Revive all threads with an error status.  */
/* Dirk:Note:: This was externally visible, but only for iselect.c */
static void
error_revive_threads (void)
{
  coop_t *t;
  
  while ((t = coop_qget (&coop_global_sleepq)) != NULL)
    {
      t->_errno = errno;
      t->retval = -1;
      if (t != coop_global_curr)
	coop_qput (&coop_global_runq, t);
    }
  collisionp = 0;
  gnfds = 0;
  FD_ZERO (&greadfds);
  FD_ZERO (&gwritefds);
  FD_ZERO (&gexceptfds);
}

/* Given the result of a call to safe_select and the current time,
   try to wake up some threads and return the first one.  Return NULL
   if we couldn't find any.  */
static coop_t *
find_thread (int n, struct timeval *now, int sleepingp)
{
  coop_t *t;
  int fd;

  if (n < 0)
    /* An error or a signal has occured.  Wake all threads.  Since we
       don't care to calculate if there is a sinner we report the
       error to all of them.  */
    {
      error_revive_threads ();
      if (!I_am_dead)
	return coop_global_curr;
    }
  else if (n == 0)
    {
      while (!QEMPTYP (coop_global_sleepq)
	     && (t = QFIRST (coop_global_sleepq))->timeoutp
	     && (t->wakeup_time.tv_sec < now->tv_sec
		 || (t->wakeup_time.tv_sec == now->tv_sec
		     && t->wakeup_time.tv_usec <= now->tv_usec)))
	{
	  coop_qget (&coop_global_sleepq);
	  if (collisionp)
	    finalize_fd_sets_lazily (t);
	  else
	    finalize_fd_sets (t);
	  coop_qput (&coop_global_runq, t);
	}
      if (collisionp)
	{
	  while ((t = coop_qget (&coop_global_sleepq)) != NULL)
	    coop_qput (&coop_tmp_queue, t);
	  goto rebuild_global_fd_sets;
	}
    }
  else if (n > 0)
    {
      /* Find the first interesting file descriptor */
      fd = first_interesting_fd ();
      /* Check the sleeping queue for this file descriptor.
	 Other file descriptors will be handled next time
	 coop_next_runnable_thread is called. */
      /* This code is inefficient.  We'll improve it later. */
      while ((t = coop_qget (&coop_global_sleepq)) != NULL)
	{
	  if ((t->readfds && FD_ISSET (fd, t->readfds))
	      || (t->writefds && FD_ISSET (fd, t->writefds))
	      || (t->exceptfds && FD_ISSET (fd, t->exceptfds))
	      || (t->timeoutp
		  && (t->wakeup_time.tv_sec < now->tv_sec
		      || (t->wakeup_time.tv_sec == now->tv_sec
			  && t->wakeup_time.tv_usec <= now->tv_usec))))
	    {
	      if (collisionp)
		finalize_fd_sets_lazily (t);
	      else
		finalize_fd_sets (t);
	      coop_qput (&coop_global_runq, t);
	    }
	  else
	    coop_qput(&coop_tmp_queue, t);
	}
      if (collisionp)
	{
	rebuild_global_fd_sets:
	  collisionp = 0;
	  gnfds = 0;
	  FD_ZERO (&greadfds);
	  FD_ZERO (&gwritefds);
	  FD_ZERO (&gexceptfds);
	  while ((t = coop_qget (&coop_tmp_queue)) != NULL)
	    {
	      add_fd_sets (t);
	      coop_qput (&coop_global_sleepq, t);
	    }
	}
      else
	{
	  while ((t = coop_qget (&coop_tmp_queue)) != NULL)
	    coop_qput (&coop_global_sleepq, t);
	}
    }

  return coop_qget (&coop_global_runq);
}

/* Return next runnable thread on the run queue.
 * First update the queue with possible I/O or timeouts.
 * If no thread is found, return NULL.
 */
/* Dirk:Note:: This was externally visible, but only for iselect.c */
static coop_t *
coop_next_runnable_thread ()
{
  coop_t *t;
  struct timeval now;
  int n;

  /* Just return next thread on the runq if the sleepq is empty. */
  if (QEMPTYP (coop_global_sleepq))
    {
      if (QEMPTYP (coop_global_runq))
	return coop_global_curr;
      else
	return coop_qget (&coop_global_runq);
    }

  if (gnfds > 0)
    n = safe_select (gnfds, &greadfds, &gwritefds, &gexceptfds, &timeout0);
  else
    n = 0;
  if (QFIRST (coop_global_sleepq)->timeoutp)
    {
      gettimeofday (&now, NULL);
      t = find_thread (n, &now, 0);
    }
  else
    t = find_thread (n, 0, 0);
  return t == NULL ? coop_global_curr : t;
}

/* Dirk:Note:: This was externally visible, but only for iselect.c */
static coop_t *
coop_wait_for_runnable_thread_now (struct timeval *now)
{
  int n;
  coop_t *t;

  if (gnfds > 0)
    n = safe_select (gnfds, &greadfds, &gwritefds, &gexceptfds, &timeout0);
  else
    n = 0;
  /* Is there any other runnable thread? */
  t = find_thread (n, now, 1);
  while (t == NULL)
    {
      /* No.  Let the process go to sleep. */
      if ((t = QFIRST (coop_global_sleepq))->timeoutp)
	{
	  now->tv_sec = t->wakeup_time.tv_sec - now->tv_sec;
	  if (now->tv_usec > t->wakeup_time.tv_usec)
	    {
	      --now->tv_sec;
	      now->tv_usec = 1000000 + t->wakeup_time.tv_usec - now->tv_usec;
	    }
	  else
	    now->tv_usec = t->wakeup_time.tv_usec - now->tv_usec;
	  n = safe_select (gnfds, &greadfds, &gwritefds, &gexceptfds, now);
	}
      else
	n = safe_select (gnfds, &greadfds, &gwritefds, &gexceptfds, NULL);
      gettimeofday (now, NULL);
      t = find_thread (n, now, 1);
    }

  return t;
}

/* Dirk:Note:: This was externally visible, but only for iselect.c */
static coop_t *
coop_wait_for_runnable_thread ()
{
  struct timeval now;

  if (QEMPTYP (coop_global_sleepq))
    {
      if (QEMPTYP (coop_global_runq))
	return coop_global_curr;
      else
	return coop_qget (&coop_global_runq);
    }

  if (QFIRST (coop_global_sleepq)->timeoutp)
    gettimeofday (&now, NULL);
  
  return coop_wait_for_runnable_thread_now (&now);
}

/* Initialize bit counting array */
static void init_bc (int bit, int i, int n)
{
  if (bit == 0)
    bc[i] = n;
  else
    {
      init_bc (bit >> 1, i, n);
      init_bc (bit >> 1, i | bit, n + 1);
    }
}

static void
init_iselect ()
{
#if 0 /* This is just symbolic */
  collisionp = 0;
  gnfds = 0;
  FD_ZERO (&greadfds);
  FD_ZERO (&gwritefds);
  FD_ZERO (&gexceptfds);
  timeout0.tv_sec = 0;
  timeout0.tv_usec = 0;
#endif
  init_bc (0x80, 0, 0);
#include "libguile/iselect.x"
}


int
coop_select (int nfds,
	     SELECT_TYPE *readfds,
	     SELECT_TYPE *writefds,
	     SELECT_TYPE *exceptfds,
	     struct timeval *timeout)
{
  struct timeval now;
  coop_t *t, *curr = coop_global_curr;

  /* If the timeout is 0, we're polling and can handle it quickly. */
  if (timeout != NULL
      && timeout->tv_sec == 0
      && timeout->tv_usec == 0)
    return select (nfds, readfds, writefds, exceptfds, timeout);

  /* Dirk:Note:: Removed call to SCM_DEFER_INTS here. */

  /* Add our file descriptor flags to the common set. */
  curr->nfds = nfds;
  curr->readfds = readfds;
  curr->writefds = writefds;
  curr->exceptfds = exceptfds;
  add_fd_sets (curr);

  /* Place ourselves on the sleep queue and get a new thread to run. */
  if (timeout == NULL)
    {
      curr->timeoutp = 0;
      coop_qput (&coop_global_sleepq, curr);
      t = coop_wait_for_runnable_thread ();
    }
  else
    {
      gettimeofday (&now, NULL);
      curr->timeoutp = 1;
      curr->wakeup_time.tv_sec = now.tv_sec + timeout->tv_sec;
      curr->wakeup_time.tv_usec = now.tv_usec + timeout->tv_usec;
      if (curr->wakeup_time.tv_usec >= 1000000)
	{
	  ++curr->wakeup_time.tv_sec;
	  curr->wakeup_time.tv_usec -= 1000000;
	}
      /* Insert the current thread at the right place in the sleep queue */
      coop_timeout_qinsert (&coop_global_sleepq, curr);
      t = coop_wait_for_runnable_thread_now (&now);
    }

  /* If the new thread is the same as the sleeping thread, do nothing */
  if (t != coop_global_curr)
    {
      /* Do a context switch. */
      coop_global_curr = t;
      QT_BLOCK (coop_sleephelp, curr, NULL, t->sp);
    }

  if (coop_global_curr->retval == -1)
    errno = coop_global_curr->_errno;

  /* Dirk:Note:: Removed call to SCM_ALLOW_INTS and SCM_ASYNC_TICK here */

  return coop_global_curr->retval;
}


/*****************************************************************************
Here ends the content of iselect.c:
*****************************************************************************/


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
