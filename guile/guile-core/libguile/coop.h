/* classes: h_files */

#ifndef COOPH
#define COOPH

/*	Copyright (C) 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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


/*****************************************************************************
Here starts the content of iselect.h:
*****************************************************************************/


/* Needed for FD_SET on some systems.  */
#include <sys/types.h>


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

extern int scm_internal_select (int fds,
				SELECT_TYPE *rfds,
				SELECT_TYPE *wfds,
				SELECT_TYPE *efds,
				struct timeval *timeout);

#ifdef GUILE_ISELECT

extern int scm_I_am_dead;

#endif /* GUILE_ISELECT */


/*****************************************************************************
Here ends the content of iselect.h:
*****************************************************************************/


#ifdef GUILE_PTHREAD_COMPAT
#include <pthread.h>
#endif

/* The notion of a thread is merged with the notion of a queue.
   Thread stuff: thread status (sp) and stuff to use during
   (re)initialization.  Queue stuff: next thread in the queue
   (next). */

struct qt_t;

typedef struct coop_t {
  struct qt_t *sp;       /* QuickThreads handle. */
  void *sto;             /* `malloc'-allocated stack. */

  struct coop_t *next;    /* Next thread in the queue. */

  struct coop_t *all_next;    
  struct coop_t *all_prev;    

  void *data;            /* Thread local data */
  void **specific;	 /* Data associated with keys */
  int n_keys;		 /* Upper limit for keys on this thread */
  
  void *base;            /* Base of stack */
  void *top;             /* Top of stack */

  void *joining;         /* A queue of threads waiting to join this
			    thread */

#ifdef GUILE_ISELECT
  int nfds;
  SELECT_TYPE *readfds;
  SELECT_TYPE *writefds;
  SELECT_TYPE *exceptfds;
  int timeoutp;
  struct timeval wakeup_time;	/* Time to stop sleeping */
  int _errno;
  int retval;
#else
  time_t wakeup_time;    /* Time to stop sleeping */
#endif

#ifdef GUILE_PTHREAD_COMPAT
  pthread_t dummy_thread;
  pthread_mutex_t dummy_mutex;
#endif
} coop_t;

extern coop_t coop_global_main;        /* Thread for the process. */  /* FIXME: needed? */


/* A queue is a circular list of threads.  The queue head is a
   designated list element.  If this is a uniprocessor-only
   implementation we can store the `main' thread in this, but in a
   multiprocessor there are several `heavy' threads but only one run
   queue.  A fancier implementation might have private run queues,
   which would lead to a simpler (trivial) implementation */

typedef struct coop_q_t {
  coop_t t;
  coop_t *tail;
} coop_q_t;

/* A Mutex variable is made up of a owner thread, and a queue of threads
   waiting on the mutex */

typedef struct coop_m {
  coop_t *owner;          /* Mutex owner */
  coop_q_t waiting;      /* Queue of waiting threads */
} coop_m;

typedef int coop_mattr;

extern int coop_mutex_init (coop_m*);
extern int coop_new_mutex_init (coop_m*, coop_mattr*);
extern int coop_mutex_lock (coop_m*);
extern int coop_mutex_trylock (coop_m*);
extern int coop_mutex_unlock (coop_m*);
extern int coop_mutex_destroy (coop_m*);

/* A Condition variable is made up of a list of threads waiting on the
   condition. */

typedef struct coop_c {
  coop_q_t waiting;      /* Queue of waiting threads */
} coop_c;

typedef int coop_cattr;

#ifndef HAVE_STRUCT_TIMESPEC
/* POSIX.4 structure for a time value.  This is like a `struct timeval' but
   has nanoseconds instead of microseconds.  */
struct timespec
{
  long int tv_sec;		/* Seconds.  */
  long int tv_nsec;		/* Nanoseconds.  */
};
#endif

extern int coop_condition_variable_init (coop_c*);
extern int coop_new_condition_variable_init (coop_c*, coop_cattr*);
extern int coop_condition_variable_wait_mutex (coop_c*, coop_m*);
extern int coop_condition_variable_timed_wait_mutex (coop_c*,
						     coop_m*,
						     const struct timespec *abstime);
extern int coop_condition_variable_signal (coop_c*);
extern int coop_condition_variable_destroy (coop_c*);

typedef int coop_k;

extern int coop_key_create (coop_k *keyp, void (*destructor) (void *value));
extern int coop_setspecific (coop_k key, const void *value);
extern void *coop_getspecific (coop_k key);
extern int coop_key_delete (coop_k);

extern void coop_join (coop_t *t);
extern void coop_yield (void);



/* Each thread starts by calling a user-supplied function of this
   type. */

typedef void (coop_userf_t)(void *p0);

/* Call this before any other primitives. */
extern void coop_init (void);

/* When one or more threads are created by the main thread,
   the system goes multithread when this is called.  It is done
   (no more runable threads) when this returns. */

extern void coop_start (void);

/* Create a thread and make it runable.  When the thread starts
   running it will call `f' with arguments `p0' and `p1'. */

extern coop_t *coop_create (coop_userf_t *f, void *p0);

/* The current thread stops running but stays runable.
   It is an error to call `coop_yield' before `coop_start'
   is called or after `coop_start' returns. */

extern void coop_yield (void);
extern int coop_select
  (int nfds, SELECT_TYPE *readfds, SELECT_TYPE *writefds, SELECT_TYPE *exceptfds, struct timeval *timeout);

/* Like `coop_yield' but the thread is discarded.  Any intermediate
   state is lost.  The thread can also terminate by simply
   returning. */

extern void coop_abort (void);

/* Dirk:Note:: The following are needed(?) in guile-coop.c */

extern coop_q_t coop_global_runq;	/* A queue of runable threads. */
extern coop_q_t coop_global_allq;	/* A queue of all threads. */
extern coop_t *coop_global_curr;       	/* Currently-executing thread. */

extern void coop_all_qput (coop_q_t *q, coop_t *t);


#endif /* COOPH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
