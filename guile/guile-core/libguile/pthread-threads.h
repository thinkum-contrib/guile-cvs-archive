/* classes: h_files */

#ifndef SCM_THREADS_PTHREADS_H
#define SCM_THREADS_PTHREADS_H

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



/* The pthreads-threads implementation.  This is a direct mapping.
*/

/* This is an interface between Guile and the pthreads thread package. */

#include <pthread.h>
#include <sched.h>

/* MDJ 021209 <djurfeldt@nada.kth.se>:
   The separation of the plugin interface and the low-level C API
   (currently in threads.h) needs to be completed in a sensible way.
 */

/* The scm_t_ types are temporarily used both in plugin and low-level API */
#define scm_t_thread			pthread_t

#define scm_i_plugin_thread_create	pthread_create

#define scm_i_plugin_thread_join	pthread_join 
#define scm_i_plugin_thread_detach	pthread_detach 
#define scm_i_plugin_thread_self	pthread_self
#define scm_i_plugin_thread_yield	sched_yield

/* Size is checked in scm_init_pthread_threads */
#ifdef SCM_DEBUG_THREADS
#define SCM_MUTEX_MAXSIZE (9 * sizeof (long))
#else
#define SCM_MUTEX_MAXSIZE (6 * sizeof (long))
#endif
typedef struct { char _[SCM_MUTEX_MAXSIZE]; } scm_t_mutex;
#define scm_t_mutexattr			pthread_mutexattr_t

extern scm_t_mutexattr scm_i_plugin_mutex; /* The "fast" mutex. */

/* This debug stuff made things a bit messy.  This needs some
   reorganization. */
#ifdef SCM_DEBUG_THREADS
int scm_i_plugin_mutex_init (scm_t_mutex *, const scm_t_mutexattr *);
int scm_i_plugin_mutex_lock (scm_t_mutex *);
int scm_i_plugin_mutex_unlock (scm_t_mutex *);
#else
#define scm_i_plugin_mutex_init(m,a) \
  pthread_mutex_init ((pthread_mutex_t *) (m), (a))
#define scm_i_plugin_mutex_lock(m) \
  pthread_mutex_lock ((pthread_mutex_t *) (m))
#define scm_i_plugin_mutex_unlock(m) \
  pthread_mutex_unlock ((pthread_mutex_t *) (m))
#endif
#define scm_i_plugin_mutex_destroy(m) \
  pthread_mutex_destroy ((pthread_mutex_t *) (m))
#define scm_i_plugin_mutex_trylock(m) \
  pthread_mutex_trylock ((pthread_mutex_t *) (m))

/* Size is checked in scm_init_pthread_threads */
#ifdef SCM_DEBUG_THREADS
#define SCM_REC_MUTEX_MAXSIZE (SCM_MUTEX_MAXSIZE + 3 * sizeof (long))
#else
#ifdef SCM_MUTEX_RECURSIVE
#define SCM_REC_MUTEX_MAXSIZE SCM_MUTEX_MAXSIZE
#else
#define SCM_REC_MUTEX_MAXSIZE (SCM_MUTEX_MAXSIZE + 2 * sizeof (long))
#endif
#endif
typedef struct { char _[SCM_REC_MUTEX_MAXSIZE]; } scm_t_rec_mutex;

extern scm_t_mutexattr scm_i_plugin_rec_mutex;

#if defined (SCM_MUTEX_RECURSIVE) && !defined (SCM_DEBUG_THREADS)
/* pthreads has recursive mutexes! */
#define scm_i_plugin_rec_mutex_init(m,a) \
  pthread_mutex_init ((pthread_mutex_t *) (m), (a))
#define scm_i_plugin_rec_mutex_destroy(m) \
  pthread_mutex_destroy ((pthread_mutex_t *) (m))
#define scm_i_plugin_rec_mutex_lock(m) \
  pthread_mutex_lock ((pthread_mutex_t *) (m))
#define scm_i_plugin_rec_mutex_trylock(m) \
  pthread_mutex_trylock ((pthread_mutex_t *) (m))
#define scm_i_plugin_rec_mutex_unlock(m) \
  pthread_mutex_unlock ((pthread_mutex_t *) (m))
#else
int scm_i_plugin_rec_mutex_init	(scm_t_rec_mutex *, const scm_t_mutexattr *);
#define scm_i_plugin_rec_mutex_destroy(mx) do { (void) (mx); } while (0)
int scm_i_plugin_rec_mutex_lock (scm_t_rec_mutex *);
int scm_i_plugin_rec_mutex_trylock (scm_t_rec_mutex *);
int scm_i_plugin_rec_mutex_unlock (scm_t_rec_mutex *);
#endif

#define scm_t_cond			pthread_cond_t

#define scm_i_plugin_cond_init		pthread_cond_init 
#define scm_i_plugin_cond_destroy	pthread_cond_destroy
#ifdef SCM_DEBUG_THREADS
int scm_i_plugin_cond_wait (scm_t_cond *, scm_t_mutex *);
int scm_i_plugin_cond_timedwait (scm_t_cond *,
				 scm_t_mutex *,
				 const struct timespec *);
#else
#define scm_i_plugin_cond_wait(c, m) \
  pthread_cond_wait ((c), (pthread_mutex_t *) (m))
#define scm_i_plugin_cond_timedwait(c, m, t) \
  pthread_cond_timedwait ((c), (pthread_mutex_t *) (m), (t))
#endif
#define scm_i_plugin_cond_signal	pthread_cond_signal 
#define scm_i_plugin_cond_broadcast	pthread_cond_broadcast 

#define scm_t_key			pthread_key_t

#define scm_i_plugin_key_create		pthread_key_create 
#define scm_i_plugin_key_delete		pthread_key_delete 
#define scm_i_plugin_setspecific	pthread_setspecific 
#define scm_i_plugin_getspecific	pthread_getspecific 

#define scm_i_plugin_select		select

void scm_init_pthread_threads (void);

#endif  /* SCM_THREADS_PTHREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
