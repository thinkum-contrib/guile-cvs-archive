/* Copyright (C) 2002 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */




#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/scmconfig.h"

/* Should go to threads-plugin */
scm_t_mutexattr scm_i_plugin_mutex;

scm_t_mutexattr scm_i_plugin_rec_mutex;

#ifndef SCM_MUTEXATTR_SETTYPE_DECLARED
int pthread_mutexattr_settype (pthread_mutexattr_t *, int);
#endif

void
scm_init_pthread_threads ()
{
  pthread_mutexattr_init (&scm_i_plugin_mutex);
#ifdef SCM_MUTEX_FAST
  pthread_mutexattr_settype (&scm_i_plugin_mutex, SCM_MUTEX_FAST);
#endif
  /* These values should be passed in a structure. */
  scm_i_plugin_mutex_size = sizeof (pthread_mutex_t);
  scm_i_plugin_mutex_init = (scm_t_mutex_init) pthread_mutex_init;
  scm_i_plugin_mutex_lock = (scm_t_mutex_lock) pthread_mutex_lock;
  scm_i_plugin_mutex_unlock = (scm_t_mutex_unlock) pthread_mutex_unlock;

#if defined (SCM_MUTEX_RECURSIVE) && !defined (SCM_DEBUG_THREADS)
  pthread_mutexattr_init (&scm_i_plugin_rec_mutex);
  pthread_mutexattr_settype (&scm_i_plugin_rec_mutex, SCM_MUTEX_RECURSIVE);
  scm_i_plugin_rec_mutex_size = sizeof (pthread_mutex_t);
  scm_i_plugin_rec_mutex_init = (scm_t_rec_mutex_init) pthread_mutex_init;
  scm_i_plugin_rec_mutex_destroy = (scm_t_rec_mutex_destroy) pthread_mutex_destroy;
  scm_i_plugin_rec_mutex_lock = (scm_t_rec_mutex_lock) pthread_mutex_lock;
  scm_i_plugin_rec_mutex_trylock = (scm_t_rec_mutex_trylock) pthread_mutex_trylock;
  scm_i_plugin_rec_mutex_unlock = (scm_t_rec_mutex_unlock) pthread_mutex_unlock;
#endif

  scm_i_plugin_cond_wait = (scm_t_cond_wait) pthread_cond_wait;
  scm_i_plugin_cond_timedwait = (scm_t_cond_timedwait) pthread_cond_timedwait;

  scm_init_threads_plugin ();
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
