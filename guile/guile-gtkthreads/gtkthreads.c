/* Component of libguilegtkthreads.a
 * Copyright (C) 2000, 2001 Free Software Foundation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* Define the following option to include the primitive
 * `gtkthreads-update' which makes the main loop wake up.
 */

/* #define GUILE_GTKTHREADS_UPDATE */

#include <libguile.h>
#include <glib.h>
#ifndef HAVE_SGTK_GDK_THREADS_ENTER
#include <gdk/gdk.h>
#endif

#include <unistd.h>

#include "gtkthreads.h"

extern int errno;

#ifdef FD_SET
#  define SELECT_MASK fd_set
#else /* !NO_FD_SET */
#  ifdef _IBMR2 /*fixme* not defined*/
#    define SELECT_MASK void
#  else /* !_IBMR2 */
#    define SELECT_MASK int
#  endif /* !_IBMR2 */
#endif /* !NO_FD_SET */

#ifdef GUILE_GTKTHREADS_UPDATE
static int poll_waiting = 0;
static int wake_up_pipe[2] = { -1, -1 };
static GPollFD wake_up_rec;
#endif

static gint 
g_poll (GPollFD *fds,
	guint    nfds,
	gint     timeout)
{
  struct timeval tv;
  SELECT_MASK rset, wset, xset;
  GPollFD *f;
  int ready;
  int maxfd = 0;

  FD_ZERO (&rset);
  FD_ZERO (&wset);
  FD_ZERO (&xset);

  for (f = fds; f < &fds[nfds]; ++f)
    if (f->fd >= 0)
      {
	if (f->events & G_IO_IN)
	  FD_SET (f->fd, &rset);
	if (f->events & G_IO_OUT)
	  FD_SET (f->fd, &wset);
	if (f->events & G_IO_PRI)
	  FD_SET (f->fd, &xset);
	if (f->fd > maxfd && (f->events & (G_IO_IN|G_IO_OUT|G_IO_PRI)))
	  maxfd = f->fd;
      }

  tv.tv_sec = timeout / 1000;
  tv.tv_usec = (timeout % 1000) * 1000;

#ifdef GUILE_GTKTHREADS_UPDATE
  poll_waiting = TRUE;
#endif
  
  ready = scm_internal_select (maxfd + 1, &rset, &wset, &xset,
		               timeout == -1 ? NULL : &tv);

#ifdef GUILE_GTKTHREADS_UPDATE
  if (!poll_waiting)
    {
#ifndef NATIVE_WIN32
      gchar c;
      read (wake_up_pipe[0], &c, 1);
#endif
    }
  else
    poll_waiting = FALSE;
#endif

  if (ready > 0)
    for (f = fds; f < &fds[nfds]; ++f)
      {
	f->revents = 0;
	if (f->fd >= 0)
	  {
	    if (FD_ISSET (f->fd, &rset))
	      f->revents |= G_IO_IN;
	    if (FD_ISSET (f->fd, &wset))
	      f->revents |= G_IO_OUT;
	    if (FD_ISSET (f->fd, &xset))
	      f->revents |= G_IO_PRI;
	  }
      }

  return ready;
}

#ifdef GUILE_GTKTHREADS_UPDATE
/* Wake the main loop up from a poll() */
static void
g_main_wakeup (void)
{
  if (poll_waiting)
    {
      poll_waiting = FALSE;
#ifndef NATIVE_WIN32
      write (wake_up_pipe[1], "A", 1);
#else
      ReleaseSemaphore (wake_up_semaphore, 1, NULL);
#endif
    }
}

SCM_PROC (s_gtkthreads_update, "gtkthreads-update", 0, 0, 0, scm_gtkthreads_update);

SCM
scm_gtkthreads_update ()
{
  g_main_wakeup ();
  return SCM_UNSPECIFIED;
}

#endif /* GUILE_GTKTHREADS_UPDATE */

#ifndef HAVE_SGTK_GDK_THREADS_ENTER

SCM_PROC (s_gdk_threads_enter, "gdk-threads-enter", 0, 0, 0, sgtk_gdk_threads_enter);

SCM
sgtk_gdk_threads_enter ()
{
  gdk_threads_enter ();
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_gdk_threads_leave, "gdk-threads-leave", 0, 0, 0, sgtk_gdk_threads_leave);

SCM
sgtk_gdk_threads_leave ()
{
  gdk_threads_leave ();
  return SCM_UNSPECIFIED;
}

#endif /* HAVE_SGTK_GDK_THREADS_ENTER */

#ifndef HAVE_SCM_SET_CURRENT_MODULE
#define scm_set_current_module scm_select_module
#endif

void
scm_init_gtkthreads ()
{
  SCM threads_module = scm_make_module (scm_read_0str ("(gtk threads)"));
  SCM old_module = scm_set_current_module (threads_module);
  scm_init_gthread ();
  g_main_set_poll_func (g_poll);
#ifdef GUILE_GTKTHREADS_UPDATE
  if (pipe (wake_up_pipe) < 0)
    g_error ("Cannot create pipe main loop wake-up: %s\n",
	     g_strerror (errno));

  wake_up_rec.fd = wake_up_pipe[0];
  wake_up_rec.events = G_IO_IN;
  g_main_add_poll (&wake_up_rec, 0);
#endif
#include "gtkthreads.x"
  scm_set_current_module (old_module);
}

void
scm_init_gtk_guilegtkthreads_module ()
{
  scm_register_module_xxx ("gtk guilegtkthreads", (void *) scm_init_gtkthreads);
}
