/* Component of libguilegtkthreads.a
 * Copyright (C) 2000 Free Software Foundation
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

extern SCM scm_gtkthreads_update (void);

#ifndef HAVE_SGTK_GDK_THREADS_ENTER
extern SCM sgtk_gdk_threads_enter (void);
extern SCM sgtk_gdk_threads_leave (void);
#endif

extern void scm_init_gtkthreads (void);
extern void scm_init_gthread (void);
extern void scm_init_gtk_guilegtkthreads_module (void);
