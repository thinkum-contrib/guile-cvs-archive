/* classes: h_files */

#ifndef SCM_NULL_THREADS_H
#define SCM_NULL_THREADS_H

/* Copyright (C) 2005 Free Software Foundation, Inc.
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



/* The null-threads implementation.  We provide the subset of the
   standard pthread API that is used by Guile, but no new threads can
   be created.

   This file merely exits so that Guile can be compiled and run
   without using pthreads.  Improving performance via optimizations
   that are possible in a single-threaded program is not a primary
   goal.
*/


#endif  /* SCM_NULL_THREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
