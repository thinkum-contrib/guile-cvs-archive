/* classes: h_files */

#ifndef SCM_STIME_H
#define SCM_STIME_H

/* Copyright (C) 1995,1996,1997,1998,2000, 2003 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"



# ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   ifdef HAVE_TIME_H
#    include <time.h>
#   endif
#  endif
# endif

/* This should be figured out by autoconf.  */
#if ! defined(SCM_TIME_UNITS_PER_SECOND) && defined(CLK_TCK)
#  define SCM_TIME_UNITS_PER_SECOND ((int) CLK_TCK)
#endif
#if ! defined(SCM_TIME_UNITS_PER_SECOND) && defined(CLOCKS_PER_SEC)
#  define SCM_TIME_UNITS_PER_SECOND ((int) CLOCKS_PER_SEC)
#endif
#if ! defined(SCM_TIME_UNITS_PER_SECOND)
#  define SCM_TIME_UNITS_PER_SECOND 60
#endif


SCM_API long scm_c_get_internal_run_time (void);
SCM_API SCM scm_get_internal_real_time (void);
SCM_API SCM scm_get_internal_run_time (void);
SCM_API SCM scm_current_time (void);
SCM_API SCM scm_gettimeofday (void);
SCM_API SCM scm_localtime (SCM time, SCM zone);
SCM_API SCM scm_gmtime (SCM time);
SCM_API SCM scm_mktime (SCM sbd_time, SCM zone);
SCM_API SCM scm_tzset (void);
SCM_API SCM scm_times (void);
SCM_API SCM scm_strftime (SCM format, SCM stime);
SCM_API SCM scm_strptime (SCM format, SCM string);
SCM_API void scm_init_stime (void);

#endif  /* SCM_STIME_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
