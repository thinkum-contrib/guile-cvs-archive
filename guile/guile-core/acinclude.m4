dnl acinclude.m4 for guile

dnl  	Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

dnl This file is part of GUILE.
dnl
dnl GUILE is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as
dnl published by the Free Software Foundation; either version 2, or
dnl (at your option) any later version.
dnl
dnl GUILE is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public
dnl License along with GUILE; see the file COPYING.  If not, write
dnl to the Free Software Foundation, Inc., 59 Temple Place, Suite
dnl 330, Boston, MA 02111-1307 USA

dnl  On the NeXT, #including <utime.h> doesn't give you a definition for
dnl  struct utime, unless you #define _POSIX_SOURCE.

AC_DEFUN(GUILE_STRUCT_UTIMBUF, [
  AC_CACHE_CHECK([whether we need POSIX to get struct utimbuf],
    guile_cv_struct_utimbuf_needs_posix,
    [AC_TRY_CPP([
#ifdef __EMX__
#include <sys/utime.h>
#else
#include <utime.h>
#endif
struct utime blah;
],
                guile_cv_struct_utimbuf_needs_posix=no,
		guile_cv_struct_utimbuf_needs_posix=yes)])
  if test "$guile_cv_struct_utimbuf_needs_posix" = yes; then
     AC_DEFINE(UTIMBUF_NEEDS_POSIX)
  fi])




dnl
dnl Apparently, at CMU they have a weird version of libc.h that is
dnl installed in /usr/local/include and conflicts with unistd.h.
dnl In these situations, we should not #include libc.h.
dnl This test arranges to #define LIBC_H_WITH_UNISTD_H iff libc.h is
dnl present on the system, and is safe to #include.
dnl
AC_DEFUN([GUILE_HEADER_LIBC_WITH_UNISTD],
  [
    AC_CHECK_HEADERS(libc.h unistd.h)
    AC_CACHE_CHECK(
      [whether libc.h and unistd.h can be included together],
      guile_cv_header_libc_with_unistd,
      [
        if test "$ac_cv_header_libc_h" = "no"; then
          guile_cv_header_libc_with_unistd="no"
        elif test "$ac_cv_header_unistd_h" = "no"; then
          guile_cv_header_libc_with_unistd="yes"
        else
          AC_TRY_COMPILE(
	    [
#             include <libc.h>
#             include <unistd.h>
	    ],
	    [],
	    [guile_cv_header_libc_with_unistd=yes],
	    [guile_cv_header_libc_with_unistd=no]
          )
        fi
      ]
    )
    if test "$guile_cv_header_libc_with_unistd" = yes; then
      AC_DEFINE(LIBC_H_WITH_UNISTD_H)
    fi
  ]
)



dnl This is needed when we want to check for the same function repeatedly
dnl with other parameters, such as libraries, varying.
dnl
dnl GUILE_NAMED_CHECK_FUNC(FUNCTION, TESTNAME,
dnl                        [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
AC_DEFUN(GUILE_NAMED_CHECK_FUNC,
[AC_MSG_CHECKING([for $1])
AC_CACHE_VAL(ac_cv_func_$1_$2,
[AC_TRY_LINK(
dnl Don't include <ctype.h> because on OSF/1 3.0 it includes <sys/types.h>
dnl which includes <sys/select.h> which contains a prototype for
dnl select.  Similarly for bzero.
[/* System header to define __stub macros and hopefully few prototypes,
    which can conflict with char $1(); below.  */
#include <assert.h>
/* Override any gcc2 internal prototype to avoid an error.  */
#ifdef __cplusplus
extern "C"
#endif
/* We use char because int might match the return type of a gcc2
    builtin and then its argument prototype would still apply.  */
char $1();
], [
/* The GNU C library defines this for functions which it implements
    to always fail with ENOSYS.  Some functions are actually named
    something starting with __ and the normal name is an alias.  */
#if defined (__stub_$1) || defined (__stub___$1)
choke me
#else
$1();
#endif
], eval "ac_cv_func_$1_$2=yes", eval "ac_cv_func_$1_$2=no")])
if eval "test \"`echo '$ac_cv_func_'$1'_'$2`\" = yes"; then
  AC_MSG_RESULT(yes)
  ifelse([$3], , :, [$3])
else
  AC_MSG_RESULT(no)
ifelse([$4], , , [$4
])dnl
fi
])
