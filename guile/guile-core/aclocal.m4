dnl aclocal.m4 generated automatically by aclocal 1.1l

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version)

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AM_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE")
VERSION=[$2]
AC_SUBST(VERSION)
AC_DEFINE_UNQUOTED(VERSION, "$VERSION")
AM_SANITY_CHECK
AC_ARG_PROGRAM
AC_PROG_MAKE_SET])


# serial 1

AC_DEFUN(AM_PROG_INSTALL,
[AC_REQUIRE([AC_PROG_INSTALL])
test -z "$INSTALL_SCRIPT" && INSTALL_SCRIPT='${INSTALL_PROGRAM}'
AC_SUBST(INSTALL_SCRIPT)dnl
])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
[AC_MSG_CHECKING([whether build environment is sane])
echo timestamp > conftestfile
# Do this in a subshell so we don't clobber the current shell's
# arguments.  FIXME: maybe try `-L' hack like GETLOADAVG test?
if (set X `ls -t $srcdir/configure conftestfile`; test "[$]2" = conftestfile)
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

# Add --enable-maintainer-mode option to configure.
# From Jim Meyering

# serial 1

AC_DEFUN(AM_MAINTAINER_MODE,
[AC_MSG_CHECKING([whether to enable maintainer-specific portions of Makefiles])
  dnl maintainer-mode is disabled by default
  AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode enable make rules and dependencies not useful
                          (and sometimes confusing) to the casual installer],
      USE_MAINTAINER_MODE=$enableval,
      USE_MAINTAINER_MODE=no)
  AC_MSG_RESULT($USE_MAINTAINER_MODE)
  if test $USE_MAINTAINER_MODE = yes; then
    MAINT=
  else
    MAINT='#M#'
  fi
  AC_SUBST(MAINT)dnl
]
)

