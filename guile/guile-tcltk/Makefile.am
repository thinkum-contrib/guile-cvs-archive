## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libgtcltk.la
bin_PROGRAMS = guile-tcltk

libgtcltk_la_SOURCES = \
	guile-tcl.c guile-tk.c guile-tcl.h guile-tk.h gtcltk-module.c
EXTRA_libgtcltk_la_SOURCES = tclInt.h tclUnixPort.h tclUnixNotfy.c
BUILT_SOURCES = guile-tcl.x guile-tk.x
libgtcltk_la_DEPENDENCIES = @LIBLOBJS@
libgtcltk_la_LIBADD = @LIBLOBJS@
libgtcltk_la_LDFLAGS = -version-info 1:0

guile_tcltk_LDADD = libgtcltk.la

modincludedir = $(includedir)/guile-tcltk
modinclude_HEADERS = guile-tcl.h guile-tk.h

subpkgdatadir = @TCLTKDIR@
subpkgdata_DATA = tcltk.scm hand.scm graph.scm breakout.scm

EXTRA_DIST = license.terms $(subpkgdata_DATA)

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS) $< > $@

COMPILE = $(CC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)
